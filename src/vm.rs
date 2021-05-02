use function::Type;

use crate::itemise::ItemPath;
use std::collections::HashMap;
use std::collections::binary_heap::Iter;
use std::sync::Arc;
use crate::function::{GetFunctionGraph, InstructionType, JumpType};
use crate::{Error, Result};
use crate::function;
use crate::storage::Storage;

#[derive(Debug)]
pub(crate) enum MoveArg {
    Byte(u8),
    Half(u32),
    Word(u64),
    Stack{
        offset: i64,
        len: u64,
    },
    Constant{
        idx: u64,
        len: u64,
    },
    AddressOfConstant{
        idx: u64,
    },

    InstructionIdx,
    StackIdx,

    ValA,
    ValB,
    AddResult,
    SubResult,
    AddU32Result,
    EqResult,

    DataAddressA,
    DataLenA,
    DataA,

    DataAddressB,
    DataLenB,
    DataB,

    SkipNext,

    FunctionStartIdx(ItemPath),
    ReturnAddressStack,

    ReturnValue,
    ReturnValuePos,
    ReturnValueLen,
    Halt,

    None,
}

fn size(t: &Type) -> usize {
    match t {
        Type::U32 => 4,
        Type::Bool => 1,
        Type::String => 16,
        Type::Never => 0,
        Type::Null => 0,
        Type::Struct(_) => panic!("Struct not supported"),
        Type::Compound(_, _) => panic!("Compound not supported"),
    }
}

fn align(t: &Type) -> usize {
    match t {
        Type::U32 => std::mem::align_of::<u32>(),
        Type::Bool => 1,
        Type::String => 16,
        Type::Never => 1,
        Type::Null => 1,
        Type::Struct(_) => panic!("Struct not supported align"),
        Type::Compound(_, _) => panic!("Compound not supported align"),
    }
}

fn align_up_to(x: usize, align: usize) -> usize {
    x + ((align - (x % align)) % align)
}

fn offsets<'a, T: Iterator<Item = &'a Type> + 'a>(types: T, initial: usize) -> impl Iterator<Item = usize> + 'a {
    let mut total = initial;
    types.map(move |t| {
        total = align_up_to(total, align(t));
        let ret = total;
        total += size(t);
        ret
    })
}

#[derive(Debug)]
pub(crate) struct Instruction {
    pub(crate) src: MoveArg,
    pub(crate) dst: MoveArg,
}

#[derive(Debug)]
pub(crate) struct Program {
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) constants: Vec<u8>,
    pub(crate) functions: HashMap<ItemPath, u64>,
}

impl Program {
    pub(crate) fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            functions: HashMap::new(),
        }
    }
}

pub(crate) struct Vm {
    stack: Vec<u8>,
    instruction_idx: u64,
    stack_idx: u64,

    val_a: u64,
    val_b: u64,

    data_address_a: u64,
    data_len_a: u64,

    data_address_b: u64,
    data_len_b: u64,

    skip_next: u64,

    return_value_stack_pos: Vec<u64>,
    return_value_stack_len: Vec<u64>,
    program_return: u64,
    return_address_stack: Vec<u64>,
}

impl std::fmt::Debug for Vm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        write!(f, r#"Vm {{
    stack: {:?},
    instruction_idx: {:?},
    stack_idx: {:?},
    val_a: {:?},
    val_b: {:?},
    data_address_a: {:?},
    data_len_a: {:?},
    data_address_b: {:?},
    data_len_b: {:?},
    skip_next: {:?},
    return_value_stack_pos: {:?},
    return_value_stack_len: {:?},
    program_return: {:?},
    return_address_stack: {:?},
}}
        "#,
        &self.stack[0..32],
        self.instruction_idx,
        self.stack_idx,
        self.val_a,
        self.val_b,
        self.data_address_a,
        self.data_len_a,
        self.data_address_b,
        self.data_len_b,
        self.skip_next,
        self.return_value_stack_pos,
        self.return_value_stack_len,
        self.program_return,
        self.return_address_stack
    )
    }
}

#[derive(Debug)]
enum DataSource {
    Byte(u8),
    Half(u32),
    Word(u64),
    Slice(*const u8, usize),
    None,
}

#[derive(Debug)]
enum DataSink<'a> {
    Register(&'a mut u64),
    Slice(*mut u8, usize),
    Halt,
    None,
}

impl Vm {
    pub(crate) fn new(stack_size: usize) -> Self {
        Self {
            stack: vec![0; stack_size],
            instruction_idx: 0,
            stack_idx: 0,

            val_a: 0,
            val_b: 0,

            data_address_a: 0,
            data_len_a: 0,

            data_address_b: 0,
            data_len_b: 0,

            skip_next: 0,

            return_value_stack_pos: vec![],
            return_value_stack_len: vec![],
            program_return: 0,
            return_address_stack: vec![],
        }
    }

    pub(crate) fn run(&mut self, program: &Program) -> u64 {
        loop {
            if self.skip_next != 0 {
                self.skip_next = 0;
                self.instruction_idx += 1;
                continue;
            }
            
            let inst = &program.instructions[self.instruction_idx as usize];

            let src = match inst.src {
                MoveArg::Byte(value) => DataSource::Byte(value),
                MoveArg::Half(value) => DataSource::Half(value),
                MoveArg::Word(value) => DataSource::Word(value),
                MoveArg::Stack{offset, len} => DataSource::Slice(&self.stack[(self.stack_idx as i64 + offset) as usize], len as usize),
                MoveArg::Constant{idx, len} => DataSource::Slice(&program.constants[idx as usize], len as usize),
                MoveArg::AddressOfConstant{idx} => {
                    let ptr: *const u8 = &program.constants[idx as usize];
                    DataSource::Word(ptr as u64)
                },
                MoveArg::InstructionIdx => DataSource::Word(self.instruction_idx),
                MoveArg::StackIdx => DataSource::Word(self.stack_idx),

                MoveArg::ValA => DataSource::Word(self.val_a),
                MoveArg::ValB => DataSource::Word(self.val_b),
                MoveArg::AddResult => DataSource::Word(self.val_a + self.val_b),
                MoveArg::AddU32Result => DataSource::Half((self.val_a + self.val_b) as u32),
                MoveArg::SubResult => DataSource::Word(self.val_a - self.val_b),
                MoveArg::EqResult => DataSource::Byte((self.val_a == self.val_b) as u8),

                MoveArg::DataAddressA => DataSource::Word(self.data_address_a),
                MoveArg::DataLenA => DataSource::Word(self.data_len_a),
                MoveArg::DataA => DataSource::Slice(self.data_address_a as *const u8, self.data_len_a as usize),

                MoveArg::DataAddressB => DataSource::Word(self.data_address_b),
                MoveArg::DataLenB => DataSource::Word(self.data_len_b),
                MoveArg::DataB => DataSource::Slice(self.data_address_b as *const u8, self.data_len_b as usize),

                MoveArg::SkipNext => DataSource::Word(self.skip_next),

                MoveArg::FunctionStartIdx(ref path) => DataSource::Word(*program.functions.get(path).unwrap()),
                MoveArg::ReturnAddressStack => DataSource::Word(self.return_address_stack.pop().unwrap() + 2),

                MoveArg::ReturnValuePos => {
                    self.return_value_stack_pos.pop();
                    DataSource::None
                },
                MoveArg::ReturnValueLen => {
                    self.return_value_stack_len.pop();
                    DataSource::None
                },
                MoveArg::ReturnValue => {
                    if let (Some(stack_pos), Some(len)) = (self.return_value_stack_pos.last(), self.return_value_stack_len.last()) {
                        DataSource::Slice(&self.stack[*stack_pos as usize], *len as usize)
                    } else {
                        DataSource::Word(self.program_return)
                    }
                },
                MoveArg::Halt => DataSource::Byte(0),
                MoveArg::None => DataSource::None,
            };

            self.instruction_idx += 1;

            let dst = match inst.dst {
                MoveArg::Byte(_) => panic!("Invalid Dest"),
                MoveArg::Half(_) => panic!("Invalid Dest"),
                MoveArg::Word(_) => panic!("Invalid Dest"),
                MoveArg::Constant{..} => panic!("Invalid Dest"),
                MoveArg::AddressOfConstant{..} => panic!("Invalid Dest"),
                MoveArg::FunctionStartIdx(_) => panic!("Invalid Dest"),

                MoveArg::Stack{offset, len} => DataSink::Slice(&mut self.stack[(self.stack_idx as i64 + offset) as usize], len as usize),
                MoveArg::InstructionIdx => DataSink::Register(&mut self.instruction_idx),
                MoveArg::StackIdx => DataSink::Register(&mut self.stack_idx),
                
                MoveArg::ValA => DataSink::Register(&mut self.val_a),
                MoveArg::ValB => DataSink::Register(&mut self.val_b),
                MoveArg::AddResult => panic!("Invalid Dest"),
                MoveArg::AddU32Result => panic!("Invalid Dest"),
                MoveArg::SubResult => panic!("Invalid Dest"),
                MoveArg::EqResult => panic!("Invalid Dest"),

                MoveArg::DataAddressA => DataSink::Register(&mut self.data_address_a),
                MoveArg::DataLenA => DataSink::Register(&mut self.data_len_a),
                MoveArg::DataA => DataSink::Slice(self.data_address_a as *mut u8, self.data_len_a as usize),

                MoveArg::DataAddressB => DataSink::Register(&mut self.data_address_b),
                MoveArg::DataLenB => DataSink::Register(&mut self.data_len_b),
                MoveArg::DataB => DataSink::Slice(self.data_address_b as *mut u8, self.data_len_b as usize),

                MoveArg::SkipNext => DataSink::Register(&mut self.skip_next),

                MoveArg::ReturnAddressStack => {
                    self.return_address_stack.push(0);
                    DataSink::Register(self.return_address_stack.last_mut().unwrap())
                },

                MoveArg::ReturnValuePos => {
                    self.return_value_stack_pos.push(0);
                    DataSink::Register(self.return_value_stack_pos.last_mut().unwrap())
                },
                MoveArg::ReturnValueLen => {
                    self.return_value_stack_len.push(0);
                    DataSink::Register(self.return_value_stack_len.last_mut().unwrap())
                },
                MoveArg::ReturnValue => {
                    if let (Some(stack_pos), Some(len)) = (self.return_value_stack_pos.last(), self.return_value_stack_len.last()) {
                        DataSink::Slice(&mut self.stack[*stack_pos as usize], *len as usize)
                    } else {
                        DataSink::Register(&mut self.program_return)
                    }
                },
                MoveArg::Halt => DataSink::Halt,
                MoveArg::None => DataSink::None,
            };

            match (src, dst) {
                (DataSource::None, _) | (_, DataSink::None) => {},
                (DataSource::Byte(value), DataSink::Register(reg)) => *reg = value as u64,
                (DataSource::Byte(value), DataSink::Slice(data, len)) => unsafe {data.write_bytes(value, len)},

                (DataSource::Half(value), DataSink::Register(reg)) => *reg = value as u64,
                (DataSource::Half(value), DataSink::Slice(data, len)) => unsafe {
                    let u32_data = data.cast::<u32>();
                    for i in 0..len/4 {
                        *u32_data.add(i) = value;
                    }
                },

                (DataSource::Word(value), DataSink::Register(reg)) => *reg = value,
                (DataSource::Word(value), DataSink::Slice(data, len)) => unsafe {
                    let u64_data = data.cast::<u64>();
                    for i in 0..len/8 {
                        *u64_data.add(i) = value;
                    }
                },

                (DataSource::Slice(data, len), DataSink::Register(reg)) => unsafe {
                    assert!(len <= 8);
                    let reg_ptr: *mut u64 = reg;
                    data.copy_to_nonoverlapping(reg_ptr.cast::<u8>(), len);
                },
                (DataSource::Slice(s, len), DataSink::Slice(d, _)) => unsafe {s.copy_to_nonoverlapping(d, len)},

                (_, DataSink::Halt) => return self.program_return,
            }
        }
    }
}



fn vm_program(graph: &function::Graph, prog: &mut Program, halt_on_return: bool) -> Result<Vec<ItemPath>> {
    let mut dependencies = Vec::new();

    let (stack_offsets, stack_size) = {
        let mut args = vec![];
        let mut locals = vec![];

        for handle in graph.objects() {
            let object = graph.object(handle);
            match object.source {
                function::ObjectSource::Argument(idx) => {
                    args.resize(idx + 1, None);
                    args[idx] = Some(handle);
                },
                function::ObjectSource::Local => {
                    locals.push(handle);
                }
            }
        }
    
        let mut stack_offsets = Storage::new();
        let arg_types = args.iter().map(|arg| {
            let handle = arg.expect("Missing arg index");
            graph.object(handle).t.as_ref().unwrap()
        }); 

        let mut total = 0;
        for (arg, offset) in args.iter().zip(offsets(arg_types, 0)) {
            let handle = arg.expect("Missing arg index");
            *stack_offsets.get_mut(&handle) = Some(offset);
            total = offset;
        } 

        let local_types = locals.iter().map(|handle| {
            graph.object(*handle).t.as_ref().unwrap()
        });

        for (handle, offset) in locals.iter().zip(offsets(local_types, total)) {
            *stack_offsets.get_mut(handle) = Some(offset);
            total = offset;
        } 
        
        (stack_offsets, total)
    };

    let move_arg_for = |o: function::ObjectHandle| -> MoveArg {
        MoveArg::Stack{
            offset: *stack_offsets.get(&o).unwrap() as i64,
            len: size(graph.object(o).t.as_ref().unwrap()) as u64
        }
    };

    let move_arg_for_len = |o: function::ObjectHandle, len: usize, offset: usize| -> MoveArg {
        MoveArg::Stack{offset: (offset + stack_offsets.get(&o).unwrap()) as i64, len: len as u64}
    };

    // Make the instructions for each block
    let mut blocks: Vec<Vec<Instruction>> = Vec::new();
    blocks.reserve(graph.num_blocks());

    let mut block_offsets = Storage::new();
    block_offsets.reserve(graph.num_blocks());
    let mut last_offset = prog.instructions.len();

    for block in graph.blocks() {
        let mut instructions = Vec::new();
        for instruction in &graph.block(block).instructions {
            match instruction.t {
                InstructionType::Eq{dest, lhs, rhs} => {
                    instructions.push(Instruction{
                        src: move_arg_for(lhs),
                        dst: MoveArg::ValA,
                    });

                    instructions.push(Instruction{
                        src: move_arg_for(rhs),
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::EqResult,
                        dst: move_arg_for(dest),
                    });
                },
                InstructionType::StoreU32(dest, value) => {
                    instructions.push(Instruction{
                        src: MoveArg::Half(value),
                        dst: move_arg_for(dest),
                    });
                },
                InstructionType::StoreString(ref dest, ref val) => {
                    let const_idx = prog.constants.len();
                    prog.constants.extend(val.bytes());
                    instructions.push(Instruction{
                        src: MoveArg::AddressOfConstant{idx: const_idx as u64},
                        dst: move_arg_for_len(*dest, 8, 0),
                    });

                    instructions.push(Instruction{
                        src: MoveArg::Word(val.len() as u64),
                        dst: move_arg_for_len(*dest, 8, 8),
                    });
                },
                InstructionType::StoreNull(_) => {},
                InstructionType::SetType(_, _) => {},
                InstructionType::Add{dest, lhs, rhs} => {
                    instructions.push(Instruction{
                        src: move_arg_for(lhs),
                        dst: MoveArg::ValA,
                    });

                    instructions.push(Instruction{
                        src: move_arg_for(rhs),
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::AddU32Result,
                        dst: move_arg_for(dest),
                    });
                },
                InstructionType::Copy{src, dst} => {
                    instructions.push(Instruction{
                        src: move_arg_for(src),
                        dst: move_arg_for(dst),
                    });
                },
                InstructionType::Call{ref dst, ref callee, ref signature, ref args} => {
                    dependencies.push(callee.clone());

                    let arg_offsets = offsets(signature.args.iter().map(|a|&a.1), 0);
                    for (arg, offset) in args.iter().zip(arg_offsets) {
                        instructions.push(Instruction{
                            src: move_arg_for(*arg),
                            dst: MoveArg::Stack{
                                offset: offset as i64 + stack_size as i64,
                                len: size(graph.object(*arg).t.as_ref().unwrap()) as u64,
                            },
                        });
                    }

                    instructions.push(Instruction{
                        src: MoveArg::StackIdx,
                        dst: MoveArg::ValA,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::Word(*stack_offsets.get(dst).unwrap() as u64),
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::AddResult,
                        dst: MoveArg::ReturnValuePos,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::Word(size(graph.object(*dst).t.as_ref().unwrap()) as u64),
                        dst: MoveArg::ReturnValueLen,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::Word(stack_size as u64),
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::AddResult,
                        dst: MoveArg::StackIdx,
                    });


                    instructions.push(Instruction{
                        src: MoveArg::InstructionIdx,
                        dst: MoveArg::ReturnAddressStack,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::FunctionStartIdx(callee.clone()),
                        dst: MoveArg::InstructionIdx,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::StackIdx,
                        dst: MoveArg::ValA,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::Word(stack_size as u64),
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::SubResult,
                        dst: MoveArg::StackIdx,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::ReturnValueLen,
                        dst: MoveArg::None,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::ReturnValuePos,
                        dst: MoveArg::None,
                    });

                },
                InstructionType::Return{src} => {
                    instructions.push(Instruction{
                        src: move_arg_for(src),
                        dst: MoveArg::ReturnValue,
                    });
                },
            }
        }

        let next_offset = last_offset + instructions.len() + match graph.block(block).jump.as_ref().unwrap().t {
            JumpType::Cond{..} => 3,
            JumpType::Uncond(_) => 1,
            JumpType::Return => 1,
        };
        *block_offsets.get_mut(&block) = Some(last_offset);
        last_offset = next_offset;

        blocks.push(instructions);
    }
    
    // Add the jump instructions to each block
    for (instructions, block) in blocks.iter_mut().zip(graph.blocks()) {
        match graph.block(block).jump.as_ref().unwrap().t {
            JumpType::Return => {
                if halt_on_return {
                    instructions.push(Instruction{
                        src: MoveArg::Byte(1),
                        dst: MoveArg::Halt,
                    });
                } else {
                    instructions.push(Instruction{
                        src: MoveArg::ReturnAddressStack,
                        dst: MoveArg::InstructionIdx,
                    });
                }
            },
            JumpType::Cond{condition, true_, false_} => {
                instructions.push(Instruction{
                    src: move_arg_for(condition),
                    dst: MoveArg::SkipNext,
                });
                instructions.push(Instruction{
                    src: MoveArg::Word(*block_offsets.get(&false_).unwrap() as u64),
                    dst: MoveArg::InstructionIdx,
                });
                instructions.push(Instruction{
                    src: MoveArg::Word(*block_offsets.get(&true_).unwrap() as u64),
                    dst: MoveArg::InstructionIdx,
                });
            },
            JumpType::Uncond(dest) => {
                instructions.push(Instruction{
                    src: MoveArg::Word(*block_offsets.get(&dest).unwrap() as u64),
                    dst: MoveArg::InstructionIdx,
                });
            },
        }
    }
    prog.instructions.extend(blocks.into_iter().flatten());
    Ok(dependencies)
}


#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetFunctionVmProgram {
    pub(crate) path: ItemPath,
}

impl crate::Query for GetFunctionVmProgram {
    type Output = Result<Program>;
}

impl GetFunctionVmProgram {
    pub(crate) async fn make(self, prog: Arc<crate::Program>) -> <Self as crate::Query>::Output {
        let mut vm_prog = Program::new();    
        let mut deps = vec![self.path];
        let mut halt_on_return = true;

        while !deps.is_empty() {
            deps.retain(|path| !vm_prog.functions.contains_key(path));

            let graph_futures = 
                deps.iter()
                .map(|path| crate::make_query!(&prog, GetFunctionGraph{path: path.clone()}));

            let graph_arcs = futures::future::join_all(graph_futures).await;

            let mut new_deps = Vec::new();
            for (arc, path) in graph_arcs.iter().zip(&deps) {
                if arc.is_err() {
                    return Err(arc.as_ref().as_ref().unwrap_err().clone());
                }

                vm_prog.functions.insert(path.clone(), vm_prog.instructions.len() as u64);
                new_deps.extend(vm_program(arc.as_ref().as_ref().unwrap(), &mut vm_prog, halt_on_return)?);
                halt_on_return = false;
            }
            deps = new_deps;
        }
        Ok(vm_prog)
    }
}
