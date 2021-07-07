use function::Type;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::itemise::ItemPath;
use std::collections::HashMap;
use std::sync::Arc;
use crate::function::{GetFunctionGraph, InstructionType, JumpType};
use crate::{Result, make_query};
use crate::function;
use crate::storage::Storage;
use crate::Program;
use crate::structs::{GetStruct, Struct};

#[derive(Clone, Copy)]
struct SendPtr<T> {
    p: *const T
}

impl<T> SendPtr<T> {
    fn new(p: &T) -> Self {
        Self{
            p
        }
    }

    unsafe fn deref(&self) -> &T {
        &*self.p
    }
}

unsafe impl<T> Sync for SendPtr<T> {}
unsafe impl<T> Send for SendPtr<T> {}

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

fn size<'a, 'b: 'a, 'c: 'a>(t: &'b Type, prog: &'c Arc<Program>) -> BoxFuture<'a, Result<usize>> {
    async move {
        match t {
            Type::U32 => Ok(4),
            Type::Bool => Ok(1),
            Type::String => Ok(16),
            Type::Never => Ok(0),
            Type::Null => Ok(0),
            Type::Struct(path) => {
                let struct_ = make_query!(prog, GetStruct{path: path.clone()}).await?;
                let mut s = 0;
                for t in struct_.member_types() {
                    s = align_up_to(s, align(t, prog).await?);
                    s += size(t, prog).await?;
                }
                Ok(s)
            },
            Type::Compound(_, _) => panic!("Compound not supported"),
        }
    }.boxed()
}

fn align<'a, 'b: 'a, 'c: 'a>(t: &'b Type, prog: &'c Arc<Program>) -> BoxFuture<'a, Result<usize>> {
    async move {
        match t {
            Type::U32 => Ok(std::mem::align_of::<u32>()),
            Type::Bool => Ok(1),
            Type::String => Ok(16),
            Type::Never => Ok(1),
            Type::Null => Ok(1),
            Type::Struct(path) => {
                let struct_ = make_query!(prog, GetStruct{path: path.clone()}).await?;
                let first_type = struct_.member_types().next();
                match first_type {
                    Some(t) => {
                        align(t, prog).await
                    },
                    None => Ok(1),
                }
            },
            Type::Compound(_, _) => panic!("Compound not supported align"),
        }
    }.boxed()
}

async fn struct_field_offset(struct_: &Arc<Struct>, prog: &Arc<Program>, field_name: &str) -> Result<usize> {
    let member_idx = struct_.member_idx(field_name).unwrap();
    let mut s = 0;
    for t in struct_.member_types().take(member_idx) {
        s = align_up_to(s, align(t, &prog).await?);
        s += size(t, &prog).await?;
    }
    s = align_up_to(s, align(struct_.get_member(field_name).unwrap(), &prog).await?);
    Ok(s)
}

async fn field_offset(struct_path: &ItemPath, prog: Arc<Program>, field_name: &str) -> Result<usize> {
    let struct_ = make_query!(prog, GetStruct{path: struct_path.clone()}).await?;
    struct_field_offset(&struct_, &prog, field_name).await
}

async fn field_spec_offset(struct_path: &ItemPath, prog: Arc<Program>, field_spec: &[String]) -> Result<usize> {
    let mut offset = 0;

    let mut struct_ = make_query!(prog, GetStruct{path: struct_path.clone()}).await?;
    for field_name in field_spec.iter().take(field_spec.len() - 1) {

        offset += struct_field_offset(&struct_, &prog, field_name).await?;

        let next_struct_type = struct_.get_member(field_name).unwrap();
        if let Type::Struct(next_item_path) = next_struct_type {
            struct_ = make_query!(prog, GetStruct{path: next_item_path.clone()}).await?;
        } else {
            panic!("Field access on non struct in vm");
        }
    }

    if let Some(last_field) = field_spec.last() {
        offset += struct_field_offset(&struct_, &prog, last_field).await?;
    }

    Ok(offset)
}

fn align_up_to(x: usize, align: usize) -> usize {
    x + ((align - (x % align)) % align)
}

async fn offsets<'a, T: Iterator<Item = &'a Type> + 'a>(types: T, initial: usize, prog: Arc<Program>) -> Result<Vec<usize>> {
    let mut total = initial;
    let mut rtn = Vec::new();
    for t in types {
        total = align_up_to(total, align(t, &prog).await?);
        rtn.push(total);
        total += size(t, &prog).await?;
    }
    Ok(rtn)
}

#[derive(Debug)]
pub(crate) struct Instruction {
    pub(crate) src: MoveArg,
    pub(crate) dst: MoveArg,
}

#[derive(Debug)]
pub(crate) struct VmProgram {
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) constants: Vec<u8>,
    pub(crate) functions: HashMap<ItemPath, u64>,
}

impl VmProgram {
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

    pub(crate) fn run(&mut self, program: &VmProgram) -> u64 {
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


async fn vm_program(graph: &function::Graph, vm_prog: &mut VmProgram, prog: Arc<Program>, halt_on_return: bool) -> Result<Vec<ItemPath>> {
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
        for (arg, offset) in args.iter().zip(offsets(arg_types, 0, prog.clone()).await?) {
            let handle = arg.expect("Missing arg index");
            *stack_offsets.get_mut(&handle) = Some(offset);
            total = offset;
        } 

        let local_types = locals.iter().map(|handle| {
            graph.object(*handle).t.as_ref().unwrap()
        });

        for (handle, offset) in locals.iter().zip(offsets(local_types, total, prog.clone()).await?) {
            *stack_offsets.get_mut(handle) = Some(offset);
            total = offset;
        } 
        
        (stack_offsets, total)
    };

    let move_arg_for = |o: function::ObjectHandle| {
        let gp = SendPtr::new(graph);
        let sop = SendPtr::new(&stack_offsets);
        let pp = SendPtr::new(&prog);
        async move {
            // The returned future from this closure never outlives graph, stack_offsets or prog
            unsafe {
                Ok(MoveArg::Stack{
                    offset: *(*sop.deref()).get(&o).unwrap() as i64,
                    len: size((*gp.deref()).object(o).t.as_ref().unwrap(), &*pp.deref()).await? as u64
                })
            }
        }
    };

    let offset_move_arg_for = |o: function::ObjectHandle, offset: usize| {
        let gp = SendPtr::new(graph);
        let sop = SendPtr::new(&stack_offsets);
        let pp = SendPtr::new(&prog);
        async move {
            // The returned future from this closure never outlives graph, stack_offsets or prog
            unsafe {
                Ok(MoveArg::Stack{
                    offset: (*(*sop.deref()).get(&o).unwrap() + offset) as i64,
                    len: size((*gp.deref()).object(o).t.as_ref().unwrap(), &*pp.deref()).await? as u64
                })
            }
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
    let mut last_offset = vm_prog.instructions.len();

    for block in graph.blocks() {
        let mut instructions = Vec::new();
        for instruction in &graph.block(block).instructions {
            match instruction.t {
                InstructionType::Eq{dest, lhs, rhs} => {
                    instructions.push(Instruction{
                        src: move_arg_for(lhs).await?,
                        dst: MoveArg::ValA,
                    });

                    instructions.push(Instruction{
                        src: move_arg_for(rhs).await?,
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::EqResult,
                        dst: move_arg_for(dest).await?,
                    });
                },
                InstructionType::StoreU32(dest, value) => {
                    instructions.push(Instruction{
                        src: MoveArg::Half(value),
                        dst: move_arg_for(dest).await?,
                    });
                },
                InstructionType::StoreString(ref dest, ref val) => {
                    let const_idx = vm_prog.constants.len();
                    vm_prog.constants.extend(val.bytes());
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
                        src: move_arg_for(lhs).await?,
                        dst: MoveArg::ValA,
                    });

                    instructions.push(Instruction{
                        src: move_arg_for(rhs).await?,
                        dst: MoveArg::ValB,
                    });

                    instructions.push(Instruction{
                        src: MoveArg::AddU32Result,
                        dst: move_arg_for(dest).await?,
                    });
                },
                InstructionType::Copy{src, dst} => {
                    instructions.push(Instruction{
                        src: move_arg_for(src).await?,
                        dst: move_arg_for(dst).await?,
                    });
                },
                InstructionType::Call{ref dst, ref callee, ref signature, ref args} => {
                    dependencies.push(callee.clone());

                    let arg_offsets = offsets(signature.args.iter().map(|a|&a.1), 0, prog.clone()).await?;
                    for (arg, offset) in args.iter().zip(arg_offsets) {
                        instructions.push(Instruction{
                            src: move_arg_for(*arg).await?,
                            dst: MoveArg::Stack{
                                offset: offset as i64 + stack_size as i64,
                                len: size(graph.object(*arg).t.as_ref().unwrap(), &prog).await? as u64,
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
                        src: MoveArg::Word(size(graph.object(*dst).t.as_ref().unwrap(), &prog).await? as u64),
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
                        src: move_arg_for(src).await?,
                        dst: MoveArg::ReturnValue,
                    });
                },
                InstructionType::StructFieldAssignment{ref parent_object, ref field_spec, ref value} => {
                    let parent_type = graph.object(*parent_object).t.as_ref().unwrap();
                    if let Type::Struct(path) = parent_type {
                        instructions.push(Instruction{
                            src: move_arg_for(*value).await?,
                            dst: offset_move_arg_for(*parent_object, field_spec_offset(&path, prog.clone(), field_spec).await?).await?
                        });
                    } else {
                        instructions.push(Instruction{
                            src: move_arg_for(*value).await?,
                            dst: move_arg_for(*parent_object).await?,
                        });
                    }
                },
                InstructionType::StructFieldAccess{ref parent_object, ref field, ref dest} => {
                    let parent_type = graph.object(*parent_object).t.as_ref().unwrap();
                    if let Type::Struct(path) = parent_type {
                        instructions.push(Instruction{
                            src: offset_move_arg_for(*parent_object, field_offset(&path, prog.clone(), field).await?).await?,
                            dst: move_arg_for(*dest).await?,
                        });
                    } else {
                        panic!("Field access on non struct");
                    }
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
                    src: move_arg_for(condition).await?,
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
    vm_prog.instructions.extend(blocks.into_iter().flatten());
    Ok(dependencies)
}


#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetFunctionVmProgram {
    pub(crate) path: ItemPath,
}

impl crate::Query for GetFunctionVmProgram {
    type Output = Result<VmProgram>;
}

impl GetFunctionVmProgram {
    pub(crate) async fn make(self, prog: Arc<crate::Program>) -> <Self as crate::Query>::Output {
        let mut vm_prog = VmProgram::new();    
        let mut deps = vec![self.path];
        let mut halt_on_return = true;

        while !deps.is_empty() {
            deps.retain(|path| !vm_prog.functions.contains_key(path));

            let graph_futures = 
                deps.iter()
                .map(|path| crate::make_query!(&prog, GetFunctionGraph{path: path.clone()}));

            let graphs = futures::future::try_join_all(graph_futures).await?;

            let mut new_deps = Vec::new();
            for (graph, path) in graphs.iter().zip(&deps) {
                vm_prog.functions.insert(path.clone(), vm_prog.instructions.len() as u64);
                new_deps.extend(vm_program(graph, &mut vm_prog, prog.clone(), halt_on_return).await?);
                halt_on_return = false;
            }
            deps = new_deps;
        }
        Ok(vm_prog)
    }
}
