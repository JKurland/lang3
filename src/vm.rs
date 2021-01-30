
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
    AddU32Result,
    EqResult,

    DataAddressA,
    DataLenA,
    DataA,

    DataAddressB,
    DataLenB,
    DataB,

    SkipNext,

    ProgramReturnValue,
    Halt,
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
}

impl Program {
    pub(crate) fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }
}

#[derive(Debug)]
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

    program_return_value: u64,
}

#[derive(Debug)]
enum DataSource {
    Byte(u8),
    Half(u32),
    Word(u64),
    Slice(*const u8, usize),
}

#[derive(Debug)]
enum DataSink<'a> {
    Register(&'a mut u64),
    Slice(*mut u8, usize),
    Halt,
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

            program_return_value: 0,
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
                MoveArg::EqResult => DataSource::Byte((self.val_a == self.val_b) as u8),

                MoveArg::DataAddressA => DataSource::Word(self.data_address_a),
                MoveArg::DataLenA => DataSource::Word(self.data_len_a),
                MoveArg::DataA => DataSource::Slice(self.data_address_a as *const u8, self.data_len_a as usize),

                MoveArg::DataAddressB => DataSource::Word(self.data_address_b),
                MoveArg::DataLenB => DataSource::Word(self.data_len_b),
                MoveArg::DataB => DataSource::Slice(self.data_address_b as *const u8, self.data_len_b as usize),

                MoveArg::SkipNext => DataSource::Word(self.skip_next),

                MoveArg::ProgramReturnValue => DataSource::Word(self.program_return_value),
                MoveArg::Halt => DataSource::Byte(0),
            };

            self.instruction_idx += 1;

            let dst = match inst.dst {
                MoveArg::Byte(_) => panic!("Invalid Dest"),
                MoveArg::Half(_) => panic!("Invalid Dest"),
                MoveArg::Word(_) => panic!("Invalid Dest"),
                MoveArg::Constant{..} => panic!("Invalid Dest"),
                MoveArg::AddressOfConstant{..} => panic!("Invalid Dest"),

                MoveArg::Stack{offset, len} => DataSink::Slice(&mut self.stack[(self.stack_idx as i64 + offset) as usize], len as usize),
                MoveArg::InstructionIdx => DataSink::Register(&mut self.instruction_idx),
                MoveArg::StackIdx => DataSink::Register(&mut self.stack_idx),
                
                MoveArg::ValA => DataSink::Register(&mut self.val_a),
                MoveArg::ValB => DataSink::Register(&mut self.val_b),
                MoveArg::AddResult => panic!("Invalid Dest"),
                MoveArg::AddU32Result => panic!("Invalid Dest"),
                MoveArg::EqResult => panic!("Invalid Dest"),

                MoveArg::DataAddressA => DataSink::Register(&mut self.data_address_a),
                MoveArg::DataLenA => DataSink::Register(&mut self.data_len_a),
                MoveArg::DataA => DataSink::Slice(self.data_address_a as *mut u8, self.data_len_a as usize),

                MoveArg::DataAddressB => DataSink::Register(&mut self.data_address_b),
                MoveArg::DataLenB => DataSink::Register(&mut self.data_len_b),
                MoveArg::DataB => DataSink::Slice(self.data_address_b as *mut u8, self.data_len_b as usize),

                MoveArg::SkipNext => DataSink::Register(&mut self.skip_next),

                MoveArg::ProgramReturnValue => DataSink::Register(&mut self.program_return_value),
                MoveArg::Halt => DataSink::Halt,
            };

            match (src, dst) {
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
                    let reg_ptr: *mut u64 = reg;
                    data.copy_to_nonoverlapping(reg_ptr.cast::<u8>(), len);
                },
                (DataSource::Slice(s, len), DataSink::Slice(d, _)) => unsafe {s.copy_to_nonoverlapping(d, len)},

                (_, DataSink::Halt) => return self.program_return_value,
            }
        }
    }
}
