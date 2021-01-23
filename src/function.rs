use futures::channel::mpsc::Receiver;

use crate::itemise::{ItemPath, GetGlobalItems, ItemType};
use crate::lex::{Token, TokenType};
use crate::{Result, Error, Query, Program, make_query};
use crate::vm;
use std::{collections::HashMap, sync::Arc, collections::HashSet};

#[derive(Debug)]
enum FunctionItemType {
    Loop,
    If,
    Expr,
    Scope,
    Return,
    Let,
    Assign,
}

#[derive(Debug)]
struct FunctionItem<'a> {
    t: FunctionItemType,
    // tokens includes the item prefix and closing brace, e.g. "loop {," and "}"
    tokens: &'a[Token],
}

#[derive(Debug)]
enum FunctionAstType {
    U32 (u32),
    StringLiteral(String),
    Return(Box<FunctionAst>),
    IfStatement {
        condition: Box<FunctionAst>,
        true_: Box<FunctionAst>,
        false_: Box<FunctionAst>,
    },
    Loop {
        body: Box<FunctionAst>,
    },
    Scope {
        items: Vec<FunctionAst>,
    },
    Let {
        ident: String,
        value: Box<FunctionAst>,
    },
    Assign {
        ident: String,
        value: Box<FunctionAst>,
    },
}
#[derive(Debug)]
pub(crate) struct FunctionAst {
    t: FunctionAstType,
}

fn itemise_function(token_stream: &[Token]) -> Result<Vec<FunctionItem>> {
    #[derive(Debug)]
    enum State {
        NewItem,
        IfSignature,
        LoopSignature,
        Expr,
        IfBody,
        Return,
        Let,
        Assign,
        Skip(usize, Box<State>),
        Else,
        InnerScope(usize, Box<State>),
        Body(FunctionItemType),
        FinishedItem(FunctionItemType),
    }

    let mut item_start_idx = 0;
    let mut state = State::NewItem;
    let mut items = Vec::new();
    for (idx, token) in token_stream.iter().enumerate() {
        dbg!(idx, token, &state);
        let new_state = match state {
            State::NewItem => {
                item_start_idx = idx;
                match token.t {
                    TokenType::If => State::IfSignature,
                    TokenType::Loop => State::LoopSignature,
                    TokenType::OpenBrace => State::Body(FunctionItemType::Scope),
                    TokenType::Return => State::Return,
                    TokenType::Let => {
                        let ident = token_stream.get(idx + 1).ok_or(Error::new("Expected ident after let"))?;
                        let eq = token_stream.get(idx + 2).ok_or(Error::new("Expected = after let"))?;
                        match (&ident.t, &eq.t) {
                            (TokenType::Ident(_), TokenType::Equal) => State::Skip(2, Box::new(State::Let)),
                            _ => return Err(Error::new("Expected ident and = after let")),
                        }
                    },
                    TokenType::Ident(_) => {
                        if let Some(next) = token_stream.get(idx + 1) {
                            match next.t {
                                TokenType::Equal => State::Skip(1, Box::new(State::Assign)),
                                _ => State::Expr,
                            }
                        } else {
                            State::Expr
                        }
                    },
                    _ => State::Expr,
                }
            },
            State::IfSignature => {
                match token.t {
                    TokenType::OpenBrace => State::IfBody,
                    TokenType::CloseBrace => return Err(Error::new("Unexpected }")),
                    _ => State::IfSignature,
                }
            },
            State::LoopSignature => {
                match token.t {
                    TokenType::OpenBrace => State::Body(FunctionItemType::Loop),
                    _ => return Err(Error::new("Expected {")),
                }
            },
            State::Expr => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(1, Box::new(state)),
                    TokenType::SemiColon => State::FinishedItem(FunctionItemType::Expr),
                    _ => state,
                }
            },
            State::IfBody => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(1, Box::new(state)),
                    TokenType::CloseBrace => {
                        match token_stream.get(idx + 1) {
                            Some(Token{t: TokenType::Else}) => State::Skip(1, Box::new(State::Else)),
                            _ => State::FinishedItem(FunctionItemType::If),
                        }
                    },
                    _ => state,
                }
            },
            State::Skip(left_to_skip, outer_state) => {
                if left_to_skip == 1 {
                    *outer_state
                } else {
                    State::Skip(left_to_skip - 1, outer_state)
                }
            },
            State::Else => {
                match token.t {
                    TokenType::If => State::IfSignature,
                    TokenType::OpenBrace => State::Body(FunctionItemType::If),
                    _ => return Err(Error::new("Exected if or {")),
                }
            },
            State::Body(item_type) => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(1, Box::new(State::Body(item_type))),
                    TokenType::CloseBrace => State::FinishedItem(item_type),
                    _ => State::Body(item_type),
                }
            },
            State::InnerScope(brace_count, outer_scope) => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(brace_count + 1, outer_scope),
                    TokenType::CloseBrace => {
                        if brace_count == 1 {
                            *outer_scope
                        } else {
                            State::InnerScope(brace_count - 1, outer_scope)
                        }
                    }
                    _ => State::InnerScope(brace_count, outer_scope),
                }
            },
            State::Return => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(1, Box::new(State::Return)),
                    TokenType::SemiColon => State::FinishedItem(FunctionItemType::Return),
                    _ => State::Return,
                }
            },
            State::Let => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(1, Box::new(State::Let)),
                    TokenType::SemiColon => State::FinishedItem(FunctionItemType::Let),
                    _ => State::Let,
                }
            },
            State::Assign => {
                match token.t {
                    TokenType::OpenBrace => State::InnerScope(1, Box::new(State::Assign)),
                    TokenType::SemiColon => State::FinishedItem(FunctionItemType::Assign),
                    _ => State::Assign,
                }
            },
            State::FinishedItem(_) => unreachable!(),
        };

        if let State::FinishedItem(item_type) = new_state {
            items.push(FunctionItem {
                t: item_type,
                tokens: &token_stream[item_start_idx..idx+1]
            });
            state = State::NewItem;
        } else {
            state = new_state;
        }

    }
    Ok(items)
}

pub(crate) fn parse_body(token_stream: &[Token]) -> Result<FunctionAst> {
    dbg!(token_stream);
    let items = itemise_function(token_stream)?;
    dbg!(&items);
    Ok(FunctionAst{t: FunctionAstType::Scope{
        items: items
                .iter()
                .map(parse_item)
                .collect::<Result<Vec<FunctionAst>>>()?
    }})
}

fn parse_scope(token_stream: &[Token]) -> Result<FunctionAst> {
    assert_eq!(token_stream.get(0), Some(&Token{t: TokenType::OpenBrace}));
    assert_eq!(token_stream.last(), Some(&Token{t: TokenType::CloseBrace}));

    parse_body(&token_stream[1..token_stream.len()-1])
}

fn parse_item(item: &FunctionItem) -> Result<FunctionAst> {
    match item.t {
        FunctionItemType::Expr => parse_expr(item.tokens),
        FunctionItemType::If => parse_if(item.tokens),
        FunctionItemType::Loop => parse_loop(item.tokens),
        FunctionItemType::Scope => parse_scope(item.tokens),
        FunctionItemType::Return => parse_return(item.tokens),
        FunctionItemType::Let => parse_let(item.tokens),
        FunctionItemType::Assign => parse_assign(item.tokens),
    }
}

fn parse_loop(token_stream: &[Token]) -> Result<FunctionAst> {
    assert_eq!(token_stream.get(0), Some(&Token{t: TokenType::Loop}));
    assert_eq!(token_stream.last(), Some(&Token{t: TokenType::CloseBrace}));
    
    if token_stream.get(1) != Some(&Token{t: TokenType::OpenBrace}) {
        return Err(Error::new("Expected {"));
    }

    let loop_body_tokens = &token_stream[2..token_stream.len()-1];
    Ok(FunctionAst {
        t: FunctionAstType::Loop{body: Box::new(parse_body(loop_body_tokens)?)}
    })
}

fn parse_return(token_stream: &[Token]) -> Result<FunctionAst> {
    assert_eq!(token_stream.get(0), Some(&Token{t: TokenType::Return}));
    Ok(FunctionAst{t: FunctionAstType::Return(Box::new(parse_expr(&token_stream[1..])?))})
}

fn parse_let(token_stream: &[Token]) -> Result<FunctionAst> {
    assert_eq!(token_stream.get(0), Some(&Token{t: TokenType::Let}));
    let ident = match token_stream.get(1).unwrap().t {
        TokenType::Ident(ref s) => s.clone(),
        _ => panic!("Found invalid let item"),
    };

    assert_eq!(token_stream.get(2), Some(&Token{t: TokenType::Equal}));
    Ok(FunctionAst {
        t: FunctionAstType::Let{ident, value: Box::new(parse_expr(&token_stream[3..])?)}
    })
}

fn parse_assign(token_stream: &[Token]) -> Result<FunctionAst> {
    let ident = match token_stream.get(0).unwrap().t {
        TokenType::Ident(ref s) => s.clone(),
        _ => panic!("Found invalid assign item"),
    };

    assert_eq!(token_stream.get(1), Some(&Token{t: TokenType::Equal}));
    Ok(FunctionAst {
        t: FunctionAstType::Assign{ident, value: Box::new(parse_expr(&token_stream[2..])?)}
    })
}

fn parse_if(token_stream: &[Token]) -> Result<FunctionAst> {
    assert_eq!(token_stream.get(0), Some(&Token{t: TokenType::If}));
    assert_eq!(token_stream.last(), Some(&Token{t: TokenType::CloseBrace}));

    let open_brace_pos = 
        token_stream
            .iter()
            .position(|t| t.t == TokenType::OpenBrace).unwrap();

    if open_brace_pos == 1 {
        return Err(Error::new("Expected condition"));
    }

    let condition_tokens = &token_stream[1..open_brace_pos];
    let condition = parse_expr(condition_tokens)?;

    let else_pos_opt = token_stream.iter().position(|t| t.t == TokenType::Else);

    let if_body_tokens = match else_pos_opt {
        None => {
            &token_stream[open_brace_pos+1..token_stream.len()-1]
        },
        Some(else_pos) => {
            // todo, check the position of the else is correct during function body itemisation
            &token_stream[open_brace_pos+1..else_pos]
        }
    };

    let else_body_tokens = match else_pos_opt {
        None => {
            &[]
        },
        Some(else_pos) => {
            if token_stream[else_pos + 1].t == TokenType::OpenBrace {
                // in the case of "else {"
                &token_stream[else_pos+2..token_stream.len()-1]
            } else if token_stream[else_pos + 1].t == TokenType::If {
                // in the case of "else if {"
                &token_stream[else_pos+1..token_stream.len()-1]
            } else {
                return Err(Error::new("Expected if or {"));
            }
        }
    };

    Ok(FunctionAst{t: FunctionAstType::IfStatement{
        condition: Box::new(condition),
        true_: Box::new(parse_body(if_body_tokens)?),
        false_: Box::new(parse_body(else_body_tokens)?),
    }})
}

fn parse_expr(token_stream: &[Token]) -> Result<FunctionAst> {
    if token_stream.len() != 2 {
        return Err(Error::new("Invalid expression"));
    }

    match token_stream.get(0) {
        None => Err(Error::new("Expected something")),
        Some(token) => {
            match token.t {
                TokenType::Int(ref s) => Ok(FunctionAst{t: FunctionAstType::U32(parse_u32(&s))}),
                TokenType::String(ref s) => Ok(FunctionAst{t: FunctionAstType::StringLiteral(s.clone())}),
                _ => Err(Error::new("Unexpected token in expr"))
            }
        }
    }
}

fn parse_u32(s: &str) -> u32 {
    let num = s.trim_start_matches('-');
    num.chars().fold(0, |acc, c| {
        (acc * 10) + c.to_digit(10).unwrap()
    })
}

#[derive(Debug)]
enum InstructionType {
    Eq{
        dest: ObjectHandle,
        lhs: ObjectHandle,
        rhs: ObjectHandle,
    },
    StoreU32(ObjectHandle, u32),
    StoreString(ObjectHandle, String),
}

#[derive(Debug)]
struct Instruction {
    t: InstructionType
}

#[derive(Debug)]
enum JumpType {
    Uncond(BlockHandle),
    Cond{
        condition: ObjectHandle,
        true_: BlockHandle,
        false_: BlockHandle,
    },
    Return(ObjectHandle),
}

#[derive(Debug)]
struct Jump {
    t: JumpType,
}
#[derive(Debug)]
struct Object {
    t: Option<Type>,
}

#[derive(Debug)]
struct Block {
    instructions: Vec<Instruction>,
    jump: Option<Jump>,
}

impl Block {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            jump: None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Hash, Eq, Debug)]
struct ObjectHandle (usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct BlockHandle (usize);

struct BlockHandleIter {
    n: usize,
    i: usize,
}

impl Iterator for BlockHandleIter {
    type Item = BlockHandle;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n {
            return None;
        }
        let ret = self.i;
        self.i += 1;
        Some(BlockHandle(ret))
    }
}

struct ObjectHandleIter {
    n: usize,
    i: usize,
}

impl Iterator for ObjectHandleIter {
    type Item = ObjectHandle;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n {
            return None;
        }
        let ret = self.i;
        self.i += 1;
        Some(ObjectHandle(ret))
    }
}

#[derive(Debug)]
pub(crate) struct Graph {
    objects: Vec<Object>,
    blocks: Vec<Block>,
    names: Vec<HashMap<String, ObjectHandle>>,
}

#[derive(Clone)]
struct Labels {
    break_: Option<BlockHandle>,
}

impl Graph {
    fn new() -> Self {
        Self {
            objects: Vec::new(),
            blocks: vec![Block::new()],
            names: vec![HashMap::new()],
        }
    }

    fn push_scope(&mut self) {
        self.names.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.names.pop();
    }
    fn add_name(&mut self, name: String) -> Result<ObjectHandle> {
        let new_object = self.new_object();
        let old_value = self.names.last_mut().unwrap().insert(name, new_object);
        if old_value.is_some() {
            return Err(Error::new("Name redefined"));
        } else {
            return Ok(new_object);
        }
    }
    fn get_name(&self, name: &str) -> Result<ObjectHandle> {
        for map in self.names.iter().rev() {
            if let Some(handle) = map.get(name) {
                return Ok(*handle);
            }
        }
        Err(Error::new("No such name"))
    }

    fn block_mut(&mut self, handle: BlockHandle) -> &mut Block {
        self.blocks.get_mut(handle.0).unwrap()
    }
    fn block(&self, handle: BlockHandle) -> &Block {
        self.blocks.get(handle.0).unwrap()
    }
    fn entry_block(&self) -> BlockHandle {
        BlockHandle(0)
    }
    fn blocks(&self) -> BlockHandleIter {
        BlockHandleIter{n: self.blocks.len(), i: 0}
    }
    fn new_block(&mut self) -> BlockHandle {
        self.blocks.push(Block::new());
        BlockHandle(self.blocks.len() - 1)
    }

    fn objects(&self) -> ObjectHandleIter {
        ObjectHandleIter{n: self.objects.len(), i: 0}
    }
    fn object(&self, handle: ObjectHandle) -> &Object {
        self.objects.get(handle.0).unwrap()
    }
    fn object_mut(&mut self, handle: ObjectHandle) -> &mut Object {
        self.objects.get_mut(handle.0).unwrap()
    }
    fn num_objects(&self) -> usize {
        return self.objects.len();
    }
    fn new_object(&mut self) -> ObjectHandle {
        self.objects.push(Object{t: None});
        ObjectHandle(self.blocks.len() - 1)
    }

    fn from_function_ast(ast: &FunctionAst, return_type: Type) -> Result<Self> {
        let mut graph = Self::new();
        dbg!(ast);
        graph.add_flow(ast, graph.entry_block(), &Labels{break_: None})?;

        InferenceSystem::add_types(&mut graph, return_type)?;
        for handle in graph.objects() {
            let object = graph.object(handle);
            if object.t.is_none() {
                return Err(Error::new("Couldn't infer all types"));
            }
        }

        Ok(graph)
    }

    pub(crate) fn from_function_ast_u32(ast: &FunctionAst) -> Result<Self> {
        Self::from_function_ast(ast, Type::U32)
    }

    fn evaluate_and_store(&mut self, ast: &FunctionAst, current_block: BlockHandle, object_handle: ObjectHandle) -> Result<BlockHandle> {
        let block = self.block_mut(current_block);
        match ast.t {
            FunctionAstType::U32(i) => {
                block.instructions.push(Instruction{t: InstructionType::StoreU32(object_handle, i)})
            },
            FunctionAstType::StringLiteral(ref s) => {
                block.instructions.push(Instruction{t: InstructionType::StoreString(object_handle, s.clone())})
            },
            _ => return Err(Error::new("Invalid expression"))
        }
        Ok(current_block)
    } 

    fn add_flow(&mut self, ast: &FunctionAst, current_block: BlockHandle, labels: &Labels) -> Result<BlockHandle> {
        match ast.t {
            FunctionAstType::IfStatement{
                ref condition,
                ref true_,
                ref false_
            } => {
                let cond_obj_handle = self.new_object();
                let after_eval_block = self.evaluate_and_store(&*condition, current_block, cond_obj_handle)?;
                
                let true_block = self.new_block();
                let false_block = self.new_block();
                
                self.block_mut(after_eval_block).jump = Some(Jump{t: JumpType::Cond{
                    condition: cond_obj_handle,
                    true_: true_block,
                    false_: false_block,
                }});

                let final_true_block = self.add_flow(
                    &*true_,
                    true_block,
                    labels
                )?;

                let final_false_block = self.add_flow(
                    &*false_,
                    false_block,
                    labels,
                )?;

                let after_block = self.new_block();

                self.block_mut(final_true_block).jump = Some(Jump{t: JumpType::Uncond(after_block)});
                self.block_mut(final_false_block).jump = Some(Jump{t: JumpType::Uncond(after_block)});
                Ok(after_block)
            },
            FunctionAstType::Loop{ref body} => {
                let body_block = self.new_block();
                self.block_mut(current_block).jump = Some(Jump{t: JumpType::Uncond(body_block)});

                let after_block = self.new_block();

                let mut loop_labels = labels.clone();
                loop_labels.break_ = Some(after_block);

                let final_block = self.add_flow(&*body, body_block, &loop_labels)?;
                self.block_mut(final_block).jump = Some(Jump{t: JumpType::Uncond(body_block)});

                Ok(after_block)
            },
            FunctionAstType::Scope{ref items} => {
                let mut block_handle = current_block;
                self.push_scope();
                for item in items {
                    block_handle = self.add_flow(item, block_handle, labels)?;
                }
                self.pop_scope();
                Ok(block_handle)
            },
            FunctionAstType::U32(_) => Err(Error::new("Unexpected int")),
            FunctionAstType::StringLiteral(_) => Err(Error::new("Unexpected string")),
            FunctionAstType::Return(ref value) => {
                let return_obj = self.new_object();
                let after_eval_block = self.evaluate_and_store(&*value, current_block, return_obj)?;
                self.block_mut(after_eval_block).jump = Some(Jump{t: JumpType::Return(return_obj)});
                Ok(current_block)
            },
            FunctionAstType::Let{ref ident, ref value} => {
                let var_obj = self.add_name(ident.clone())?;
                self.evaluate_and_store(&*value, current_block, var_obj)
            },
            FunctionAstType::Assign{ref ident, ref value} => {
                let var_obj = self.get_name(&ident)?;
                self.evaluate_and_store(&*value, current_block, var_obj)
            },
        }
    }


    pub(crate) fn vm_program(&self) -> Result<vm::Program> {
        let mut prog = vm::Program::new();
        let stack_offsets_iter = self.objects.iter()
            .map(|o| o.t.as_ref().unwrap().size())
            .scan(0, |total, size| {*total += size; Some(*total - size)});
        let stack_offsets: Vec<usize> = std::iter::once(0).chain(stack_offsets_iter).collect();

        
        let move_arg_for = |o: ObjectHandle| -> vm::MoveArg {
            vm::MoveArg::Stack{offset: stack_offsets[o.0] as i64, len: self.objects[o.0].t.as_ref().unwrap().size() as u64}
        };
        
        let move_arg_for_len = |o: ObjectHandle, len: usize, offset: usize| -> vm::MoveArg {
            vm::MoveArg::Stack{offset: (offset + stack_offsets[o.0]) as i64, len: len as u64}
        };
        
        // Make the instructions for each block
        let mut blocks: Vec<Vec<vm::Instruction>> = Vec::new();
        blocks.reserve(self.blocks.len());
        
        let mut block_offsets = Vec::new();
        block_offsets.reserve(self.blocks.len());
        let mut last_offset = prog.instructions.len();
        
        for block in &self.blocks {
            let mut instructions = Vec::new();
            for instruction in &block.instructions {
                match instruction.t {
                    InstructionType::Eq{dest, lhs, rhs} => {
                        instructions.push(vm::Instruction{
                            src: move_arg_for(lhs),
                            dst: vm::MoveArg::ValA,
                        });

                        instructions.push(vm::Instruction{
                            src: move_arg_for(rhs),
                            dst: vm::MoveArg::ValB,
                        });

                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::EqResult,
                            dst: move_arg_for(dest),
                        })
                    },
                    InstructionType::StoreU32(dest, value) => {
                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::Half(value),
                            dst: move_arg_for(dest),
                        });
                    },
                    InstructionType::StoreString(ref dest, ref val) => {
                        let const_idx = prog.constants.len();
                        prog.constants.extend(val.bytes());
                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::AddressOfConstant{idx: const_idx as u64},
                            dst: move_arg_for_len(*dest, 8, 0),
                        });

                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::Word(val.len() as u64),
                            dst: move_arg_for_len(*dest, 8, 8),
                        })
                    },
                }
            }

            let next_offset = last_offset + instructions.len() + match block.jump.as_ref().unwrap().t {
                JumpType::Cond{..} => 3,
                JumpType::Uncond(_) => 1,
                JumpType::Return(_) => 2,
            };
            block_offsets.push(last_offset);
            last_offset = next_offset;

            blocks.push(instructions);
        }
        
        // Add the jump instructions to each block
        for (instructions, block) in blocks.iter_mut().zip(&self.blocks) {
            match block.jump.as_ref().unwrap().t {
                JumpType::Return(obj) => {
                    instructions.push(vm::Instruction{
                        src: move_arg_for(obj),
                        dst: vm::MoveArg::ProgramReturnValue,
                    });
                    instructions.push(vm::Instruction{
                        src: vm::MoveArg::Byte(0),
                        dst: vm::MoveArg::Halt,
                    });
                },
                JumpType::Cond{condition, true_, false_} => {
                    instructions.push(vm::Instruction{
                        src: move_arg_for(condition),
                        dst: vm::MoveArg::SkipNext,
                    });
                    instructions.push(vm::Instruction{
                        src: vm::MoveArg::Word(block_offsets[false_.0] as u64),
                        dst: vm::MoveArg::InstructionIdx,
                    });
                    instructions.push(vm::Instruction{
                        src: vm::MoveArg::Word(block_offsets[true_.0] as u64),
                        dst: vm::MoveArg::InstructionIdx,
                    });
                },
                JumpType::Uncond(dest) => {
                    instructions.push(vm::Instruction{
                        src: vm::MoveArg::Word(block_offsets[dest.0] as u64),
                        dst: vm::MoveArg::InstructionIdx,
                    });
                },
            }
        }
        dbg!(&blocks);
        prog.instructions.extend(blocks.into_iter().flatten());
        Ok(prog)
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Constuctor {}

#[derive(Clone, PartialEq, Debug)]
enum Type {
    U32,
    Bool,
    String,
    Compound(Constuctor, Vec<Type>),
    Placeholder(ObjectHandle),
}

impl Type {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            Type::U32 => false,
            Type::Bool => false,
            Type::String => false,
            Type::Compound(_, types) => types.iter().any(|t| t.depends_on(obj)),
            Type::Placeholder(other) => obj == *other,
        }
    }

    fn do_substitutions(&mut self, graph: &Graph) {
        match self {
            Type::Compound(_, types) => types.into_iter().for_each(|t| t.do_substitutions(graph)),
            Type::Placeholder(obj) => {
                if let Some(ref sub) = graph.object(*obj).t {
                    *self = sub.clone();
                }
            },
            _ => {},
        }
    }

    fn size(&self) -> usize {
        match self {
            Type::U32 => 4,
            Type::Bool => 1,
            Type::String => 16,
            Type::Compound(_, _) => panic!("Compound not supported"),
            Type::Placeholder(_) => panic!("Remaining placeholde"),
        }
    }
}

struct TypeEquation {
    lhs: Type,
    rhs: Type,
}

struct InferenceSystem {
    equations: Vec<TypeEquation>,
}

impl InferenceSystem {
    fn add_eqn(&mut self, lhs: Type, rhs: Type) {
        self.equations.push(TypeEquation{lhs, rhs});
    }

    fn add_types(graph: &mut Graph, return_type: Type) -> Result<()> {
        let mut is = InferenceSystem{equations: Vec::new()};
        for block_handle in graph.blocks() {
            let block = graph.block(block_handle);
            for inst in &block.instructions {
                match inst.t {
                    InstructionType::Eq{dest, lhs, rhs} => {
                        is.add_eqn(Type::Placeholder(dest), Type::Bool);
                        is.add_eqn(Type::Placeholder(lhs), Type::Placeholder(rhs));
                    },
                    InstructionType::StoreU32(dest, _) => {
                        is.add_eqn(Type::Placeholder(dest), Type::U32);
                    }
                    InstructionType::StoreString(dest, _) => {
                        is.add_eqn(Type::Placeholder(dest), Type::String);
                    }
                }
            }

            match &block.jump {
                None => {},
                Some(j) => {
                    match j.t {
                        JumpType::Return(val) => {
                            is.add_eqn(Type::Placeholder(val), return_type.clone());
                        },
                        JumpType::Cond{condition, ..} => {
                            is.add_eqn(Type::Placeholder(condition), Type::Bool);
                        },
                        JumpType::Uncond(..) => {},
                    }
                }
            }
        }
        is.results(graph)?;
        Ok(())
    }

    fn results(&mut self, graph: &mut Graph) -> Result<()> {
        while self.can_progress() {
            self.delete_tautologies();
            self.decompose();
            self.check_conflict()?;
            self.swap();
            self.eliminate(graph);
            self.check()?;
        }
        Ok(())
    }

    fn delete_tautologies(&mut self) {
        self.equations.retain(|e| e.lhs != e.rhs);
    }

    fn decompose(&mut self) {
        let mut new_equations = Vec::new();
        let mut to_remove = Vec::new();
        for (idx, eqn) in self.equations.iter().enumerate() {
            match (&eqn.lhs, &eqn.rhs) {
                (Type::Compound(c, args), Type::Compound(c2, args2)) => {
                    if c == c2 && args.len() == args2.len() {
                        to_remove.push(idx);
                        for (a1, a2) in args.iter().zip(args2.iter()) {
                            new_equations.push(TypeEquation{
                                lhs: a1.clone(),
                                rhs: a2.clone(),
                            });
                        }
                    }
                },
                _ => {}
            }
        }

        for idx in to_remove.into_iter().rev() {
            self.equations.swap_remove(idx);
        }

        self.equations.append(&mut new_equations);
    }

    fn check_conflict(&self) -> Result<()> {
        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (Type::Compound(c, args), Type::Compound(c2, args2)) => {
                    if c != c2 || args.len() != args2.len() {
                        return Err(Error::new("Type error"));
                    }
                },
                _ => {}
            }
        }
        Ok(())
    }

    fn swap(&mut self) {
        for eqn in &mut self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (_, Type::Placeholder(_)) => {
                    std::mem::swap(&mut eqn.lhs, &mut eqn.rhs);
                },
                _ => {}
            }
        }
    }

    fn eliminate(&mut self, graph: &mut Graph) {
        let mut to_remove = Vec::new();
        for (idx, eqn) in self.equations.iter().enumerate() {
            match (&eqn.lhs, &eqn.rhs) {
                (Type::Placeholder(_), Type::Placeholder(_)) => {},
                (Type::Placeholder(obj), rhs) => {
                    if !rhs.depends_on(*obj) {
                        to_remove.push(idx);
                        graph.object_mut(*obj).t = Some(rhs.clone());
                    }
                },
                _ => {}
            }
        }

        for idx in to_remove.into_iter().rev() {
            self.equations.swap_remove(idx);
        }

        for eqn in &mut self.equations {
            eqn.lhs.do_substitutions(graph);
            eqn.rhs.do_substitutions(graph);
        }
    }

    fn check(&self) -> Result<()> {
        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (Type::Placeholder(obj), rhs) => {
                    if rhs.depends_on(*obj) {
                        return Err(Error::new("Self referential type"));
                    }
                },
                _ => {}
            }
        }
        Ok(())
    }

    fn can_progress(&self) -> bool {
        if self.equations.is_empty() {
            return false;
        }

        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (Type::Placeholder(_), Type::Placeholder(_)) => {},
                _ => return true,
            }
        }
        return false;
    }
}



#[derive(Hash, PartialEq, Clone)]
struct GetFunctionAst {
    path: ItemPath,
}

impl Query for GetFunctionAst {
    type Output = Result<FunctionAst>;
}

impl GetFunctionAst {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let global_items_arc = make_query!(&prog, GetGlobalItems).await;
        if global_items_arc.is_err() {
            return Err(global_items_arc.as_ref().as_ref().unwrap_err().clone());
        }

        let global_items = global_items_arc.as_ref().as_ref().unwrap();

        let item = global_items.get(&self.path).ok_or(
            Error::new("Could not find function")
        )?;

        if item.t != ItemType::Fn {
            return Err(Error::new("Not a function"));
        }

        parse_body(&item.tokens)
    }
}

#[derive(Hash, PartialEq, Clone)]
struct GetFunctionGraph {
    path: ItemPath,
}

impl Query for GetFunctionGraph {
    type Output = Result<Graph>;
}

impl GetFunctionGraph {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let function_ast_arc = make_query!(&prog, GetFunctionAst{path: self.path}).await;
        if function_ast_arc.is_err() {
            return Err(function_ast_arc.as_ref().as_ref().unwrap_err().clone());
        }

        let function_ast = function_ast_arc.as_ref().as_ref().unwrap();
        Graph::from_function_ast(function_ast, Type::U32)
    }
}