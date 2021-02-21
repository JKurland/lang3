use crate::itemise::{self, GetGlobalItems, ItemPath, ItemType, Item};
use crate::lex::{Token, TokenType};
use crate::{Result, Error, Query, Program, make_query};
use crate::vm;
use std::{collections::HashMap, sync::Arc};
use futures::{join, future::join_all};
use itemise::FunctionSignature;

#[derive(Debug)]
enum FunctionItemType {
    Loop,
    If,
    Break,
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
    VarName(String),
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
        t: Option<Type>,
        value: Box<FunctionAst>,
    },
    Assign {
        ident: String,
        value: Box<FunctionAst>,
    },
    Add {
        lhs: Box<FunctionAst>,
        rhs: Box<FunctionAst>,
    },
    DoubleEq {
        lhs: Box<FunctionAst>,
        rhs: Box<FunctionAst>,
    },
    ParenGroup {
        lhs: Option<Box<FunctionAst>>,
        inside: Option<Box<FunctionAst>>,
        rhs: Option<Box<FunctionAst>>,
    },
    Break,
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
        Break,
        Return,
        LetBeforeEq,
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
        let new_state = match state {
            State::NewItem => {
                item_start_idx = idx;
                match token.t {
                    TokenType::If => State::IfSignature,
                    TokenType::Loop => State::LoopSignature,
                    TokenType::OpenBrace => State::Body(FunctionItemType::Scope),
                    TokenType::Return => State::Return,
                    TokenType::Let => State::LetBeforeEq,
                    TokenType::Break => State::Break,
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
            State::Break => {
                match token.t {
                    TokenType::SemiColon => State::FinishedItem(FunctionItemType::Break),
                    _ => return Err(Error::new("Expected ;")),
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
            State::LetBeforeEq => {
                match token.t {
                    TokenType::Equal => State::Let,
                    _ => State::LetBeforeEq,
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

    match state {
        State::NewItem => Ok(items),
        _ => Err(Error::new("Unexpected end of item")),
    }
}

pub(crate) fn parse_body(token_stream: &[Token]) -> Result<FunctionAst> {
    let items = itemise_function(token_stream)?;
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
        FunctionItemType::Break => Ok(FunctionAst{t: FunctionAstType::Break}),
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
        _ => return Err(Error::new("Expected ident after let")),
    };

    let eq_idx = token_stream.iter()
        .enumerate()
        .find_map(|(idx, token)| {
            if token.t == TokenType::Equal {
                Some(idx)
            } else {
                None
            }
        });
    
    match eq_idx {
        Some(i) => {
            Ok(FunctionAst{t: FunctionAstType::Let{
                ident,
                t: None,
                value: Box::new(parse_expr(&token_stream[i + 1..])?),
            }})
        },
        None => Err(Error::new("Expected = after let")),
    }
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
            &token_stream[open_brace_pos+1..else_pos-1]
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

#[derive(Debug)]
enum OperatorType {
    Add,
    DoubleEq,
    ParenGroup(usize),
}

#[derive(Debug)]
struct Operator {
    t: OperatorType,
    idx: usize,
    priority: i32,
}


fn parse_expr_inner(operators: &[Operator], token_stream: &[Token], left_end: usize, right_end: usize) -> Result<FunctionAst> {
    if let Some((root_op_idx, root_op)) = operators.iter().enumerate().min_by_key(|o| o.1.priority) {
        match root_op.t {
            OperatorType::Add => {
                Ok(FunctionAst{t: FunctionAstType::Add{
                    lhs: Box::new(parse_expr_inner(&operators[..root_op_idx], token_stream, left_end, root_op.idx - 1)?),
                    rhs: Box::new(parse_expr_inner(&operators[root_op_idx + 1..], token_stream, root_op.idx + 1, right_end)?),
                }})
            },
            OperatorType::DoubleEq => {
                Ok(FunctionAst{t: FunctionAstType::DoubleEq{
                    lhs: Box::new(parse_expr_inner(&operators[..root_op_idx], token_stream, left_end, root_op.idx - 1)?),
                    rhs: Box::new(parse_expr_inner(&operators[root_op_idx + 1..], token_stream, root_op.idx + 1, right_end)?),
                }})
            },
            OperatorType::ParenGroup(open_idx) => {
                // By this point we've already handled all the operators where one of the operands can be a paren group
                // since all their evalutation priorities are lower than that of ParenGroup. That means lhs and rhs must
                // both be complete expressions since otherwise this ParenGroup would be one of their operands.
                let lhs = if left_end == open_idx {
                    None
                } else {
                    Some(Box::new(parse_expr_inner(&operators[..root_op_idx], token_stream, left_end, open_idx - 1)?))
                };

                let rhs = if right_end == root_op.idx {
                    None
                } else {
                    Some(Box::new(parse_expr_inner(&operators[root_op_idx + 1..], token_stream, root_op.idx + 1, right_end)?))
                };

                let inside = if open_idx + 1 == root_op.idx {
                    None
                } else {
                    Some(Box::new(parse_expr(&token_stream[open_idx+1..root_op.idx])?))
                };

                Ok(FunctionAst{t: FunctionAstType::ParenGroup{lhs, rhs, inside}})
            },
        }
    } else {
        if left_end != right_end {
            Err(Error::new("Invalid expression, expected literal"))
        } else {
            match token_stream[left_end].t {
                TokenType::Int(ref s) => Ok(FunctionAst{t: FunctionAstType::U32(parse_u32(&s))}),
                TokenType::String(ref s) => Ok(FunctionAst{t: FunctionAstType::StringLiteral(s.clone())}),
                TokenType::Ident(ref s) => Ok(FunctionAst{t: FunctionAstType::VarName(s.clone())}),
                _ => Err(Error::new("Unexpected token in expr")),
            }
        }
    }
}

fn parse_expr(token_stream: &[Token]) -> Result<FunctionAst> {
    let expr_body = if token_stream.last() == Some(&Token{t: TokenType::SemiColon}) {
        &token_stream[0..token_stream.len()-1]
    } else {
        token_stream
    };


    // find the operators in the expression
    let mut operators = Vec::new();
    operators.reserve(expr_body.len());

    enum State {
        TopLevel,
        InParens(usize),
    }

    let mut state = State::TopLevel;

    for (idx, token) in expr_body.iter().enumerate() {
        let new_state = match state {
            State::TopLevel => {
                match token.t {
                    TokenType::Plus => {
                        operators.push(Operator{
                            t: OperatorType::Add,
                            idx,
                            priority: 10,
                        });
                        state
                    },
                    TokenType::DoubleEq => {
                        operators.push(Operator{
                            t: OperatorType::DoubleEq,
                            idx,
                            priority: 0,
                        });
                        state
                    },
                    TokenType::OpenParen => State::InParens(idx),
                    _ => state,
                }
            },
            State::InParens(open_idx) => {
                match token.t {
                    TokenType::CloseParen => {
                        operators.push(Operator{
                            t: OperatorType::ParenGroup(open_idx),
                            idx,
                            priority: 100,
                        });
                        State::TopLevel
                    },
                    _ => state,
                }
            },
        };
        state = new_state;
    }

    if let State::InParens(_) = state {
        return Err(Error::new("Expected )"));
    }

    // fill in the lhs and rhs of each operator
    parse_expr_inner(&operators, expr_body, 0, expr_body.len() - 1)
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
    Add{
        dest: ObjectHandle,
        lhs: ObjectHandle,
        rhs: ObjectHandle,
    },
    StoreU32(ObjectHandle, u32),
    StoreString(ObjectHandle, String),
    Copy{
        src: ObjectHandle,
        dst: ObjectHandle,
    },
    Call{
        callee: ItemPath,
        signature: FunctionSignature,
        args: Vec<ObjectHandle>,
    },
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
        ObjectHandle(self.objects.len() - 1)
    }

    pub(crate) fn from_function_ast(ast: &FunctionAst, return_type: Type, global_items: &HashMap<ItemPath, Item>) -> Result<Self> {
        let mut graph = Self::new();
        graph.add_flow(ast, graph.entry_block(), &Labels{break_: None}, global_items)?;

        InferenceSystem::add_types(&mut graph, return_type)?;
        for handle in graph.objects() {
            let object = graph.object(handle);
            if object.t.is_none() {
                return Err(Error::new("Couldn't infer all types"));
            }
        }

        Ok(graph)
    }

    fn evaluate_and_store(&mut self, ast: &FunctionAst, mut current_block: BlockHandle, object_handle: ObjectHandle) -> Result<BlockHandle> {
        match ast.t {
            FunctionAstType::U32(i) => {
                let block = self.block_mut(current_block);
                block.instructions.push(Instruction{t: InstructionType::StoreU32(object_handle, i)});
            },
            FunctionAstType::StringLiteral(ref s) => {
                let block = self.block_mut(current_block);
                block.instructions.push(Instruction{t: InstructionType::StoreString(object_handle, s.clone())});
            },
            FunctionAstType::Add{ref lhs, ref rhs} => {
                let lhs_obj = self.new_object();
                current_block = self.evaluate_and_store(&*lhs, current_block, lhs_obj)?;

                let rhs_obj = self.new_object();
                current_block = self.evaluate_and_store(&*rhs, current_block, rhs_obj)?;

                let block = self.block_mut(current_block);
                block.instructions.push(Instruction{t: InstructionType::Add{
                    dest: object_handle,
                    lhs: lhs_obj,
                    rhs: rhs_obj,
                }});
            },
            FunctionAstType::DoubleEq{ref lhs, ref rhs} => {
                let lhs_obj = self.new_object();
                current_block = self.evaluate_and_store(&*lhs, current_block, lhs_obj)?;

                let rhs_obj = self.new_object();
                current_block = self.evaluate_and_store(&*rhs, current_block, rhs_obj)?;

                let block = self.block_mut(current_block);
                block.instructions.push(Instruction{t: InstructionType::Eq{
                    dest: object_handle,
                    lhs: lhs_obj,
                    rhs: rhs_obj,
                }});
            },
            FunctionAstType::VarName(ref s) => {
                let src = self.get_name(s)?;
                let block = self.block_mut(current_block);
                block.instructions.push(Instruction{t: InstructionType::Copy{src, dst: object_handle}});
            },
            _ => return Err(Error::new("Invalid expression"))
        }
        Ok(current_block)
    } 

    fn add_flow(&mut self, ast: &FunctionAst, current_block: BlockHandle, labels: &Labels, global_items: &HashMap<ItemPath, Item>) -> Result<BlockHandle> {
        match ast.t {
            FunctionAstType::IfStatement{ref condition, ref true_, ref false_ } => {
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
                    labels,
                    global_items,
                )?;

                let final_false_block = self.add_flow(
                    &*false_,
                    false_block,
                    labels,
                    global_items,
                )?;

                let after_block = self.new_block();

                if self.block(final_true_block).jump.is_none() {
                    self.block_mut(final_true_block).jump = Some(Jump{t: JumpType::Uncond(after_block)});
                }

                if self.block(final_false_block).jump.is_none() {
                    self.block_mut(final_false_block).jump = Some(Jump{t: JumpType::Uncond(after_block)});
                }
                Ok(after_block)
            },
            FunctionAstType::Loop{ref body} => {
                let body_block = self.new_block();
                self.block_mut(current_block).jump = Some(Jump{t: JumpType::Uncond(body_block)});

                let after_block = self.new_block();

                let mut loop_labels = labels.clone();
                loop_labels.break_ = Some(after_block);

                let final_block = self.add_flow(&*body, body_block, &loop_labels, global_items)?;
                self.block_mut(final_block).jump = Some(Jump{t: JumpType::Uncond(body_block)});

                Ok(after_block)
            },
            FunctionAstType::Break => {
                self.block_mut(current_block).jump = Some(Jump{
                    t: JumpType::Uncond(labels.break_.ok_or(Error::new("Break must be inside loop"))?)
                });
                Ok(current_block)
            },
            FunctionAstType::Scope{ref items} => {
                let mut block_handle = current_block;
                self.push_scope();
                for item in items {
                    block_handle = self.add_flow(item, block_handle, labels, global_items)?;
                }
                self.pop_scope();
                Ok(block_handle)
            },
            FunctionAstType::ParenGroup{ref lhs, ref inside, ref rhs} => {
                if rhs.is_some() {
                    return Err(Error::new("Paren group not supported as prefix operator"));
                }

                
                match lhs {
                    None => {
                        let val = self.new_object();
                        match inside {
                            None => Err(Error::new("Expected something inside parens")),
                            Some(inside) => self.evaluate_and_store(&**inside, current_block, val),
                        }
                    },
                    Some(lhs) => {
                        let callee_path;
                        if let FunctionAstType::VarName(ref callee_name) = lhs.t {
                            callee_path = ItemPath::new(callee_name);
                        } else {
                            return Err(Error::new("Expected name before parens"))
                        }
                        
                        let callee_item = global_items.get(&callee_path).ok_or(Error::new("Unknown item"))?;

                        if let ItemType::Fn(ref signature) = callee_item.t {
                            self.block_mut(current_block).instructions.push(Instruction{
                                t: InstructionType::Call{
                                    callee: callee_path,
                                    signature: signature.clone(),
                                    args: vec![]
                                }
                            });
                            Ok(current_block)
                        } else {
                            Err(Error::new("Can only call functions"))
                        }
                    },
                }
            },
            FunctionAstType::U32(_) => Err(Error::new("Unexpected int")),
            FunctionAstType::StringLiteral(_) => Err(Error::new("Unexpected string")),
            FunctionAstType::Add{..} => Err(Error::new("Unexpected add")),
            FunctionAstType::DoubleEq{..} => Err(Error::new("Unexpected ==")),
            FunctionAstType::VarName{..} => Err(Error::new("Unexpected var name")),
            FunctionAstType::Return(ref value) => {
                let return_obj = self.new_object();
                let after_eval_block = self.evaluate_and_store(&*value, current_block, return_obj)?;
                self.block_mut(after_eval_block).jump = Some(Jump{t: JumpType::Return(return_obj)});
                Ok(current_block)
            },
            FunctionAstType::Let{ref ident, t: _, ref value} => {
                let var_obj = self.add_name(ident.clone())?;
                self.evaluate_and_store(&*value, current_block, var_obj)
            },
            FunctionAstType::Assign{ref ident, ref value} => {
                let var_obj = self.get_name(&ident)?;
                self.evaluate_and_store(&*value, current_block, var_obj)
            },
        }
    }


    pub(crate) fn vm_program(&self, prog: &mut vm::Program, halt_on_return: bool) -> Result<Vec<ItemPath>> {
        let mut dependencies = Vec::new();
        let stack_offsets: Vec<usize> = self.objects.iter()
            .map(|o| o.t.as_ref().unwrap().size())
            .scan(0, |total, size| {*total += size; Some(*total - size)})
            .collect();
        
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
                        });
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
                        });
                    },
                    InstructionType::Add{dest, lhs, rhs} => {
                        instructions.push(vm::Instruction{
                            src: move_arg_for(lhs),
                            dst: vm::MoveArg::ValA,
                        });

                        instructions.push(vm::Instruction{
                            src: move_arg_for(rhs),
                            dst: vm::MoveArg::ValB,
                        });

                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::AddU32Result,
                            dst: move_arg_for(dest),
                        });
                    },
                    InstructionType::Copy{src, dst} => {
                        instructions.push(vm::Instruction{
                            src: move_arg_for(src),
                            dst: move_arg_for(dst),
                        });
                    },
                    InstructionType::Call{ref callee, ref signature, ref args} => {
                        if !args.is_empty() {
                            return Err(Error::new("function arguments not supported yet"));
                        }

                        dependencies.push(callee.clone());

                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::InstructionIdx,
                            dst: vm::MoveArg::ReturnAddressStack,
                        });

                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::FunctionStartIdx(callee.clone()),
                            dst: vm::MoveArg::InstructionIdx,
                        });
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
                        dst: vm::MoveArg::ReturnValue,
                    });

                    if halt_on_return {
                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::Byte(1),
                            dst: vm::MoveArg::Halt,
                        });
                    } else {
                        instructions.push(vm::Instruction{
                            src: vm::MoveArg::ReturnAddressStack,
                            dst: vm::MoveArg::InstructionIdx,
                        });
                    }
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
        prog.instructions.extend(blocks.into_iter().flatten());
        Ok(dependencies)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Constuctor {}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Type {
    U32,
    Bool,
    String,
    Struct(itemise::ItemPath),
    Compound(Constuctor, Vec<Type>),
}

impl Type {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            Type::U32 => false,
            Type::Bool => false,
            Type::String => false,
            Type::Struct(_) => false,
            Type::Compound(_, types) => types.iter().any(|t| t.depends_on(obj)),
        }
    }

    fn do_substitutions(&mut self, graph: &Graph) {
        match self {
            Type::Compound(_, types) => types.into_iter().for_each(|t| t.do_substitutions(graph)),
            _ => {},
        }
    }

    fn size(&self) -> usize {
        match self {
            Type::U32 => 4,
            Type::Bool => 1,
            Type::String => 16,
            Type::Struct(_) => panic!("Struct not supported"),
            Type::Compound(_, _) => panic!("Compound not supported"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum TypeExpression {
    Type(Type),
    Placeholder(ObjectHandle),
}

impl TypeExpression {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            TypeExpression::Type(t) => t.depends_on(obj),
            TypeExpression::Placeholder(other) => obj == *other,
        }
    }

    fn do_substitutions(&mut self, graph: &Graph) {
        match self {
            TypeExpression::Type(t) => t.do_substitutions(graph),
            TypeExpression::Placeholder(obj) => {
                if let Some(ref sub) = graph.object(*obj).t {
                    *self = TypeExpression::Type(sub.clone());
                }
            },
        }
    }
}

struct TypeEquation {
    lhs: TypeExpression,
    rhs: TypeExpression,
}

struct InferenceSystem {
    equations: Vec<TypeEquation>,
}

impl InferenceSystem {
    fn add_eqn(&mut self, lhs: TypeExpression, rhs: TypeExpression) {
        self.equations.push(TypeEquation{lhs, rhs});
    }

    fn add_types(graph: &mut Graph, return_type: Type) -> Result<()> {
        let mut is = InferenceSystem{equations: Vec::new()};
        for block_handle in graph.blocks() {
            let block = graph.block(block_handle);
            for inst in &block.instructions {
                match inst.t {
                    InstructionType::Eq{dest, lhs, rhs} => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::Bool));
                        is.add_eqn(TypeExpression::Placeholder(lhs), TypeExpression::Placeholder(rhs));
                    },
                    InstructionType::StoreU32(dest, _) => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::U32));
                    },
                    InstructionType::StoreString(dest, _) => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::String));
                    },
                    InstructionType::Add{dest, lhs, rhs} => {
                        is.add_eqn(TypeExpression::Placeholder(lhs), TypeExpression::Type(Type::U32));
                        is.add_eqn(TypeExpression::Placeholder(rhs), TypeExpression::Type(Type::U32));
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::U32));
                    },
                    InstructionType::Copy{src, dst} => {
                        is.add_eqn(TypeExpression::Placeholder(src), TypeExpression::Placeholder(dst));
                    },
                    InstructionType::Call{callee: _, ref signature, ref args} => {
                        for (arg, sig) in args.iter().zip(&signature.args) {
                            is.add_eqn(TypeExpression::Placeholder(*arg), TypeExpression::Type(sig.1.clone()));
                        }
                    },
                }
            }

            match &block.jump {
                None => {},
                Some(j) => {
                    match j.t {
                        JumpType::Return(val) => {
                            is.add_eqn(TypeExpression::Placeholder(val), TypeExpression::Type(return_type.clone()));
                        },
                        JumpType::Cond{condition, ..} => {
                            is.add_eqn(TypeExpression::Placeholder(condition), TypeExpression::Type(Type::Bool));
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
                (TypeExpression::Type(Type::Compound(c, args)), TypeExpression::Type(Type::Compound(c2, args2))) => {
                    if c == c2 && args.len() == args2.len() {
                        to_remove.push(idx);
                        for (a1, a2) in args.iter().zip(args2.iter()) {
                            new_equations.push(TypeEquation{
                                lhs: TypeExpression::Type(a1.clone()),
                                rhs: TypeExpression::Type(a2.clone()),
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
                (TypeExpression::Type(Type::Compound(c, args)), TypeExpression::Type(Type::Compound(c2, args2))) => {
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
                (_, TypeExpression::Placeholder(_)) => {
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
                (TypeExpression::Placeholder(_), TypeExpression::Placeholder(_)) => {},
                (TypeExpression::Placeholder(obj), TypeExpression::Type(rhs)) => {
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
                (TypeExpression::Placeholder(obj), rhs) => {
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
                (TypeExpression::Placeholder(_), TypeExpression::Placeholder(_)) => {},
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

        match item.t {
            ItemType::Fn(_) => parse_body(&item.tokens),
            _ => return Err(Error::new("Not a function")),
        }
    }
}

#[derive(Hash, PartialEq, Clone)]
struct GetFunctionReturnType {
    path: ItemPath,
}

impl Query for GetFunctionReturnType {
    type Output = Result<Type>;
}

impl GetFunctionReturnType {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let global_items_arc = make_query!(&prog, GetGlobalItems).await;
        if global_items_arc.is_err() {
            return Err(global_items_arc.as_ref().as_ref().unwrap_err().clone());
        }

        let global_items = global_items_arc.as_ref().as_ref().unwrap();

        let item = global_items.get(&self.path).ok_or(
            Error::new("Could not find function")
        )?;

        match item.t {
            ItemType::Fn(ref signature) => {
                Ok(signature.return_type.as_ref().ok_or(Error::new("Function has unknown return type"))?.clone())
            },
            _ => return Err(Error::new("Not a function")),
        }
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
        let function_return_type_fut = make_query!(&prog, GetFunctionReturnType{path: self.path.clone()});
        let function_ast_fut = make_query!(&prog, GetFunctionAst{path: self.path.clone()});
        let global_items_fut = make_query!(&prog, GetGlobalItems);

        let (function_return_type_arc, function_ast_arc, global_items_arc) = join!(function_return_type_fut, function_ast_fut, global_items_fut);

        if function_return_type_arc.is_err() {
            return Err(function_return_type_arc.as_ref().as_ref().unwrap_err().clone());
        }
        let function_return_type = function_return_type_arc.as_ref().as_ref().unwrap();

        if function_ast_arc.is_err() {
            return Err(function_ast_arc.as_ref().as_ref().unwrap_err().clone());
        }

        if global_items_arc.is_err() {
            return Err(global_items_arc.as_ref().as_ref().unwrap_err().clone());
        }
        let global_items = global_items_arc.as_ref().as_ref().unwrap();

        let function_ast = function_ast_arc.as_ref().as_ref().unwrap();
        Graph::from_function_ast(function_ast, function_return_type.clone(), global_items)
    }
}


#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetFunctionVmProgram {
    pub(crate) path: ItemPath,
}

impl Query for GetFunctionVmProgram {
    type Output = Result<vm::Program>;
}

impl GetFunctionVmProgram {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let mut vm_prog = vm::Program::new();    
        let mut deps = vec![self.path];
        let mut halt_on_return = true;

        while !deps.is_empty() {
            deps.retain(|path| !vm_prog.functions.contains_key(path));

            let graph_futures = 
                deps.iter()
                .map(|path| make_query!(&prog, GetFunctionGraph{path: path.clone()}));

            let graph_arcs = join_all(graph_futures).await;

            let mut new_deps = Vec::new();
            for (arc, path) in graph_arcs.iter().zip(&deps) {
                if arc.is_err() {
                    return Err(arc.as_ref().as_ref().unwrap_err().clone());
                }

                vm_prog.functions.insert(path.clone(), vm_prog.instructions.len() as u64);
                new_deps.extend(arc.as_ref().as_ref().unwrap().vm_program(&mut vm_prog, halt_on_return)?);
                halt_on_return = false;
            }
            deps = new_deps;
        }
        Ok(vm_prog)
    }
}
