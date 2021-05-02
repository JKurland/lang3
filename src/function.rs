use crate::itemise::{self, GetGlobalItems, ItemPath, ItemType, Item};
use crate::lex::{Token, TokenType};
use crate::{Result, Error, Query, Program, make_query};
use std::{collections::HashMap, sync::Arc};
use futures::join;
use itemise::FunctionSignature;
use crate::storage::Handle;

// AST
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
    // tokens includes the item prefix and closing brace, e.g. "loop {" and "}"
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
        contains_break: bool,
    },
    Scope {
        items: Vec<FunctionAst>,
        ending_semicolon: bool,
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
    Comma {
        lhs: Box<FunctionAst>,
        rhs: Option<Box<FunctionAst>>,
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
    for (idx, token) in token_stream.iter().chain(std::iter::once(&Token{t: TokenType::EOF})).enumerate() {
        let new_state = match state {
            State::NewItem => {
                item_start_idx = idx;
                match token.t {
                    TokenType::EOF => state,
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
                    TokenType::SemiColon | TokenType::EOF => State::FinishedItem(FunctionItemType::Expr),
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
                    TokenType::SemiColon | TokenType::EOF => State::FinishedItem(FunctionItemType::Break),
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
                    TokenType::SemiColon | TokenType::EOF => State::FinishedItem(FunctionItemType::Return),
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
            let end_idx = if idx == token_stream.len() {
                idx
            } else {
                idx + 1
            };

            items.push(FunctionItem {
                t: item_type,
                tokens: &token_stream[item_start_idx..end_idx]
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
    let ending_semicolon = match token_stream.last() {
        None => false,
        Some(token) => token.t == TokenType::SemiColon,
    };
    Ok(FunctionAst{t: FunctionAstType::Scope{
        items: items
                .iter()
                .map(parse_item)
                .collect::<Result<Vec<FunctionAst>>>()?,
        ending_semicolon,
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

    let contains_break = loop_body_tokens.iter().find(|t| t.t == TokenType::Break).is_some();

    Ok(FunctionAst {
        t: FunctionAstType::Loop{body: Box::new(parse_body(loop_body_tokens)?), contains_break}
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
    Comma,
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
            OperatorType::Comma => {
                let rhs = if root_op.idx == right_end {
                    None
                } else {
                    Some(Box::new(parse_expr_inner(&operators[root_op_idx + 1..], token_stream, root_op.idx + 1, right_end)?))
                };

                Ok(FunctionAst{t: FunctionAstType::Comma{
                    lhs: Box::new(parse_expr_inner(&operators[..root_op_idx], token_stream, left_end, root_op.idx - 1)?),
                    rhs,
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

    #[derive(Debug)]
    enum State {
        TopLevel,
        InParens{open_idx: usize, depth: usize},
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
                    TokenType::Comma => {
                        operators.push(Operator{
                            t: OperatorType::Comma,
                            idx,
                            priority: -10,
                        });
                        state
                    },
                    TokenType::OpenParen => State::InParens{open_idx: idx, depth: 1},
                    _ => state,
                }
            },
            State::InParens{open_idx, depth} => {
                match token.t {
                    TokenType::CloseParen => {
                        if depth == 1 {
                            operators.push(Operator{
                                t: OperatorType::ParenGroup(open_idx),
                                idx,
                                priority: 100,
                            });
                            State::TopLevel
                        } else {
                            State::InParens{open_idx, depth: depth - 1}        
                        }
                    },
                    TokenType::OpenParen => State::InParens{open_idx, depth: depth + 1},
                    _ => state,
                }
            },
        };
        state = new_state;
    }

    if let State::InParens{..} = state {
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





// Control Flow Graph
#[derive(Debug)]
pub(crate) enum InstructionType {
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
    StoreNull(ObjectHandle),
    SetType(ObjectHandle, Type),
    Copy{
        src: ObjectHandle,
        dst: ObjectHandle,
    },
    Call{
        dst: ObjectHandle,
        callee: ItemPath,
        signature: FunctionSignature,
        args: Vec<ObjectHandle>,
    },
    Return{
        src: ObjectHandle,
    },
}

#[derive(Debug)]
pub(crate) struct Instruction {
    pub(crate) t: InstructionType
}

#[derive(Debug)]
pub(crate) enum JumpType {
    Uncond(BlockHandle),
    Cond{
        condition: ObjectHandle,
        true_: BlockHandle,
        false_: BlockHandle,
    },
    Return,
}

#[derive(Debug)]
pub(crate) struct Jump {
    pub(crate) t: JumpType,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ObjectSource {
    Argument(usize),
    Local,
}

#[derive(Debug)]
pub(crate) struct Object {
    pub(crate) t: Option<Type>,
    pub(crate) source: ObjectSource,
}

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) jump: Option<Jump>,
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
pub(crate) struct ObjectHandle (usize);

impl Handle for ObjectHandle {
    fn index(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct BlockHandle (usize);

impl Handle for BlockHandle {
    fn index(&self) -> usize {
        self.0
    }
}

pub(crate) struct BlockHandleIter {
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

pub(crate) struct ObjectHandleIter {
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
    never_objects: Vec<ObjectHandle>,
}

#[derive(Clone)]
struct Labels {
    break_: Option<BlockHandle>,
}

#[derive(Clone)]
struct GraphBuildCtx<'a> {
    labels: Labels,
    global_items: &'a HashMap<ItemPath, Item>,
    return_object: ObjectHandle,
}

impl Graph {
    fn new() -> Self {
        Self {
            objects: Vec::new(),
            blocks: vec![Block::new()],
            names: vec![HashMap::new()],
            never_objects: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.names.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.names.pop();
    }
    fn add_name(&mut self, name: &str, source: ObjectSource) -> Result<ObjectHandle> {
        let new_object = self.new_object(source);
        let old_value = self.names.last_mut().unwrap().insert(name.to_string(), new_object);
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
    pub(crate) fn block(&self, handle: BlockHandle) -> &Block {
        self.blocks.get(handle.0).unwrap()
    }
    fn entry_block(&self) -> BlockHandle {
        BlockHandle(0)
    }
    pub(crate) fn blocks(&self) -> BlockHandleIter {
        BlockHandleIter{n: self.blocks.len(), i: 0}
    }
    fn new_block(&mut self) -> BlockHandle {
        self.blocks.push(Block::new());
        BlockHandle(self.blocks.len() - 1)
    }
    pub(crate) fn num_blocks(&self) -> usize {
        self.blocks.len()
    }


    pub(crate) fn objects(&self) -> ObjectHandleIter {
        ObjectHandleIter{n: self.objects.len(), i: 0}
    }
    pub(crate) fn object(&self, handle: ObjectHandle) -> &Object {
        self.objects.get(handle.0).unwrap()
    }
    fn object_mut(&mut self, handle: ObjectHandle) -> &mut Object {
        self.objects.get_mut(handle.0).unwrap()
    }
    fn num_objects(&self) -> usize {
        return self.objects.len();
    }
    fn new_object(&mut self, source: ObjectSource) -> ObjectHandle {
        self.objects.push(Object{t: None, source});
        ObjectHandle(self.objects.len() - 1)
    }

    pub(crate) fn from_function_ast(ast: &FunctionAst, signature: FunctionSignature, global_items: &HashMap<ItemPath, Item>) -> Result<Self> {
        let return_type = signature.return_type;
        let mut graph = Self::new();
        let return_object = graph.new_object(ObjectSource::Local);
    
        let ctx = GraphBuildCtx{
            labels: Labels{break_: None},
            global_items,
            return_object,
        };

        for (idx, arg) in signature.args.iter().enumerate() {
            let arg_handle = graph.add_name(&arg.0, ObjectSource::Argument(idx))?;
            graph.block_mut(graph.entry_block()).instructions.push(Instruction{t: InstructionType::SetType(arg_handle, arg.1.clone())});
        }
        graph.push_scope();

        let final_block = graph.evaluate_and_store(ast, graph.entry_block(), Some(return_object), &ctx)?;

        if let Some(handle) = final_block {
            let block = graph.block_mut(handle);
            if block.jump.is_none() {
                block.instructions.push(Instruction{t: InstructionType::Return{src: return_object}});
                block.jump = Some(Jump{t: JumpType::Return});
            }
        }

        InferenceSystem::add_types(&mut graph, return_type, return_object)?;
        for handle in graph.objects() {
            let object = graph.object(handle);
            if object.t.is_none() {
                return Err(Error::new("Couldn't infer all types"));
            }
        }

        Ok(graph)
    }

    fn evaluate_and_store(&mut self, ast: &FunctionAst, current_block: BlockHandle, object_handle: Option<ObjectHandle>, ctx: &GraphBuildCtx) -> Result<Option<BlockHandle>> {
        let unwrap_block = |b: Option<BlockHandle>| b.ok_or(Error::new("Unreachable Statement"));

        match ast.t {
            FunctionAstType::IfStatement{ref condition, ref true_, ref false_ } => {
                let cond_obj_handle = self.new_object(ObjectSource::Local);
                let after_eval_block = self.evaluate_and_store(&*condition, current_block, Some(cond_obj_handle), ctx)?;

                let true_block = self.new_block();
                let false_block = self.new_block();
                
                self.block_mut(unwrap_block(after_eval_block)?).jump = Some(Jump{t: JumpType::Cond{
                    condition: cond_obj_handle,
                    true_: true_block,
                    false_: false_block,
                }});

                let final_true_block = self.evaluate_and_store(
                    &*true_,
                    true_block,
                    object_handle,
                    ctx,
                )?;

                let final_false_block = self.evaluate_and_store(
                    &*false_,
                    false_block,
                    object_handle,
                    ctx,
                )?;

                let after_block = self.new_block();
                let mut both_never = true;

                for final_block in &[final_true_block, final_false_block] {
                    match final_block {
                        Some(block) => {
                            both_never = false;
                            if self.block(*block).jump.is_none() {
                                self.block_mut(*block).jump = Some(Jump{t: JumpType::Uncond(after_block)});
                            }
                        },
                        None => {
                            if let Some(obj) = object_handle {
                                self.never_objects.push(obj)
                            }
                        }
                    }
                }

                if both_never {
                    Ok(None)
                } else {
                    Ok(Some(after_block))
                }
            },
            FunctionAstType::Loop{ref body, ref contains_break} => {
                let body_block = self.new_block();
                self.block_mut(current_block).jump = Some(Jump{t: JumpType::Uncond(body_block)});

                let after_block = self.new_block();

                let mut loop_ctx = ctx.clone();
                loop_ctx.labels.break_ = Some(after_block);

                let final_block = self.evaluate_and_store(&*body, body_block, None, &loop_ctx)?;
                self.block_mut(unwrap_block(final_block)?).jump = Some(Jump{t: JumpType::Uncond(body_block)});

                if let Some(obj) = object_handle {
                    if *contains_break {
                        self.block_mut(after_block).instructions.push(Instruction{t: InstructionType::StoreNull(obj)});
                    } else {
                        self.never_objects.push(obj);
                    }
                }

                Ok(Some(after_block))
            },
            FunctionAstType::Break => {
                self.block_mut(current_block).jump = Some(Jump{
                    t: JumpType::Uncond(ctx.labels.break_.ok_or(Error::new("Break must be inside loop"))?)
                });
                Ok(None)
            },
            FunctionAstType::Scope{ref items, ending_semicolon} => {
                let mut block_handle = Some(current_block);

                if items.len() == 0 {
                    if let Some(obj) = object_handle {
                        self.block_mut(current_block).instructions.push(Instruction{t: InstructionType::StoreNull(obj)});
                    }
                    return Ok(block_handle);
                }

                self.push_scope();
                for (idx, item) in items.iter().enumerate() {
                    let mut obj = None;
                    if !ending_semicolon && idx == items.len() - 1 {
                        obj = object_handle;
                    }
                    block_handle = self.evaluate_and_store(item, unwrap_block(block_handle)?, obj, ctx)?;
                }

                if let None = block_handle {

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
                        let val = self.new_object(ObjectSource::Local);
                        match inside {
                            None => Err(Error::new("Expected something inside parens")),
                            Some(inside) => self.evaluate_and_store(&**inside, current_block, Some(val), ctx),
                        }
                    },
                    Some(lhs) => {
                        
                        let callee_path;
                        if let FunctionAstType::VarName(ref callee_name) = lhs.t {
                            callee_path = ItemPath::new(callee_name);
                        } else {
                            return Err(Error::new("Expected name before parens"))
                        }
                        
                        let mut block = current_block;
                        let mut args: Vec<ObjectHandle> = vec![];
                        if let Some(top_ast) = inside {
                            let mut ast= top_ast;
                            loop {
                                match ast.t {
                                    FunctionAstType::Comma{ref lhs, ref rhs} => {
                                        let object_handle = self.new_object(ObjectSource::Local);
                                        block = self.evaluate_and_store(lhs, block, Some(object_handle), ctx)?.ok_or(Error::new("Function arg doesn't evaluate"))?;
                                        args.push(object_handle);

                                        if let Some(ref rhs_ast) = rhs {
                                            ast = rhs_ast;
                                        } else {
                                            break;
                                        }
                                    },
                                    _ => {
                                        let object_handle = self.new_object(ObjectSource::Local);
                                        block = self.evaluate_and_store(ast, block, Some(object_handle), ctx)?.ok_or(Error::new("Function arg doesn't evaluate"))?;
                                        args.push(object_handle);
                                        break;
                                    }
                                };
                            }
                        }

                        let callee_item = ctx.global_items.get(&callee_path).ok_or(Error::new("Unknown item"))?;

                        if let ItemType::Fn(ref signature) = callee_item.t {
                            if signature.args.len() != args.len() {
                                return Err(Error::new("Wrong number of args for function"));
                            }

                            let t = InstructionType::Call{
                                dst: object_handle.unwrap_or_else(|| self.new_object(ObjectSource::Local)),
                                callee: callee_path,
                                signature: signature.clone(),
                                args,
                            };

                            self.block_mut(block).instructions.push(Instruction{t});

                            Ok(Some(block))
                        } else {
                            Err(Error::new("Can only call functions"))
                        }
                    },
                }
            },
            FunctionAstType::Comma{..} => {
                Err(Error::new("Cannot use comma operator outside of function arguments"))
            },
            FunctionAstType::U32(i) => {
                if let Some(obj) = object_handle {
                    let block = self.block_mut(current_block);
                    block.instructions.push(Instruction{t: InstructionType::StoreU32(obj, i)});
                }
                Ok(Some(current_block))
            },
            FunctionAstType::StringLiteral(ref s) => {
                if let Some(obj) = object_handle {
                    let block = self.block_mut(current_block);
                    block.instructions.push(Instruction{t: InstructionType::StoreString(obj, s.clone())});
                }
                Ok(Some(current_block))
            },
            FunctionAstType::Add{ref lhs, ref rhs} => {
                let lhs_obj = self.new_object(ObjectSource::Local);
                let mut cur_block = current_block;
                cur_block = unwrap_block(self.evaluate_and_store(&*lhs, cur_block, Some(lhs_obj), ctx)?)?;

                let rhs_obj = self.new_object(ObjectSource::Local);
                cur_block = unwrap_block(self.evaluate_and_store(&*rhs, cur_block, Some(rhs_obj), ctx)?)?;

                let obj = match object_handle {
                    Some(obj) => obj,
                    None => self.new_object(ObjectSource::Local),
                };
                let block = self.block_mut(cur_block);

                block.instructions.push(Instruction{t: InstructionType::Add{
                    dest: obj,
                    lhs: lhs_obj,
                    rhs: rhs_obj,
                }});
                Ok(Some(cur_block))
            },
            FunctionAstType::DoubleEq{ref lhs, ref rhs} => {
                let lhs_obj = self.new_object(ObjectSource::Local);
                let mut cur_block = current_block;
                cur_block = unwrap_block(self.evaluate_and_store(&*lhs, cur_block, Some(lhs_obj), ctx)?)?;

                let rhs_obj = self.new_object(ObjectSource::Local);
                cur_block = unwrap_block(self.evaluate_and_store(&*rhs, cur_block, Some(rhs_obj), ctx)?)?;

                let obj = match object_handle {
                    Some(obj) => obj,
                    None => self.new_object(ObjectSource::Local),
                };
                let block = self.block_mut(cur_block);

                block.instructions.push(Instruction{t: InstructionType::Eq{
                    dest: obj,
                    lhs: lhs_obj,
                    rhs: rhs_obj,
                }});
                Ok(Some(cur_block))
            },
            FunctionAstType::VarName(ref s) => {
                let src = self.get_name(s)?;
                let block = self.block_mut(current_block);
                if let Some(obj) = object_handle {
                    block.instructions.push(Instruction{t: InstructionType::Copy{src, dst: obj}});
                }
                Ok(Some(current_block))
            },
            FunctionAstType::Return(ref value) => {
                let after_eval_block = unwrap_block(self.evaluate_and_store(&*value, current_block, Some(ctx.return_object), ctx)?)?;
                let block = self.block_mut(after_eval_block);
                block.jump = Some(Jump{t: JumpType::Return});
                block.instructions.push(Instruction{t: InstructionType::Return{src: ctx.return_object}});

                if let Some(obj) = object_handle {
                    self.never_objects.push(obj);
                }
                Ok(None)
            },
            FunctionAstType::Let{ref ident, t: _, ref value} => {
                if let Some(obj) = object_handle {
                    self.block_mut(current_block).instructions.push(Instruction{t: InstructionType::StoreNull(obj)});
                }
                let var_obj = self.add_name(&ident, ObjectSource::Local)?;
                self.evaluate_and_store(&*value, current_block, Some(var_obj), ctx)
            },
            FunctionAstType::Assign{ref ident, ref value} => {
                if let Some(obj) = object_handle {
                    self.block_mut(current_block).instructions.push(Instruction{t: InstructionType::StoreNull(obj)});
                }
                let var_obj = self.get_name(&ident)?;
                self.evaluate_and_store(&*value, current_block, Some(var_obj), ctx)
            },
        }
    }
}




// Type System
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Constuctor {}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Type {
    U32,
    Bool,
    String,
    Never,
    Null,
    Struct(ItemPath),
    Compound(Constuctor, Vec<Type>),
}

impl Type {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            Type::U32 => false,
            Type::Bool => false,
            Type::String => false,
            Type::Never => false,
            Type::Null => false,
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

#[derive(Debug)]
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

    fn add_types(graph: &mut Graph, return_type: Type, return_object: ObjectHandle) -> Result<()> {
        let mut is = InferenceSystem{equations: Vec::new()};
        is.add_eqn(TypeExpression::Placeholder(return_object), TypeExpression::Type(return_type.clone()));
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
                    InstructionType::StoreNull(dest) => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::Null));
                    },
                    InstructionType::SetType(ref obj, ref t) => {
                        is.add_eqn(TypeExpression::Placeholder(*obj), TypeExpression::Type(t.clone()));
                    },
                    InstructionType::Add{dest, lhs, rhs} => {
                        is.add_eqn(TypeExpression::Placeholder(lhs), TypeExpression::Type(Type::U32));
                        is.add_eqn(TypeExpression::Placeholder(rhs), TypeExpression::Type(Type::U32));
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::U32));
                    },
                    InstructionType::Copy{src, dst} => {
                        is.add_eqn(TypeExpression::Placeholder(src), TypeExpression::Placeholder(dst));
                    },
                    InstructionType::Call{ref dst, callee: _, ref signature, ref args} => {
                        is.add_eqn(TypeExpression::Placeholder(*dst), TypeExpression::Type(signature.return_type.clone()));
                        for (arg, sig) in args.iter().zip(&signature.args) {
                            is.add_eqn(TypeExpression::Placeholder(*arg), TypeExpression::Type(sig.1.clone()));
                        }
                    },
                    InstructionType::Return{src} => {
                        is.add_eqn(TypeExpression::Placeholder(src), TypeExpression::Type(return_type.clone()));
                    },
                }
            }

            match &block.jump {
                None => {},
                Some(j) => {
                    match j.t {
                        JumpType::Return => {},
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
        Self::fill_in_nevers(graph);
        Ok(())
    }

    fn fill_in_nevers(graph: &mut Graph) {
        for handle in graph.never_objects.clone() {
            let obj = graph.object_mut(handle);
            if obj.t.is_none() {
                obj.t = Some(Type::Never);
            } 
        }
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
                (TypeExpression::Type(a), TypeExpression::Type(b)) => {
                    if a != b {
                        return Err(Error::new("Type error2"));
                    }
                }
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




// Queries
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
struct GetFunctionSignature {
    path: ItemPath,
}

impl Query for GetFunctionSignature {
    type Output = Result<FunctionSignature>;
}

impl GetFunctionSignature {
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
                Ok(signature.clone())
            },
            _ => return Err(Error::new("Not a function")),
        }
    }
}

#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetFunctionGraph {
    pub(crate) path: ItemPath,
}

impl Query for GetFunctionGraph {
    type Output = Result<Graph>;
}

impl GetFunctionGraph {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let function_signature_fut = make_query!(&prog, GetFunctionSignature{path: self.path.clone()});
        let function_ast_fut = make_query!(&prog, GetFunctionAst{path: self.path.clone()});
        let global_items_fut = make_query!(&prog, GetGlobalItems);

        let (function_signature_arc, function_ast_arc, global_items_arc) = join!(function_signature_fut, function_ast_fut, global_items_fut);

        if function_signature_arc.is_err() {
            return Err(function_signature_arc.as_ref().as_ref().unwrap_err().clone());
        }
        let function_signature = function_signature_arc.as_ref().as_ref().unwrap();

        if function_ast_arc.is_err() {
            return Err(function_ast_arc.as_ref().as_ref().unwrap_err().clone());
        }

        if global_items_arc.is_err() {
            return Err(global_items_arc.as_ref().as_ref().unwrap_err().clone());
        }
        let global_items = global_items_arc.as_ref().as_ref().unwrap();

        let function_ast = function_ast_arc.as_ref().as_ref().unwrap();
        Graph::from_function_ast(function_ast, function_signature.clone(), global_items)
    }
}
