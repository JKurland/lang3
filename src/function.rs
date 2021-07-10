use crate::inference::{InferenceSystem, Type, TypeExpression};
use crate::itemise::{self, GetGlobalItems, ItemPath, ItemType};
use crate::lex::{Token, TokenType};
use crate::{Result, Error, Query, Program, make_query};
use std::{collections::HashMap, sync::Arc};
use futures::future::{BoxFuture, FutureExt};
use futures::try_join;
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
    StructFieldAssign {
        ident: String,
        field_spec: Vec<String>,
        value: Box<FunctionAst>,
    },
    Add {
        lhs: Box<FunctionAst>,
        rhs: Box<FunctionAst>,
    },
    Dot {
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
    BraceGroup {
        lhs: Option<Box<FunctionAst>>,
        inside: Option<Box<FunctionAst>>,
        rhs: Option<Box<FunctionAst>>,
    },
    Break,
    StructInit {
        fields: Vec<Box<FunctionAst>>,
    },
    StructFieldInit {
        field_name: String,
        value: Box<FunctionAst>,
    },
}

#[derive(Debug)]
pub(crate) struct FunctionAst {
    t: FunctionAstType,
    
}

// returns None if tokens doesn't start with an assign, otherwise returns Some(len)
// where len is the number of tokens on the rhs of the equal +1 for the equal
fn starts_with_assign(tokens: &[Token]) -> Option<usize> {
    for (idx, token) in tokens.iter().enumerate() {
        match token.t {
            TokenType::Equal => {
                return Some(idx + 1);
            },
            TokenType::Ident(_) => {},
            TokenType::Dot => {},
            _ => {return None;}
        }
    }
    return None;
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

                if let Some(to_skip) = starts_with_assign(&token_stream[idx..]) {
                    State::Skip(to_skip, Box::new(State::Assign))
                } else {
                    match token.t {
                        TokenType::EOF => state,
                        TokenType::If => State::IfSignature,
                        TokenType::Loop => State::LoopSignature,
                        TokenType::OpenBrace => State::Body(FunctionItemType::Scope),
                        TokenType::Return => State::Return,
                        TokenType::Let => State::LetBeforeEq,
                        TokenType::Break => State::Break,
                        TokenType::Ident(_) => State::Expr,
                        _ => State::Expr,
                    }
                }
            },
            State::IfSignature => {
                match token.t {
                    TokenType::OpenBrace => State::IfBody,
                    TokenType::CloseBrace => return Err(Error::SyntaxErrorUnexpected(vec!["}"])),
                    _ => State::IfSignature,
                }
            },
            State::LoopSignature => {
                match token.t {
                    TokenType::OpenBrace => State::Body(FunctionItemType::Loop),
                    _ => return Err(Error::SyntaxErrorExpected(vec!["{"])),
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
                    _ => return Err(Error::SyntaxErrorExpected(vec![";"])),
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
                    _ => return Err(Error::SyntaxErrorExpected(vec!["if", "{"])),
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
        _ => Err(Error::UnexpectedEndOfFunction),
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
        return Err(Error::SyntaxErrorExpected(vec!["{"]));
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
        _ => return Err(Error::SyntaxErrorExpected(vec!["identifier"])),
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
        None => Err(Error::SyntaxErrorExpected(vec!["="])),
    }
}

fn parse_assign(token_stream: &[Token]) -> Result<FunctionAst> {
    let equal_pos = token_stream.iter().position(|t| t.t == TokenType::Equal).expect("Got invalid assign, no =");
    let rhs = &token_stream[..equal_pos];

    #[derive(PartialEq)]
    enum State {
        ExpectingFirstIdent,
        ExpectingIdent,
        AfterIdent,
    }

    let mut state = State::ExpectingFirstIdent;
    let mut ident = None;
    let mut field_spec = Vec::new();
    for token in rhs {
        let new_state = match state {
            State::ExpectingFirstIdent => {
                match token.t {
                    TokenType::Ident(ref s) => {
                        ident = Some(s.clone());
                        State::AfterIdent
                    },
                    _ => return Err(Error::SyntaxErrorExpected(vec!["identifier"]))
                }
            },
            State::ExpectingIdent => {
                match token.t {
                    TokenType::Ident(ref s) => {
                        field_spec.push(s.clone());
                        State::AfterIdent
                    },
                    _ => return Err(Error::SyntaxErrorExpected(vec!["identifier"]))
                }
            },
            State::AfterIdent => {
                match token.t {
                    TokenType::Dot => State::ExpectingIdent,
                    _ => return Err(Error::SyntaxErrorExpected(vec![".", "="]))
                }
            }
        };
        state = new_state;
    }

    if state != State::AfterIdent {
        return Err(Error::SyntaxErrorExpected(vec!["identifier"]));
    }

    if let None = ident {
        panic!("No idents in AfterIdent state");
    }

    if field_spec.is_empty() {
        Ok(FunctionAst {
            t: FunctionAstType::Assign{
                ident: ident.unwrap(),
                value: Box::new(parse_expr(&token_stream[equal_pos+1..])?)
            }
        })
    } else {
        Ok(FunctionAst {
            t: FunctionAstType::StructFieldAssign{
                ident: ident.unwrap(),
                field_spec: field_spec,
                value: Box::new(parse_expr(&token_stream[equal_pos+1..])?)
            }
        })
    }
}

fn parse_if(token_stream: &[Token]) -> Result<FunctionAst> {
    assert_eq!(token_stream.get(0), Some(&Token{t: TokenType::If}));
    assert_eq!(token_stream.last(), Some(&Token{t: TokenType::CloseBrace}));

    let open_brace_pos = 
        token_stream
            .iter()
            .position(|t| t.t == TokenType::OpenBrace).unwrap();

    if open_brace_pos == 1 {
        return Err(Error::SyntaxErrorExpected(vec!["condition"]));
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
                return Err(Error::SyntaxErrorExpected(vec!["if", "{"]));
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
    Dot,
    ParenGroup(usize),
    BraceGroup(usize),
}

#[derive(Debug)]
enum Associativity {
    Left,
    Right,
}

impl Associativity {
    fn idx_to_priority(&self, idx: usize) -> i32 {
        match self {
            Associativity::Left => -(idx as i32),
            Associativity::Right => idx as i32,
        }
    }
}


#[derive(Debug)]
struct Operator {
    t: OperatorType,
    idx: usize,
    priority: i32,
    associativity: Associativity,
}


fn parse_expr_inner(operators: &[Operator], token_stream: &[Token], left_end: usize, right_end: usize) -> Result<FunctionAst> {
    if let Some((root_op_idx, root_op)) = operators.iter().enumerate().min_by_key(|o| (o.1.priority, o.1.associativity.idx_to_priority(o.0))) {
        match root_op.t {
            OperatorType::Add => {
                Ok(FunctionAst{t: FunctionAstType::Add{
                    lhs: Box::new(parse_expr_inner(&operators[..root_op_idx], token_stream, left_end, root_op.idx - 1)?),
                    rhs: Box::new(parse_expr_inner(&operators[root_op_idx + 1..], token_stream, root_op.idx + 1, right_end)?),
                }})
            },
            OperatorType::Dot => {
                Ok(FunctionAst{t: FunctionAstType::Dot{
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
                // Since the trinary operators (parens, braces) have a higher priority than al binary or unary operators
                // we don't need to worry about the lhs or rhs being incomplete expressions. We do need to worry about
                // the lhs or rhs being empty though.
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
            OperatorType::BraceGroup(open_idx) => {
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
                    // atm in an expression the only thing
                    Some(Box::new(parse_struct_init(&token_stream[open_idx+1..root_op.idx])?))
                };

                Ok(FunctionAst{t: FunctionAstType::BraceGroup{lhs, rhs, inside}})
            },
        }
    } else {
        if left_end != right_end {
            Err(Error::SyntaxErrorExpected(vec!["literal"]))
        } else {
            match token_stream[left_end].t {
                TokenType::Int(ref s) => Ok(FunctionAst{t: FunctionAstType::U32(parse_u32(&s))}),
                TokenType::String(ref s) => Ok(FunctionAst{t: FunctionAstType::StringLiteral(s.clone())}),
                TokenType::Ident(ref s) => Ok(FunctionAst{t: FunctionAstType::VarName(s.clone())}),
                _ => Err(Error::SyntaxErrorExpected(vec!["literal"])),
            }
        }
    }
}

fn parse_struct_init(token_stream: &[Token]) -> Result<FunctionAst> {
    #[derive(Debug, PartialEq)]
    enum GroupType {
        Paren,
        Brace,
    }

    #[derive(Debug)]
    enum State {
        ExpectingFieldName,
        ExpectingColon(String),
        ExprTopLevel{expr_start: usize, field_name: String},
        InGroup{expr_start: usize, field_name: String, depth: usize, group_type: GroupType},
    }

    let mut fields = Vec::new();
    let mut state = State::ExpectingFieldName;

    for (idx, token) in token_stream.iter().enumerate() {
        let new_state = match state {
            State::ExpectingFieldName => {
                match token.t {
                    TokenType::Ident(ref field_name) => {
                        State::ExpectingColon(field_name.clone())
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(vec!["field name"]));
                    }
                }
            },
            State::ExpectingColon(field_name) => {
                match token.t {
                    TokenType::Colon => {
                        State::ExprTopLevel{expr_start: idx + 1, field_name}
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(vec![":"]));
                    }
                }
            },
            State::ExprTopLevel{expr_start, field_name} => {
                match token.t {
                    TokenType::Comma => {
                        fields.push(Box::new(FunctionAst{t: FunctionAstType::StructFieldInit{
                            field_name: field_name,
                            value: Box::new(parse_expr(&token_stream[expr_start..idx])?),
                        }}));
                        State::ExpectingFieldName
                    },
                    TokenType::OpenBrace => {
                        State::InGroup{expr_start, field_name, depth: 1, group_type: GroupType::Brace}
                    },
                    TokenType::OpenParen => {
                        State::InGroup{expr_start, field_name, depth: 1, group_type: GroupType::Paren}
                    },
                    _ => State::ExprTopLevel{expr_start, field_name}
                }
            },
            State::InGroup{expr_start, field_name, depth, group_type} => {
                let new_depth = match token.t {
                    TokenType::OpenBrace if group_type == GroupType::Brace => {
                        depth + 1
                    },
                    TokenType::CloseBrace if group_type == GroupType::Brace => {
                        depth - 1
                    },
                    TokenType::OpenParen if group_type == GroupType::Paren => {
                        depth + 1
                    },
                    TokenType::CloseParen if group_type == GroupType::Paren => {
                        depth - 1
                    },
                    _ => depth,
                };

                if new_depth == 0 {
                    State::ExprTopLevel{expr_start, field_name}
                } else {
                    State::InGroup{expr_start, field_name, depth: new_depth, group_type}
                }
            },
        };
        state = new_state;
    }
    
    if let State::ExprTopLevel{expr_start, field_name} = state {
        fields.push(Box::new(FunctionAst{t: FunctionAstType::StructFieldInit{
            field_name: field_name,
            value: Box::new(parse_expr(&token_stream[expr_start..])?),
        }}));
    }

    Ok(FunctionAst{t: FunctionAstType::StructInit{fields}})
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

    #[derive(Debug, PartialEq, Clone, Copy)]
    enum GroupType {
        Paren,
        Brace,
    }

    let group_close = |g| match g {
        GroupType::Paren => ")",
        GroupType::Brace => "}",
    };

    #[derive(Debug)]
    enum State {
        TopLevel,
        InGroup{open_idx: usize, depth: usize, group_type: GroupType},
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
                            associativity: Associativity::Right,
                        });
                        state
                    },
                    TokenType::DoubleEq => {
                        operators.push(Operator{
                            t: OperatorType::DoubleEq,
                            idx,
                            priority: 0,
                            associativity: Associativity::Right,
                        });
                        state
                    },
                    TokenType::Comma => {
                        operators.push(Operator{
                            t: OperatorType::Comma,
                            idx,
                            priority: -10,
                            associativity: Associativity::Right,
                        });
                        state
                    },
                    TokenType::Dot => {
                        operators.push(Operator{
                            t: OperatorType::Dot,
                            idx,
                            priority: 20,
                            associativity: Associativity::Left,
                        });
                        state
                    },
                    TokenType::OpenParen => State::InGroup{open_idx: idx, depth: 1, group_type: GroupType::Paren},
                    TokenType::OpenBrace => State::InGroup{open_idx: idx, depth: 1, group_type: GroupType::Brace},
                    _ => state,
                }
            },
            State::InGroup{open_idx, depth, group_type} => {
                match token.t {
                    TokenType::CloseParen if group_type == GroupType::Paren => {
                        if depth == 1 {
                            operators.push(Operator{
                                t: OperatorType::ParenGroup(open_idx),
                                idx,
                                priority: 100,
                                associativity: Associativity::Right,
                            });
                            State::TopLevel
                        } else {
                            State::InGroup{open_idx, depth: depth - 1, group_type}
                        }
                    },
                    TokenType::OpenParen if group_type == GroupType::Paren => State::InGroup{open_idx, depth: depth + 1, group_type},
                    TokenType::CloseBrace if group_type == GroupType::Brace => {
                        if depth == 1 {
                            operators.push(Operator{
                                t: OperatorType::BraceGroup(open_idx),
                                idx,
                                priority: 100,
                                associativity: Associativity::Right,
                            });
                            State::TopLevel
                        } else {
                            State::InGroup{open_idx, depth: depth - 1, group_type}
                        }
                    },
                    TokenType::OpenBrace if group_type == GroupType::Brace => State::InGroup{open_idx, depth: depth + 1, group_type},
                    _ => state,
                }
            },
        };
        state = new_state;
    }

    if let State::InGroup{group_type, ..} = state {
        return Err(Error::SyntaxErrorExpected(vec![group_close(group_type)]));
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
    StructFieldAssignment{
        parent_object: ObjectHandle,
        field_spec: Vec<String>,
        value: ObjectHandle,
    },
    StructFieldAccess{
        parent_object: ObjectHandle,
        field: String,
        dest: ObjectHandle,
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
    t: Option<Type>,
    source: ObjectSource,
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
struct GraphBuildCtx {
    labels: Labels,
    prog: Arc<Program>,
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
            return Err(Error::NameRedefined(name.to_string()));
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
        Err(Error::UnknownName(name.to_string()))
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
    pub(crate) fn object_mut(&mut self, handle: ObjectHandle) -> &mut Object {
        self.objects.get_mut(handle.0).unwrap()
    }
    fn num_objects(&self) -> usize {
        return self.objects.len();
    }
    fn new_object(&mut self, source: ObjectSource) -> ObjectHandle {
        self.objects.push(Object{t: None, source});
        ObjectHandle(self.objects.len() - 1)
    }

    pub(crate) fn get_type(&self, handle: ObjectHandle) -> &Type {
        self.object(handle).t.as_ref().unwrap()
    }

    pub(crate) fn get_source(&self, handle: ObjectHandle) -> &ObjectSource {
        &self.object(handle).source
    }

    pub(crate) async fn from_function_ast(ast: &FunctionAst, signature: FunctionSignature, prog: Arc<Program>) -> Result<Self> {
        let return_type = signature.return_type;
        let mut graph = Self::new();
        let return_object = graph.new_object(ObjectSource::Local);
    
        let ctx = GraphBuildCtx{
            labels: Labels{break_: None},
            prog: prog.clone(),
            return_object,
        };

        for (idx, arg) in signature.args.iter().enumerate() {
            let arg_handle = graph.add_name(&arg.0, ObjectSource::Argument(idx))?;
            graph.block_mut(graph.entry_block()).instructions.push(Instruction{t: InstructionType::SetType(arg_handle, arg.1.clone())});
        }
        graph.push_scope();

        let final_block = graph.evaluate_and_store(ast, graph.entry_block(), Some(return_object), &ctx).await?;

        if let Some(handle) = final_block {
            let block = graph.block_mut(handle);
            if block.jump.is_none() {
                block.instructions.push(Instruction{t: InstructionType::Return{src: return_object}});
                block.jump = Some(Jump{t: JumpType::Return});
            }
        }

        add_types(&mut graph, prog.clone(), return_type, return_object).await?;
        for handle in graph.objects() {
            let object = graph.object(handle);
            if object.t.is_none() {
                return Err(Error::TypeNotInferred);
            }
        }

        Ok(graph)
    }

    fn evaluate_and_store<'a, 'b, 'c, 'd>(&'b mut self, ast: &'c FunctionAst, current_block: BlockHandle, object_handle: Option<ObjectHandle>, ctx: &'d GraphBuildCtx) -> BoxFuture<'a, Result<Option<BlockHandle>>>
    where
        'b: 'a,
        'c: 'a,
        'd: 'a
    {
        async move {
            let unwrap_block = |b: Option<BlockHandle>| b.ok_or(Error::UnreachableStatement);

            match ast.t {
                FunctionAstType::IfStatement{ref condition, ref true_, ref false_ } => {
                    let cond_obj_handle = self.new_object(ObjectSource::Local);
                    let after_eval_block = self.evaluate_and_store(&*condition, current_block, Some(cond_obj_handle), ctx).await?;

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
                    ).await?;

                    let final_false_block = self.evaluate_and_store(
                        &*false_,
                        false_block,
                        object_handle,
                        ctx,
                    ).await?;

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

                    let final_block = self.evaluate_and_store(&*body, body_block, None, &loop_ctx).await?;
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
                        t: JumpType::Uncond(ctx.labels.break_.ok_or(Error::BreakOutsideOfLoop)?)
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
                        block_handle = self.evaluate_and_store(item, unwrap_block(block_handle)?, obj, ctx).await?;
                    }

                    if let None = block_handle {

                    }

                    self.pop_scope();
                    Ok(block_handle)
                },
                FunctionAstType::ParenGroup{ref lhs, ref inside, ref rhs} => {
                    if rhs.is_some() {
                        return Err(Error::ParenGroupAsPrefix);
                    }

                    match lhs {
                        None => {
                            let val = self.new_object(ObjectSource::Local);
                            match inside {
                                None => Err(Error::InvalidEmptyParens),
                                Some(inside) => self.evaluate_and_store(&**inside, current_block, Some(val), ctx).await,
                            }
                        },
                        Some(lhs) => {
                            
                            let callee_path;
                            if let FunctionAstType::VarName(ref callee_name) = lhs.t {
                                callee_path = ItemPath::new(callee_name);
                            } else {
                                return Err(Error::FunctionCallRequiresIdent)
                            }

                            let mut block = current_block;
                            let mut args: Vec<ObjectHandle> = vec![];
                            if let Some(top_ast) = inside {
                                let mut ast= top_ast;
                                loop {
                                    match ast.t {
                                        FunctionAstType::Comma{ref lhs, ref rhs} => {
                                            let object_handle = self.new_object(ObjectSource::Local);
                                            block = self.evaluate_and_store(lhs, block, Some(object_handle), ctx).await?.ok_or(Error::FunctionArgMustEvaluate)?;
                                            args.push(object_handle);

                                            if let Some(ref rhs_ast) = rhs {
                                                ast = rhs_ast;
                                            } else {
                                                break;
                                            }
                                        },
                                        _ => {
                                            let object_handle = self.new_object(ObjectSource::Local);
                                            block = self.evaluate_and_store(ast, block, Some(object_handle), ctx).await?.ok_or(Error::FunctionArgMustEvaluate)?;
                                            args.push(object_handle);
                                            break;
                                        }
                                    };
                                }
                            }

                            let signature = make_query!(ctx.prog, GetFunctionSignature{path: callee_path.clone()}).await?;

                            {
                                let expected = signature.args.len();
                                let got = args.len();
                                if expected != got {
                                    return Err(Error::WrongNumberOfFunctionArgs{expected, got});
                                }
                            }

                            let t = InstructionType::Call{
                                dst: object_handle.unwrap_or_else(|| self.new_object(ObjectSource::Local)),
                                callee: callee_path,
                                signature: signature.as_ref().clone(),
                                args,
                            };

                            self.block_mut(block).instructions.push(Instruction{t});

                            Ok(Some(block))
                        },
                    }
                },
                FunctionAstType::BraceGroup{ref lhs, ref inside, ref rhs} => {
                    if rhs.is_some() {
                        return Err(Error::BraceGroupAsPrefix);
                    }

                    match lhs {
                        None => return Err(Error::NoStructNameBeforeBrace),
                        Some(lhs) => {
                            // doing struct init

                            let struct_path;
                            if let FunctionAstType::VarName(ref struct_name) = lhs.t {
                                struct_path = ItemPath::new(struct_name);
                            } else {
                                return Err(Error::StructInitRequiresIdent)
                            }

                            let mut cur_block = current_block;
                            if let Some(obj) = object_handle {
                                self.block_mut(cur_block).instructions.push(Instruction{
                                    t: InstructionType::SetType(obj, Type::Struct(struct_path))
                                });
                            }

                            match inside {
                                None => return Ok(Some(current_block)),
                                Some(inside_ast) => {
                                    if let FunctionAstType::StructInit{ref fields} = inside_ast.t {
                                        for field in fields {
                                            if let FunctionAstType::StructFieldInit{ref field_name, ref value} = field.t {
                                                let value_object = self.new_object(ObjectSource::Local);
                                                cur_block = unwrap_block(
                                                    self.evaluate_and_store(&value, cur_block, Some(value_object), ctx).await?
                                                )?;

                                                if let Some(obj) = object_handle {
                                                    let block = self.block_mut(cur_block);
                                                    block.instructions.push(Instruction{t: InstructionType::StructFieldAssignment{
                                                        parent_object: obj,
                                                        field_spec: vec![field_name.clone()],
                                                        value: value_object,
                                                    }});
                                                }
                                            } else {
                                                panic!("Not a struct init field inside braces");        
                                            }
                                        }
                                    } else {
                                        panic!("Not a struct init inside braces");
                                    }
                                }
                            }
                            Ok(Some(cur_block))
                        }
                    }
                },
                FunctionAstType::StructInit{..} => {
                    panic!("StructInit outside of braces");
                },
                FunctionAstType::StructFieldInit{..} => {
                    panic!("StructFieldInit outside of braces");
                },
                FunctionAstType::Dot{ref lhs, ref rhs} => {
                    if let FunctionAstType::VarName(ref field_name) = rhs.t {
                        let parent_object = self.new_object(ObjectSource::Local);

                        let cur_block = self.evaluate_and_store(
                            lhs,
                            current_block,
                            Some(parent_object),
                            ctx
                        ).await?.ok_or(Error::FieldAccessOnInvalidType(Type::Never))?;

                        if let Some(dest) = object_handle {
                            self.block_mut(cur_block).instructions.push(Instruction{
                                t: InstructionType::StructFieldAccess {
                                    parent_object,
                                    field: field_name.clone(),
                                    dest
                                }
                            });
                        }
                        return Ok(Some(cur_block));
                    } else {
                        return Err(Error::ExpectedStructFieldName);
                    }
                },
                FunctionAstType::Comma{..} => {
                    Err(Error::InvalidComma)
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
                    cur_block = unwrap_block(self.evaluate_and_store(&*lhs, cur_block, Some(lhs_obj), ctx).await?)?;

                    let rhs_obj = self.new_object(ObjectSource::Local);
                    cur_block = unwrap_block(self.evaluate_and_store(&*rhs, cur_block, Some(rhs_obj), ctx).await?)?;

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
                    cur_block = unwrap_block(self.evaluate_and_store(&*lhs, cur_block, Some(lhs_obj), ctx).await?)?;

                    let rhs_obj = self.new_object(ObjectSource::Local);
                    cur_block = unwrap_block(self.evaluate_and_store(&*rhs, cur_block, Some(rhs_obj), ctx).await?)?;

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
                    let after_eval_block = unwrap_block(self.evaluate_and_store(&*value, current_block, Some(ctx.return_object), ctx).await?)?;
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
                    self.evaluate_and_store(&*value, current_block, Some(var_obj), ctx).await
                },
                FunctionAstType::Assign{ref ident, ref value} => {
                    if let Some(obj) = object_handle {
                        self.block_mut(current_block).instructions.push(Instruction{t: InstructionType::StoreNull(obj)});
                    }
                    let var_obj = self.get_name(&ident)?;
                    self.evaluate_and_store(&*value, current_block, Some(var_obj), ctx).await
                },
                FunctionAstType::StructFieldAssign{ref ident, ref field_spec, ref value} => {
                    if let Some(obj) = object_handle {
                        self.block_mut(current_block).instructions.push(Instruction{t: InstructionType::StoreNull(obj)});
                    }

                    let eval_obj = self.new_object(ObjectSource::Local);
                    let cur_block = unwrap_block(self.evaluate_and_store(&*value, current_block, Some(eval_obj), ctx).await?)?;
                    
                    let var_obj = self.get_name(ident)?;
                    self.block_mut(cur_block).instructions.push(Instruction{
                        t: InstructionType::StructFieldAssignment{
                            parent_object: var_obj,
                            field_spec: field_spec.clone(),
                            value: eval_obj,
                        }
                    });
                    Ok(Some(cur_block))
                },
            }
        }.boxed()
    }
}






async fn add_types(graph: &mut Graph, prog: Arc<Program>, return_type: Type, return_object: ObjectHandle) -> Result<()> {
    let mut is = InferenceSystem::new();
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
                InstructionType::StructFieldAssignment{ref parent_object, ref field_spec, ref value} => {
                    is.add_eqn(TypeExpression::Placeholder(*value), TypeExpression::StructField(*parent_object, field_spec.clone()));
                },
                InstructionType::StructFieldAccess{ref parent_object, ref field, ref dest} => {
                    is.add_eqn(TypeExpression::Placeholder(*dest), TypeExpression::StructField(*parent_object, vec![field.clone()]));
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
    let types = is.results(&graph.never_objects, prog).await?;
    for obj in graph.objects() {
        graph.object_mut(obj).t = types.get(&obj).cloned();
    }
    Ok(())
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
        let global_items = make_query!(&prog, GetGlobalItems).await?;

        let item = global_items.get(&self.path).ok_or(
            Error::NoSuchItem(self.path.clone())
        )?;

        match item.t {
            ItemType::Fn(_) => parse_body(&item.tokens),
            _ => return Err(Error::ExpectedFn{path: self.path, got: item.t.clone()}),
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
        let global_items = make_query!(&prog, GetGlobalItems).await?;

        let item = global_items.get(&self.path).ok_or(
            Error::NoSuchItem(self.path.clone())
        )?;

        match item.t {
            ItemType::Fn(ref signature) => {
                Ok(signature.clone())
            },
            _ => return Err(Error::ExpectedFn{path: self.path, got: item.t.clone()}),
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
    
        let (function_signature, function_ast) = try_join!(function_signature_fut, function_ast_fut)?;
        Graph::from_function_ast(&function_ast, function_signature.as_ref().clone(), prog.clone()).await
    }
}
