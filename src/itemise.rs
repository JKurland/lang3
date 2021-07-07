use crate::{lex::{Token, TokenType, GetTokenStream}, make_query};
use std::collections::HashMap;
use crate::{Result, Error, Query, Program};
use std::sync::Arc;
use crate::inference::Type;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct ItemPath {
    name: String,
}

impl ItemPath {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FunctionSignature {
    pub(crate) args: Vec<(String, Type)>,
    pub(crate) return_type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ItemType {
    Fn(FunctionSignature),
    Struct,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Item {
    pub(crate) t: ItemType,
    pub(crate) tokens: Vec<Token>,
}

fn parse_fn_signature(tokens: &[Token]) -> Result<FunctionSignature> {
    enum State {
        Initial,
        ArgStart,
        ArgName(String),
        ArgNameColon(String),
        ArgEnd,
        ArgsEnd,
        TypeArrow,
        End,
    }

    let mut state = State::Initial;
    let mut args = Vec::new();
    let mut return_type = None;

    for token in tokens {
        let new_state = match state {
            State::Initial => {
                match token.t {
                    TokenType::OpenParen => State::ArgStart,
                    _ => return Err(Error::new("Expected (")),
                }
            },
            State::ArgStart => {
                match token.t {
                    TokenType::Ident(ref s) => State::ArgName(s.clone()),
                    TokenType::CloseParen => {
                        if args.len() == 0 {
                            State::ArgsEnd
                        } else {
                            return Err(Error::new("Expected argument name"));
                        }
                    }
                    _ => return Err(Error::new("Expected argument name")),
                }
            },
            State::ArgName(s) => {
                match token.t {
                    TokenType::Colon => State::ArgNameColon(s),
                    _ => return Err(Error::new("Expected colon")),
                }
            },
            State::ArgNameColon(s) => {
                match token.t {
                    TokenType::Ident(ref t) => {
                        args.push((s, Type::Struct(ItemPath{name: t.clone()})));
                        State::ArgEnd
                    },
                    TokenType::U32 => {
                        args.push((s, Type::U32));
                        State::ArgEnd
                    },
                    _ => return Err(Error::new("Expected type identifier")),
                }
            },
            State::ArgEnd => {
                match token.t {
                    TokenType::Comma => State::ArgStart,
                    TokenType::CloseParen => State::ArgsEnd,
                    _ => return Err(Error::new("Expected , or )")),
                }
            },
            State::ArgsEnd => {
                match token.t {
                    TokenType::ThinArrow => State::TypeArrow,
                    _ => return Err(Error::new("Expected ->")),
                }
            },
            State::TypeArrow => {
                match token.t {
                    TokenType::Ident(ref s) => {
                        return_type = Some(Type::Struct(ItemPath{name: s.clone()}));
                        State::End
                    },
                    TokenType::U32 => {
                        return_type = Some(Type::U32);
                        State::End
                    },
                    _ => return Err(Error::new("Expected return type name"))
                }
            },
            State::End => {
                return Err(Error::new("Unexpected token"));
            }
        };
        state = new_state;
    }

    match state {
        State::End | State::ArgsEnd => {
            Ok(FunctionSignature {
                args,
                return_type: return_type.unwrap_or(Type::Null),
            })
        },
        _ => Err(Error::new("Unexpected end of function signature"))
    }

}

pub(crate) fn itemise(token_stream: &Vec<Token>) -> Result<HashMap<ItemPath, Item>> {
    enum State {
        NoItem,
        StructNoPath,
        Struct(ItemPath),

        FnNoPath,
        FnPartialSignature{
            path: ItemPath,
            first_token: usize,
        },

        Body(ItemType, ItemPath, usize),
    }

    let mut state = State::NoItem;
    let mut current_item_tokens = Vec::new();
    let mut rtn = HashMap::new();
    for (idx, token) in token_stream.iter().enumerate() {
        let new_state = match state {
            State::NoItem => {
                match token.t {
                    TokenType::Struct => State::StructNoPath,
                    TokenType::Fn => State::FnNoPath,
                    _ => return Err(Error::new("Expected struct or fn")),
                }
            },

            State::StructNoPath => {
                match token.t {
                    TokenType::Ident(ref s) => State::Struct(ItemPath{name: s.clone()}),
                    _ => return Err(Error::new("Expected struct name")),
                }
            },
            State::Struct(path) => {
                match token.t {
                    TokenType::OpenBrace => State::Body(ItemType::Struct, path, 1),
                    _ => return Err(Error::new("Expected {")),
                }
            },

            State::FnNoPath => {
                match token.t {
                    TokenType::Ident(ref s) => State::FnPartialSignature{
                        path: ItemPath{name: s.clone()},
                        first_token: idx + 1
                    },
                    _ => return Err(Error::new("Expected fn name")),
                }
            },
            State::FnPartialSignature{path, first_token} => {
                match token.t {
                    TokenType::OpenBrace => State::Body(
                        ItemType::Fn(parse_fn_signature(&token_stream[first_token..idx])?),
                        path,
                        1
                    ),
                    _ => State::FnPartialSignature{path, first_token},
                }
            },
            State::Body(item_type, path, mut brace_depth) => {
                match token.t {
                    TokenType::OpenBrace => brace_depth += 1,
                    TokenType::CloseBrace => brace_depth -= 1,
                    _ => {},
                };

                if brace_depth == 0 {
                    rtn.insert(path, Item{t: item_type, tokens: current_item_tokens});
                    current_item_tokens = Vec::new();
                    State::NoItem
                } else {
                    current_item_tokens.push(token.clone());
                    State::Body(item_type, path, brace_depth)
                }
            },
        };
        state = new_state;
    }

    match state {
        State::NoItem => Ok(rtn),
        _ => {Err(Error::new("Unexpected end of file"))}
    }
}

#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetGlobalItems;

impl Query for GetGlobalItems {
    type Output = Result<HashMap<ItemPath, Item>>;
}

impl GetGlobalItems {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let tokens = make_query!(&prog, GetTokenStream).await?;
        itemise(&tokens)
    }
}


mod test {
    #[cfg(test)]
    use super::*;
    #[cfg(test)]
    use crate::lex::lex;
    #[cfg(test)]
    use maplit::hashmap;

    #[test]
    fn test_items() {
        let token_stream = lex("fn a() {}\nstruct s {1}").unwrap();
        let items = itemise(&token_stream);
        assert_eq!(
            items.unwrap(),
            hashmap!{
                ItemPath::new("a") => Item{t: ItemType::Fn(FunctionSignature{args: Vec::new(), return_type: Type::Null}), tokens: vec![]},
                ItemPath::new("s") => Item{t: ItemType::Struct, tokens: vec![Token{t: TokenType::Int("1".to_string())}]}
            }
        );
    }
}
