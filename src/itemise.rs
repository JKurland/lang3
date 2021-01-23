use crate::{lex::{Token, TokenType, GetTokenStream}, make_query};
use std::collections::HashMap;
use crate::{Result, Error, Query, Program};
use std::sync::Arc;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct ItemPath {
    name: String,
}

impl ItemPath {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ItemType {
    Fn,
    Struct,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Item {
    pub(crate) t: ItemType,
    pub(crate) tokens: Vec<Token>,
}

pub(crate) fn itemise(token_stream: &Vec<Token>) -> Result<HashMap<ItemPath, Item>> {
    enum State {
        Signature(Option<ItemType>, Option<ItemPath>),
        Body(ItemType, ItemPath, usize),
    }

    let mut state = State::Signature(None, None);
    let mut current_item_tokens = Vec::new();
    let mut rtn = HashMap::new();
    for token in token_stream {
        let new_state = match state {
            State::Body(item_type, item_path, mut brace_count) => {
                match token.t {
                    TokenType::OpenBrace => brace_count += 1,
                    TokenType::CloseBrace => brace_count -= 1,
                    _ => {current_item_tokens.push((*token).clone());}
                }

                if brace_count == 0 {
                    rtn.insert(item_path, Item{t: item_type, tokens: current_item_tokens});
                    current_item_tokens = Vec::new();
                    State::Signature(None, None)
                } else {
                    State::Body(item_type, item_path, brace_count)
                }
            },
            State::Signature(item_type, item_path) => {
                match token.t {
                    TokenType::OpenBrace => {
                        State::Body(
                            item_type.ok_or(Error::new("Expected item type before {"))?,
                            item_path.ok_or(Error::new("Expected item ident before {"))?,
                            1
                        )
                    },
                    TokenType::CloseBrace => {
                        return Err(Error::new("Unexpected }"));
                    },
                    TokenType::Fn => {
                        if item_type.is_some() {
                            return Err(Error::new("Unexpected fn keyword"));
                        }

                        State::Signature(Some(ItemType::Fn), None)
                    },
                    TokenType::Struct => {
                        if item_type.is_some() {
                            return Err(Error::new("Unexpected struct keyword"));
                        }

                        State::Signature(Some(ItemType::Struct), None)
                    },
                    TokenType::Ident(ref name) => {
                        if item_type.is_none() {
                            return Err(Error::new("Expected item type before item identifier"));
                        }

                        State::Signature(item_type, Some(ItemPath{name: (*name).clone()}))
                    },
                    _ => {
                        return Err(Error::new("Unexpected token in item signature"));
                    }
                }
            }
        };
        state = new_state;
    }
    Ok(rtn)
}

#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetGlobalItems;

impl Query for GetGlobalItems {
    type Output = Result<HashMap<ItemPath, Item>>;
}

impl GetGlobalItems {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let tokens = make_query!(&prog, GetTokenStream).await;
        match *tokens {
            Ok(ref tokens) => itemise(tokens),
            Err(ref e) => Err(e.clone()),
        }
    }
}


mod test {
    use super::*;
    use crate::lex::lex;
    use maplit::hashmap;

    #[test]
    fn test_items() {
        let token_stream = lex("fn a {}\nstruct s {1}").unwrap();
        let items = itemise(&token_stream);
        assert_eq!(
            items.unwrap(),
            hashmap!{
                ItemPath::new("a") => Item{t: ItemType::Fn, tokens: vec![]},
                ItemPath::new("s") => Item{t: ItemType::Struct, tokens: vec![Token{t: TokenType::Int("1".to_string())}]}
            }
        );
    }
}
