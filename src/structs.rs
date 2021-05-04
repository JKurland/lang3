use std::collections::HashMap;
use std::sync::Arc;

use crate::function::Type;
use crate::lex::{Token, TokenType};
use crate::{Error, Program, Query, Result, make_query};
use crate::itemise::{GetGlobalItems, ItemPath, ItemType};


#[derive(Debug, PartialEq, Eq)]
struct Struct {
    members: HashMap<String, Type>,
}


fn add_member(members: &mut HashMap<String, Type>, name: String, t: Type) -> Result<()> {
    let old_value = members.insert(name, t);
    if old_value.is_some() {
        return Err(Error::new("struct member name conflict"));
    }
    Ok(())
}

fn parse_struct_item(tokens: &[Token]) -> Result<Struct> {
    enum State {
        MemberStart,
        AfterName(String),
        AfterColon(String),
        MemberEnd,
    }

    let mut state = State::MemberStart;
    let mut members = HashMap::new();

    for token in tokens {
        let new_state = match state {
            State::MemberStart => {
                match &token.t {
                    TokenType::Ident(name) => {
                        State::AfterName(name.clone())
                    },
                    _ => {
                        return Err(Error::new("Expected member name"));
                    }
                }
            },
            State::AfterName(name) => {
                match token.t {
                    TokenType::Colon => {
                        State::AfterColon(name)
                    },
                    _ => {
                        return Err(Error::new("Expected :"));
                    }
                }
            },
            State::AfterColon(name) => {
                match &token.t {
                    TokenType::Ident(type_name) => {
                        add_member(&mut members, name, Type::Struct(ItemPath::new(&type_name)))?;
                        State::MemberEnd
                    },
                    TokenType::U32 => {
                        add_member(&mut members, name, Type::U32)?;
                        State::MemberEnd
                    },
                    _ => {
                        return Err(Error::new("Expected type name"));
                    }
                }
            },
            State::MemberEnd => {
                match token.t {
                    TokenType::Comma => {
                        State::MemberStart
                    },
                    _ => {
                        return Err(Error::new("Expected ,"));
                    }
                }
            },
        };
        state = new_state;
    }

    match state {
        State::MemberStart | State::MemberEnd => {
            Ok(Struct{members})
        },
        _ => {
            Err(Error::new("Unexpected end of struct"))
        }
    }
}








#[derive(Hash, PartialEq, Clone)]
struct GetStruct {
    path: ItemPath,
}

impl Query for GetStruct {
    type Output = Result<Struct>;
}

impl GetStruct {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let global_items_arc = make_query!(&prog, GetGlobalItems).await;
        if global_items_arc.is_err() {
            return Err(global_items_arc.as_ref().as_ref().unwrap_err().clone());
        }

        let global_items = global_items_arc.as_ref().as_ref().unwrap();

        let item = global_items.get(&self.path).ok_or(
            Error::new("Could not find struct")
        )?;

        match item.t {
            ItemType::Struct => {
                return parse_struct_item(&item.tokens)
            },
            _ => return Err(Error::new("Not a struct")),
        }
    }
}


mod tests {
    #[cfg(test)]
    use crate::lex::lex;
    #[cfg(test)]
    use super::*;
    #[cfg(test)]
    use maplit::hashmap;

    #[test]
    fn test_parse_empty_struct() {
        let result = parse_struct_item(&lex(r#"
        "#).unwrap());

        assert_eq!(result, Ok(Struct{members: HashMap::new()}));
    }

    #[test]
    fn test_parse_struct() {
        let examples = vec![
            (
                r#"
                    name: u32
                "#,
                Ok(Struct{members: hashmap!{
                    "name".to_string() => Type::U32,
                }
            })),
            (
                r#"
                    name: hello
                "#,
                Ok(Struct{members: hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                }
            })),
            (
                r#"
                    name: hello,
                "#,
                Ok(Struct{members: hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                }
            })),
            (
                r#"
                    name: hello,
                    name2: u32
                "#, 
                Ok(Struct{members: hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                    "name2".to_string() => Type::U32,
                }
            })),
            (
                r#"
                    name: hello,
                    name2: u32,
                "#, 
                Ok(Struct{members: hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                    "name2".to_string() => Type::U32,
                }
            })),
            (
                r#"
                    name: hello,
                    name2: u32,
                    name3: u32,
                    hello: u32,
                    bla: hello,
                "#, 
                Ok(Struct{members: hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                    "name2".to_string() => Type::U32,
                    "name3".to_string() => Type::U32,
                    "hello".to_string() => Type::U32,
                    "bla".to_string() => Type::Struct(ItemPath::new("hello")),
                }
            })),
        ];

        for example in examples {
            assert_eq!(parse_struct_item(&lex(example.0).unwrap()), example.1);
        }
    }
}
