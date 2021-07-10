use std::sync::Arc;

use crate::inference::Type;
use crate::lex::{Token, TokenType};
use crate::{Error, Program, Query, Result, make_query};
use crate::itemise::{GetGlobalItems, ItemPath, ItemType};


#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Struct {
    members: Vec<(String, Type)>,
}

impl Struct {
    fn new() -> Self {
        Self{
            members: Vec::new()
        }
    }

    fn add_member(&mut self, name: String, t: Type) -> Result<()> {
        for (field_name, _) in self.members.iter() {
            if &name == field_name {
                return Err(Error::StructMemberNameConflict(field_name.clone()));
            }
        }
        self.members.push((name, t));
        Ok(())
    }

    pub(crate) fn get_member(&self, name: &str) -> Option<&Type> {
        for (field_name, t) in self.members.iter() {
            if name == field_name {
                return Some(t);
            }
        }
        None
    }

    pub(crate) fn member_types(&self) -> impl Iterator<Item = &Type> {
        self.members.iter().map(|m| &m.1)
    }

    pub(crate) fn member_idx(&self, name: &str) -> Option<usize> {
        for (idx, (field_name, _)) in self.members.iter().enumerate() {
            if name == field_name {
                return Some(idx);
            }
        }
        None
    }
}


fn parse_struct_item(tokens: &[Token]) -> Result<Struct> {
    enum State {
        MemberStart,
        AfterName(String),
        AfterColon(String),
        MemberEnd,
    }

    let mut state = State::MemberStart;
    let mut struct_ = Struct::new();

    for token in tokens {
        let new_state = match state {
            State::MemberStart => {
                match &token.t {
                    TokenType::Ident(name) => {
                        State::AfterName(name.clone())
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(vec!["member name identifier"]));
                    }
                }
            },
            State::AfterName(name) => {
                match token.t {
                    TokenType::Colon => {
                        State::AfterColon(name)
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(vec![":"]));
                    }
                }
            },
            State::AfterColon(name) => {
                match &token.t {
                    TokenType::Ident(type_name) => {
                        struct_.add_member(name, Type::Struct(ItemPath::new(&type_name)))?;
                        State::MemberEnd
                    },
                    TokenType::U32 => {
                        struct_.add_member(name, Type::U32)?;
                        State::MemberEnd
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(vec!["type name"]));
                    }
                }
            },
            State::MemberEnd => {
                match token.t {
                    TokenType::Comma => {
                        State::MemberStart
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(vec![","]));
                    }
                }
            },
        };
        state = new_state;
    }

    match state {
        State::MemberStart | State::MemberEnd => {
            Ok(struct_)
        },
        _ => {
            Err(Error::UnexpectedEndOfStruct)
        }
    }
}








#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetStruct {
    pub(crate) path: ItemPath,
}

impl Query for GetStruct {
    type Output = Result<Struct>;
}

impl GetStruct {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let global_items = make_query!(&prog, GetGlobalItems).await?;

        let item = global_items.get(&self.path).ok_or(
            Error::NoSuchItem(self.path.clone())
        )?;

        match item.t {
            ItemType::Struct => {
                return parse_struct_item(&item.tokens)
            },
            _ => return Err(Error::WrongItemType{path: self.path, expected: ItemType::Struct, got: item.t.clone()}),
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
        "#).unwrap()).unwrap();

        assert_eq!(result.member_types().count(), 0);
    }

    #[test]
    fn test_parse_struct() {
        let examples = vec![
            (
                r#"
                    name: u32
                "#,
                hashmap!{
                    "name".to_string() => Type::U32,
                }
            ),
            (
                r#"
                    name: hello
                "#,
                hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                }
            ),
            (
                r#"
                    name: hello,
                "#,
                hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                }
            ),
            (
                r#"
                    name: hello,
                    name2: u32
                "#, 
                hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                    "name2".to_string() => Type::U32,
                }
            ),
            (
                r#"
                    name: hello,
                    name2: u32,
                "#, 
                hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                    "name2".to_string() => Type::U32,
                }
            ),
            (
                r#"
                    name: hello,
                    name2: u32,
                    name3: u32,
                    hello: u32,
                    bla: hello,
                "#, 
                hashmap!{
                    "name".to_string() => Type::Struct(ItemPath::new("hello")),
                    "name2".to_string() => Type::U32,
                    "name3".to_string() => Type::U32,
                    "hello".to_string() => Type::U32,
                    "bla".to_string() => Type::Struct(ItemPath::new("hello")),
                }
            ),
        ];

        for example in examples {
            let s = parse_struct_item(&lex(example.0).unwrap()).unwrap();
            for (name, ty) in example.1.iter() {
                assert_eq!(s.get_member(name).unwrap(), ty);
            }
        }
    }
}
