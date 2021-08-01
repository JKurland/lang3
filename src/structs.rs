use std::sync::Arc;

use crate::inference::Type;
use crate::lex::{Token, TokenType};
use crate::{Error, Program, Query, Result, SourceRef, make_query};
use crate::itemise::{GetGlobalItems, ItemPath, ItemType};


#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Struct {
    members: Vec<(String, Type, SourceRef)>,
}

impl Struct {
    fn new() -> Self {
        Self{
            members: Vec::new()
        }
    }

    fn add_member(&mut self, name: String, t: Type, source_ref: SourceRef) -> Result<()> {
        for (field_name, _, o_source_ref) in self.members.iter() {
            if &name == field_name {
                return Err(Error::StructMemberNameConflict(source_ref, field_name.clone(), *o_source_ref));
            }
        }
        self.members.push((name, t, source_ref));
        Ok(())
    }

    pub(crate) fn get_member(&self, name: &str) -> Option<&Type> {
        for (field_name, t, _) in self.members.iter() {
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
        for (idx, (field_name, _, _)) in self.members.iter().enumerate() {
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
        AfterName(String, usize /*field state*/),
        AfterColon(String, usize /*field state*/),
        MemberEnd,
    }

    let mut state = State::MemberStart;
    let mut struct_ = Struct::new();

    for (idx, token) in tokens.iter().enumerate() {
        let new_state = match state {
            State::MemberStart => {
                match &token.t {
                    TokenType::Ident(name) => {
                        State::AfterName(name.clone(), idx)
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(token.source_ref, vec!["member name identifier"]));
                    }
                }
            },
            State::AfterName(name, start_idx) => {
                match token.t {
                    TokenType::Colon => {
                        State::AfterColon(name, start_idx)
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(token.source_ref, vec![":"]));
                    }
                }
            },
            State::AfterColon(name, start_idx) => {
                let source_ref = tokens[start_idx].source_ref.to(&tokens[idx].source_ref);
                match &token.t {
                    TokenType::Ident(type_name) => {
                        struct_.add_member(name, Type::Struct(ItemPath::new(&type_name)), source_ref)?;
                        State::MemberEnd
                    },
                    TokenType::U32 => {
                        struct_.add_member(name, Type::U32, source_ref)?;
                        State::MemberEnd
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(token.source_ref, vec!["type name"]));
                    }
                }
            },
            State::MemberEnd => {
                match token.t {
                    TokenType::Comma => {
                        State::MemberStart
                    },
                    _ => {
                        return Err(Error::SyntaxErrorExpected(token.source_ref, vec![","]));
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
            // if tokens was empty state would be State::MemberStart
            Err(Error::UnexpectedEndOfStruct(tokens[0].source_ref))
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
    pub(crate) async fn make(self, prog: Arc<Program>, query_source: SourceRef) -> <Self as Query>::Output {
        let global_items = make_query!(&prog, GetGlobalItems, query_source).await?;

        let item = global_items.get(&self.path).ok_or(
            Error::NoSuchItem(query_source, self.path.clone())
        )?;

        match item.t {
            ItemType::Struct => {
                return parse_struct_item(&item.tokens)
            },
            _ => return Err(Error::WrongItemType(query_source, self.path, ItemType::Struct, item.t.clone())),
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
