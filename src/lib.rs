#![allow(dead_code)]

use chashmap::CHashMap;
use futures::channel::oneshot::{channel, Sender, Receiver};
use futures::executor::ThreadPool;
use inference::Type;
use itemise::{ItemPath, ItemType};
use std::future::Future;
use std::collections::hash_map;
use futures::future::{FutureExt, Shared};

use std::hash::{Hash, Hasher};
use std::any::Any;
use std::sync::Arc;
use std::fmt;

pub(crate) mod lex;
pub(crate) mod itemise;
pub(crate) mod function;
pub(crate) mod inference;
pub(crate) mod vm;
pub(crate) mod storage;
pub(crate) mod structs;


#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Error {
    TypeNotInferred,
    NoSuchItem(ItemPath),
    WrongItemType{path: ItemPath, expected: ItemType, got: ItemType},
    StructHasNoField(ItemPath, String),
    FieldAccessOnInvalidType(Type),
    ExpectedFn{path: ItemPath, got: ItemType},
    UnreachableStatement,
    BreakOutsideOfLoop,
    ParenGroupAsPrefix,
    BraceGroupAsPrefix,
    InvalidEmptyParens,
    FunctionCallRequiresIdent,
    WrongNumberOfFunctionArgs{expected: usize, got: usize},
    NoStructNameBeforeBrace,
    StructInitRequiresIdent,
    ExpectedStructFieldName,
    InvalidComma,
    FunctionArgMustEvaluate,
    SyntaxErrorExpected(Vec<&'static str>),
    SyntaxErrorUnexpected(Vec<&'static str>),
    UnexpectedEndOfFunction,
    NameRedefined(String),
    UnknownName(String),
    StringNotClosed,
    UnknownToken,
    UnexpectedEof,
    TypeMismatch(Type, Type),
    SelfReferentialType,
    StructMemberNameConflict(String),
    UnexpectedEndOfStruct,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::TypeNotInferred => write!(f, "Type not inferred"),
            Error::NoSuchItem(path) => write!(f, "No such item: {:?}", path),
            Error::WrongItemType{path, expected, got} => write!(f, "Item {:?} was expected to be {:?}, was {:?}", path, expected, got),
            Error::StructHasNoField(path, field_name) => write!(f, "Struct {:?} has no field {}", path, field_name),
            Error::FieldAccessOnInvalidType(t) => write!(f, "Cannot do field access on object of type: {:?}", t),
            Error::ExpectedFn{path, got} => write!(f, "Expected fn on path {:?}, got {:?}", path, got),
            Error::UnreachableStatement => write!(f, "A statement in the code is unreachable"),
            Error::BreakOutsideOfLoop => write!(f, "Break statements are only allowed in loops"),
            Error::ParenGroupAsPrefix => write!(f, "Paren group not supported as prefix operator"),
            Error::BraceGroupAsPrefix => write!(f, "Brace group not supported as prefix operator"),
            Error::InvalidEmptyParens => write!(f, "Parens should not be empty"),
            Error::FunctionCallRequiresIdent => write!(f, "Function calls currently only support function by name"),
            Error::WrongNumberOfFunctionArgs{expected, got} => write!(f, "Wrong number of function args, expected: {}, got: {}", expected, got),
            Error::NoStructNameBeforeBrace => write!(f, "Expected struct name before brace group"),
            Error::StructInitRequiresIdent => write!(f, "Struct inits currently only support struct by name"),
            Error::ExpectedStructFieldName => write!(f, "Expected struct field name"),
            Error::InvalidComma => write!(f, "comma must be inside function arguments"),
            Error::FunctionArgMustEvaluate => write!(f, "Function arg must evaluate"),
            Error::SyntaxErrorExpected(tokens) => write!(f, "Syntax error, expected {:?}", tokens),
            Error::SyntaxErrorUnexpected(tokens) => write!(f, "Syntax error, unexpected {:?}", tokens),
            Error::UnexpectedEndOfFunction => write!(f, "Unexpected end of function"),
            Error::NameRedefined(name) => write!(f, "Name {} redefined", name),
            Error::UnknownName(name) => write!(f, "Unkown name {}", name),
            Error::StringNotClosed => write!(f, "String literal not closed"),
            Error::UnknownToken => write!(f, "Unkown token"),
            Error::UnexpectedEof => write!(f, "Unexpected end of file"),
            Error::TypeMismatch(a, b) => write!(f, "Type mismatch, {:?} and {:?}", a, b),
            Error::SelfReferentialType => write!(f, "Could not infer types, self referential type"),
            Error::StructMemberNameConflict(name) => write!(f, "Two struct members have the same name {}", name),
            Error::UnexpectedEndOfStruct => write!(f, "Unexpected end of struct"),
            
        }
    }
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub(crate) struct CacheKey {
    hash: u64,
    partial_eq: fn(&dyn Any, &dyn Any) -> bool,
    query: Arc<dyn Any + Send + Sync>,
}

impl Hash for CacheKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl PartialEq for CacheKey {
    fn eq(&self, other: &Self) -> bool {
        return (self.partial_eq)(&*self.query, &*other.query);
    }
}

fn partial_eq_any<T: PartialEq + Any + 'static>(a: &dyn Any, b: &dyn Any) -> bool {
    a.downcast_ref::<T>() == b.downcast_ref::<T>()
}

impl CacheKey {
    fn new<T: Query>(key: T) -> Self {
        let mut hasher = hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        Self {
            hash: hasher.finish(),
            partial_eq: partial_eq_any::<T>,
            query: Arc::new(key),
        }
    }
}

pub(crate) enum CacheEntry {
    Running(Shared<Receiver<()>>),
    Failed(Box<dyn Any + Send + Sync>),
    Complete(Arc<dyn Any + Send + Sync>),
}

pub(crate) type ResultCache = CHashMap<CacheKey, CacheEntry>;

pub(crate) struct Program {
    pool: ThreadPool,
    result_cache: ResultCache,
    src: String,
}

async fn wrap_fut<T: Send + Sync + 'static, Fut: Future<Output = Result<T>>>(fut: Fut, sender: Sender<()>, prog: Arc<Program>, cache_key: CacheKey) {
    let result = fut.await;
    let mut cache_entry = prog.result_cache.get_mut(&cache_key).unwrap();
    sender.send(()).expect("Receiver was dropped");
    match result {
        Ok(ans) => {
            *cache_entry = CacheEntry::Complete(Arc::new(ans));
        },
        Err(e) => {
            *cache_entry = CacheEntry::Failed(Box::new(e));
        }
    }
}

impl Program {
    pub(crate) fn new(src: String) -> Self {
        Self {
            pool: ThreadPool::new().unwrap(),
            result_cache: CHashMap::new(),
            src,
        }
    }

    pub(crate) async fn run_query<Q: Query, T: Sync + Send + 'static, Fut>(self: &Arc<Self>, query: Q, future: Fut) -> Result<Arc<T>>
    where
    Fut: Future<Output = Result<T>> + Send + 'static {
        let cache_key = CacheKey::new(query);
        self.result_cache.upsert(
            cache_key.clone(),
            || {
                let (sender, receiver) = channel();
                let wrapped_future = wrap_fut(future, sender, self.clone(), cache_key.clone());
                self.pool.spawn_ok(wrapped_future);
                CacheEntry::Running(receiver.shared())
            },
            |_| {}
        );

        let result_fut = {
            let cache_entry = self.result_cache.get(&cache_key).unwrap();
            match *cache_entry {
                CacheEntry::Running(ref fut) => Some(fut.clone()),
                CacheEntry::Failed(_) => None,
                CacheEntry::Complete(_) => None,
            }
        };

        if let Some(fut) = result_fut {
            fut.await.expect("Query was cancelled");
        }

        let result_item = self.result_cache.get(&cache_key).unwrap();
        match *result_item {
            CacheEntry::Running(_) => panic!("Result is still not ready"),
            CacheEntry::Failed(ref error) => Err(error.as_ref().downcast_ref::<Error>().unwrap().clone()),
            CacheEntry::Complete(ref result) => Ok(result.clone().downcast::<T>().unwrap()),
        }
    }

    pub(crate) fn get_src(&self) -> &str {
        &self.src
    }
}

pub(crate) trait Query: Send + Sync + Clone + Hash + PartialEq + 'static {
    // Queries need to be Send, Sync, and cheap to Clone
    type Output;
}

#[macro_export]
macro_rules! make_query {
    ($prog:expr, $query:expr) => { 
        {
            let prog: Arc<$crate::Program> = ($prog).clone();
            let query = $query;
            async move {
                prog.run_query(query.clone(), query.make(prog.clone())).await
            }
        }
    };
}


mod tests {
    use super::*;
    use futures::executor::block_on;
    use vm::GetFunctionVmProgram;
    use itemise::ItemPath;


    fn run_main(src: &str) -> Result<u32> {
        let prog = Arc::new(Program::new(src.to_string()));
        let program = block_on(make_query!(&prog, GetFunctionVmProgram{
            path: ItemPath::new("main"),
        }))?;
        //dbg!(&program);
        let mut vm = vm::Vm::new(8092);
        Ok(vm.run(&program) as u32)
    }

    #[test]
    fn end_to_end01() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                return 3;
            }"#
        ), Ok(3));
    }

    #[test]
    fn end_to_end2() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                return 3 + 2;
            }"#
        ), Ok(5));
    }

    #[test]
    fn end_to_end3() {
        assert_eq!(run_main(
            r#"
            struct s {
                
            }

            fn main() -> u32 {
                return 3 + 2;
            }"#
        ), Ok(5));
    }

    #[test]
    fn end_to_end4() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                return i;
            }"#
        ), Ok(2));
    }

    #[test]
    fn end_to_end5() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                i = i + 1;
                return i;
            }"#
        ), Ok(3));
    }

    #[test]
    fn end_to_end6() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                i = i + i;
                return 2 + i;
            }"#
        ), Ok(6));
    }

    #[test]
    fn end_to_end7() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                if i == 2 {
                    i = 3;
                } else {
                    i = 1;
                }
                return i;
            }"#
        ), Ok(3));
    }

    #[test]
    fn end_to_end8() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                if i == 3 {
                    i = 3;
                } else {
                    i = 1;
                }
                return i;
            }"#
        ), Ok(1));
    }

    #[test]
    fn end_to_end9() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                if i == 3 {
                    i = 3;
                }
                return i;
            }"#
        ), Ok(2));
    }

    #[test]
    fn end_to_end10() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32 {
                let i = 2;
                loop {
                    i = i + 1;
                    if i == 10 {
                        break;
                    }
                }
                return i;
            }"#
        ), Ok(10));
    }

    #[test]
    fn two_funcs() {
        assert_eq!(run_main(
            r#"
            fn other() -> u32 {
                return 2;
            }

            fn main() -> u32 {
                other();
                return 10;
            }"#
        ), Ok(10));
    }

    #[test]
    fn test_if_return() {
        assert_eq!(run_main(
            r#"
            fn main() -> u32{
                if 2 == 2 {
                    63
                } else {
                    return 3;
                }
            }"#
        ), Ok(63));
    }

    #[test]
    fn test_function_call() {
        assert_eq!(run_main(r#"
            fn other() -> u32 {
                return 2;
            }

            fn main() -> u32 {
                let a = other();
                return a + 2;
            }"#
        ), Ok(4));
    }

    #[test]
    fn test_function_temp() {
        assert_eq!(run_main(r#"
            fn other() -> u32 {
                return 2;
            }

            fn main() -> u32 {
                return other() + 3;
            }"#
        ), Ok(5));
    }

    #[test]
    fn test_1_fn_arg() {
        assert_eq!(run_main(r#"
            fn add_2(a: u32) -> u32 {
                return a + 2;
            }

            fn main() -> u32 {
                return add_2(120) + 3;
            }"#
        ), Ok(125));
    }

    #[test]
    fn test_2_fn_args() {
        assert_eq!(run_main(r#"
            fn add(a: u32, b: u32) -> u32 {
                return a + b;
            }

            fn main() -> u32 {
                return add(120, 5) + 3;
            }"#
        ), Ok(128));
    }

    #[test]
    fn test_fn_args_exprs() {
        assert_eq!(run_main(r#"
            fn identity(a: u32) -> u32 {
                return a;
            }

            fn main() -> u32 {
                return identity(identity(10 + 5) + 3);
            }"#
        ), Ok(18));
    }

    #[test]
    fn test_struct_init_compiles() {
        assert_eq!(run_main(r#"
            struct S {
                a: u32,
            }

            fn main() -> u32 {
                let s = S{a: 2};
                return 18;
            }"#
        ), Ok(18));
    }

    #[test]
    fn test_struct_field_access() {
        assert_eq!(run_main(r#"
            struct S {
                a: u32,
            }

            fn main() -> u32 {
                let s = S{a: 2};
                return s.a;
            }"#
        ), Ok(2));
    }

    #[test]
    fn test_struct_field_access_2_fields() {
        assert_eq!(run_main(r#"
            struct S {
                a: u32,
                b: u32
            }

            fn main() -> u32 {
                let s = S{a: 2, b: 1 + 3};
                return s.b + s.a;
            }"#
        ), Ok(6));
    }

    #[test]
    fn test_nested_struct() {
        assert_eq!(run_main(r#"
            struct A {
                x: u32
            }

            struct S {
                a: A,
                a2: A,
            }

            fn main() -> u32 {
                let s = S{a: A{x: 1}, a2: A{x: 3}};
                return s.a.x + s.a2.x;
            }"#
        ), Ok(4));
    }

    #[test]
    fn test_struct_field_assign() {
        assert_eq!(run_main(r#"
            struct A {
                x: u32
            }

            fn main() -> u32 {
                let a = A{x: 3};
                a.x = 4;
                return a.x;
            }"#
        ), Ok(4));
    }

    #[test]
    fn test_nested_struct_assignment() {
        assert_eq!(run_main(r#"
            struct A {
                x: u32
            }

            struct S {
                a: A,
                a2: A,
            }

            fn main() -> u32 {
                let s = S{a: A{x: 1}, a2: A{x: 3}};
                s.a.x = 20;
                return s.a.x + s.a2.x;
            }"#
        ), Ok(23));
    }

    #[test]
    fn test_struct_function_argument() {
        assert_eq!(run_main(r#"
            fn f(a: A) -> u32 {
                return a.x + 1;
            }

            struct A {
                x: u32
            }

            fn main() -> u32 {
                let a = A{x: 10};
                return f(a);
            }"#
        ), Ok(11));
    }

    #[test]
    fn test_bad_return_type_error() {
        assert_eq!(run_main(r#"
            fn f() {
                return 1;
            }

            fn main() -> u32 {
                f(a);
                return 2;
            }"#
        ).is_err(), true);
    }

    #[test]
    fn test_bad_return_type_error2() {
        assert_eq!(run_main(r#"
            fn f() -> u32{
                return 1 == 3;
            }

            fn main() -> u32 {
                f(a);
                return 2;
            }"#
        ).is_err(), true);
    }

    #[test]
    fn test_bad_assign_type_error() {
        assert_eq!(run_main(r#"
            fn main() -> u32 {
                let a = 2;
                a = 3 == 4;
                return 2;
            }"#
        ).is_err(), true);
    }
}
