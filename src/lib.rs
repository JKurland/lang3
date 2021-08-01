#![allow(dead_code)]

use chashmap::CHashMap;
use futures::channel::oneshot::{channel, Sender, Receiver};
use futures::executor::ThreadPool;
use inference::Type;
use itemise::{ItemPath, ItemType};
use std::cmp::{max, min};
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

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub(crate) struct SourceRef {
    start: usize,
    end: usize,
}

impl SourceRef {
    fn new(start: usize, len: usize) -> Self {
        Self {
            start,
            end: start + len,
        }
    }

    // includes self, o and everything in between
    pub(crate) fn to(&self, o: &SourceRef) -> SourceRef {
        SourceRef {
            start: min(self.start, o.start),
            end: max(self.end, o.end),
        }
    }

    pub(crate) fn lines<'a>(&self, source: &'a str) -> &'a str {
        let maybe_first_newline;
        if self.start < source.len() {
            maybe_first_newline = source[..self.start+1].rfind('\n');
        } else {
            panic!("SourceRef out of range");
        }

        let after_first_newline = match maybe_first_newline {
            Some(i) => i + 1,
            None => 0,
        };

        let last_newline;
        if self.end - 1 < source.len() {
            last_newline = source[self.end-1..].find('\n').unwrap_or(source.len() - self.end + 1) + self.end - 1;
        } else {
            panic!("SourceRef out of range");
        }

        if after_first_newline >= last_newline {
            ""
        } else {
            &source[after_first_newline..last_newline]
        }
    }

    pub(crate) fn text<'a>(&self, source: &'a str) -> &'a str {
        if self.start < source.len() && self.end <= source.len() {
            &source[self.start..self.end]
        } else {
            panic!("SourceRef out of range");
        }
    }

    pub(crate) fn empty_after(&self) -> Self {
        Self::new(self.end, 0)
    }

    pub(crate) fn empty_before(&self) -> Self {
        Self::new(self.start, 0)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum ErrorType {
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
    StructFieldMustEvaluate,
    ReturnValueMustEvaluate,
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
    StructMemberNameConflict(String, SourceRef),
    UnexpectedEndOfStruct,
    ExpectedFunctionSignature,
}


impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::TypeNotInferred => write!(f, "Type not inferred"),
            ErrorType::NoSuchItem(path) => write!(f, "No such item: {:?}", path),
            ErrorType::WrongItemType{path, expected, got} => write!(f, "Item {:?} was expected to be {:?}, was {:?}", path, expected, got),
            ErrorType::StructHasNoField(path, field_name) => write!(f, "Struct {:?} has no field {}", path, field_name),
            ErrorType::FieldAccessOnInvalidType(t) => write!(f, "Cannot do field access on object of type: {:?}", t),
            ErrorType::ExpectedFn{path, got} => write!(f, "Expected fn on path {:?}, got {:?}", path, got),
            ErrorType::UnreachableStatement => write!(f, "A statement in the code is unreachable"),
            ErrorType::BreakOutsideOfLoop => write!(f, "Break statements are only allowed in loops"),
            ErrorType::ParenGroupAsPrefix => write!(f, "Paren group not supported as prefix operator"),
            ErrorType::BraceGroupAsPrefix => write!(f, "Brace group not supported as prefix operator"),
            ErrorType::InvalidEmptyParens => write!(f, "Parens should not be empty"),
            ErrorType::FunctionCallRequiresIdent => write!(f, "Function calls currently only support function by name"),
            ErrorType::WrongNumberOfFunctionArgs{expected, got} => write!(f, "Wrong number of function args, expected: {}, got: {}", expected, got),
            ErrorType::NoStructNameBeforeBrace => write!(f, "Expected struct name before brace group"),
            ErrorType::StructInitRequiresIdent => write!(f, "Struct inits currently only support struct by name"),
            ErrorType::ExpectedStructFieldName => write!(f, "Expected struct field name"),
            ErrorType::InvalidComma => write!(f, "comma must be inside function arguments"),
            ErrorType::FunctionArgMustEvaluate => write!(f, "Function arg must evaluate"),
            ErrorType::StructFieldMustEvaluate => write!(f, "Struct field must evaluate"),
            ErrorType::ReturnValueMustEvaluate => write!(f, "Return value must evaluate"),
            ErrorType::SyntaxErrorExpected(tokens) => write!(f, "Syntax error, expected {:?}", tokens),
            ErrorType::SyntaxErrorUnexpected(tokens) => write!(f, "Syntax error, unexpected {:?}", tokens),
            ErrorType::UnexpectedEndOfFunction => write!(f, "Unexpected end of function"),
            ErrorType::NameRedefined(name) => write!(f, "Name {} redefined", name),
            ErrorType::UnknownName(name) => write!(f, "Unkown name {}", name),
            ErrorType::StringNotClosed => write!(f, "String literal not closed"),
            ErrorType::UnknownToken => write!(f, "Unkown token"),
            ErrorType::UnexpectedEof => write!(f, "Unexpected end of file"),
            ErrorType::TypeMismatch(a, b) => write!(f, "Type mismatch, {:?} and {:?}", a, b),
            ErrorType::SelfReferentialType => write!(f, "Could not infer types, self referential type"),
            ErrorType::StructMemberNameConflict(name, other_source_ref) => write!(f, "Two struct members have the same name {}: {:?}", name, other_source_ref),
            ErrorType::UnexpectedEndOfStruct => write!(f, "Unexpected end of struct"),
            ErrorType::ExpectedFunctionSignature => write!(f, "Expected function signature"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Error {
    t: ErrorType,
    source_ref: SourceRef,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {:?}\n    {}", self.source_ref, self.t)
    }
}

impl Error {
    #[allow(non_snake_case)]
    fn TypeNotInferred(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::TypeNotInferred,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn NoSuchItem(source_ref: SourceRef, item: ItemPath) -> Self {
        Self{
            t: ErrorType::NoSuchItem(item),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn WrongItemType(source_ref: SourceRef, path: ItemPath, expected: ItemType, got: ItemType) -> Self {
        Self{
            t: ErrorType::WrongItemType{path, expected, got},
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn StructHasNoField(source_ref: SourceRef, item: ItemPath, field: String) -> Self {
        Self{
            t: ErrorType::StructHasNoField(item, field),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn FieldAccessOnInvalidType(source_ref: SourceRef, t: Type) -> Self {
        Self{
            t: ErrorType::FieldAccessOnInvalidType(t),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn ExpectedFn(source_ref: SourceRef, path: ItemPath, got: ItemType) -> Self {
        Self{
            t: ErrorType::ExpectedFn{path, got},
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn UnreachableStatement(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::UnreachableStatement,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn BreakOutsideOfLoop(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::BreakOutsideOfLoop,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn ParenGroupAsPrefix(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::ParenGroupAsPrefix,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn BraceGroupAsPrefix(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::BraceGroupAsPrefix,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn InvalidEmptyParens(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::InvalidEmptyParens,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn FunctionCallRequiresIdent(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::FunctionCallRequiresIdent,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn WrongNumberOfFunctionArgs(source_ref: SourceRef, expected: usize, got: usize) -> Self {
        Self{
            t: ErrorType::WrongNumberOfFunctionArgs{expected, got},
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn NoStructNameBeforeBrace(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::NoStructNameBeforeBrace,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn StructInitRequiresIdent(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::StructInitRequiresIdent,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn ExpectedStructFieldName(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::ExpectedStructFieldName,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn InvalidComma(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::InvalidComma,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn FunctionArgMustEvaluate(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::FunctionArgMustEvaluate,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn StructFieldMustEvaluate(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::StructFieldMustEvaluate,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn ReturnValueMustEvaluate(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::ReturnValueMustEvaluate,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn SyntaxErrorExpected(source_ref: SourceRef, expected: Vec<&'static str>) -> Self {
        Self{
            t: ErrorType::SyntaxErrorExpected(expected),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn SyntaxErrorUnexpected(source_ref: SourceRef, unexpected: Vec<&'static str>) -> Self {
        Self{
            t: ErrorType::SyntaxErrorUnexpected(unexpected),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn UnexpectedEndOfFunction(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::UnexpectedEndOfFunction,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn NameRedefined(source_ref: SourceRef, name: String) -> Self {
        Self{
            t: ErrorType::NameRedefined(name),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn UnknownName(source_ref: SourceRef, name: String) -> Self {
        Self{
            t: ErrorType::UnknownName(name),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn StringNotClosed(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::StringNotClosed,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn UnknownToken(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::UnknownToken,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn UnexpectedEof(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::UnexpectedEof,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn TypeMismatch(source_ref: SourceRef, a: Type, b: Type) -> Self {
        Self{
            t: ErrorType::TypeMismatch(a, b),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn SelfReferentialType(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::SelfReferentialType,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn StructMemberNameConflict(source_ref: SourceRef, name: String, other_source: SourceRef) -> Self {
        Self{
            t: ErrorType::StructMemberNameConflict(name, other_source),
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn UnexpectedEndOfStruct(source_ref: SourceRef) -> Self {
        Self{
            t: ErrorType::UnexpectedEndOfStruct,
            source_ref,
        }
    }

    #[allow(non_snake_case)]
    fn ExpectedFunctionSignature(source_ref: SourceRef) -> Self {
        Self {
            t: ErrorType::ExpectedFunctionSignature,
            source_ref
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
    ($prog:expr, $query:expr, $query_source:expr) => { 
        {
            let prog: Arc<$crate::Program> = ($prog).clone();
            let query = $query;
            let query_source = $query_source;
            async move {
                prog.run_query(query.clone(), query.make(prog.clone(), query_source)).await
            }
        }
    };
}


mod tests {
    use super::*;
    use futures::executor::block_on;
    use vm::GetFunctionVmProgram;
    use itemise::ItemPath;

    #[test]
    fn test_source_ref() {
        let s = r#"a b
c


d
"#;

        assert_eq!(SourceRef::new(1, 1).lines(s), "a b");
        assert_eq!(SourceRef::new(3, 1).lines(s), "");
        assert_eq!(SourceRef::new(3, 2).lines(s), "c");
        assert_eq!(SourceRef::new(3, 4).lines(s), "c\n");
        assert_eq!(SourceRef::new(7, 2).lines(s), "d");
    }

    fn run_main(src: &str) -> Result<u32> {
        let prog = Arc::new(Program::new(src.to_string()));
        let program = block_on(make_query!(&prog, GetFunctionVmProgram{
            path: ItemPath::new("main"),
        }, SourceRef::new(0,0)))?;
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
