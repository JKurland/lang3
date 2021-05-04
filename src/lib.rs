#![allow(dead_code)]

use chashmap::CHashMap;
use futures::channel::oneshot::{channel, Sender, Receiver};
use futures::executor::ThreadPool;
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
pub(crate) mod vm;
pub(crate) mod storage;
pub(crate) mod structs;


#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Error {
    msg: String
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

impl Error {
    pub(crate) fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string()
        }
    }
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct CacheKey {
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

pub enum CacheEntry {
    Running(Shared<Receiver<()>>),
    Complete(Arc<dyn Any + Send + Sync>),
}

pub type ResultCache = CHashMap<CacheKey, CacheEntry>;

pub struct Program {
    pool: ThreadPool,
    result_cache: ResultCache,
    src: String,
}

async fn wrap_fut<T: Send + Sync + 'static, Fut: Future<Output = T>>(fut: Fut, sender: Sender<()>, prog: Arc<Program>, cache_key: CacheKey) {
    let result = Arc::new(fut.await);
    let mut cache_entry = prog.result_cache.get_mut(&cache_key).unwrap();
    sender.send(()).expect("Receiver was dropped");
    *cache_entry = CacheEntry::Complete(result);
}

impl Program {
    pub fn new(src: String) -> Self {
        Self {
            pool: ThreadPool::new().unwrap(),
            result_cache: CHashMap::new(),
            src,
        }
    }

    pub async fn run_query<Q: Query, T: Sync + Send + 'static, Fut>(self: &Arc<Self>, query: Q, future: Fut) -> Arc<T>
    where
    Fut: Future<Output = T> + Send + 'static {
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
                CacheEntry::Complete(_) => None,
            }
        };

        if let Some(fut) = result_fut {
            fut.await.expect("Query was cancelled");
        }

        let result_item = self.result_cache.get(&cache_key).unwrap();
        match *result_item {
            CacheEntry::Running(_) => panic!("Result is still not ready"),
            CacheEntry::Complete(ref result) => result.clone().downcast::<T>().unwrap()
        }
    }

    pub fn get_src(&self) -> &str {
        &self.src
    }
}

pub trait Query: Send + Sync + Clone + Hash + PartialEq + 'static {
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
        let program_arc = block_on(make_query!(&prog, GetFunctionVmProgram{
            path: ItemPath::new("main"),
        }));

        if program_arc.is_err() {
            return Err(program_arc.as_ref().as_ref().unwrap_err().clone());
        }
        let program = program_arc.as_ref().as_ref().unwrap();

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
}