use chashmap::CHashMap;
use futures::channel::oneshot::{channel, Sender, Receiver};
use futures::executor::{ThreadPool, block_on};
use std::future::Future;
use std::collections::hash_map;
use futures::future::{FutureExt, Shared};

use core::panic;
use std::error::Error;
use std::hash::{Hash, Hasher};
use std::any::Any;
use std::sync::Arc;
use core::borrow::Borrow;

#[derive(Clone)]
struct CacheKey {
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

enum CacheEntry {
    Running(Shared<Receiver<()>>),
    Complete(Arc<dyn Any + Send + Sync>),
}

type ResultCache = CHashMap<CacheKey, CacheEntry>;

#[derive(Clone)]
struct Program {
    pool: &'static ThreadPool,
    result_cache: &'static ResultCache,
}

async fn wrap_fut<T: Send + Sync + 'static, Fut: Future<Output = T>>(fut: Fut, sender: Sender<()>, cache: &'static ResultCache, cache_key: CacheKey) {
    cache.insert(cache_key, CacheEntry::Complete(Arc::new(fut.await)));
    sender.send(()).expect("Receiver was dropped");
}

impl Program {
    fn new(pool: &'static ThreadPool, result_cache: &'static ResultCache) -> Self {
        Self {
            pool,
            result_cache,
        }
    }

    async fn run_query<Q: Query, T: Sync + Send + 'static, Fut>(&self, query: Q, future: Fut) -> Arc<T>
    where
    Fut: Future<Output = T> + Send + 'static {
        let cache_key = CacheKey::new(query);
        self.result_cache.upsert(
            cache_key.clone(),
            || {
                let (sender, receiver) = channel();
                let fut = wrap_fut(future, sender, self.result_cache, cache_key.clone());
                self.pool.spawn_ok(fut);
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
}

trait Query: Send + Sync + Clone + Hash + PartialEq + 'static {
    // Queries need to be Send, Sync, and cheap to Clone
    type Output;
}

macro_rules! make_query {
    ($prog:expr, $query:expr) => { 
        async {
            let prog = &$prog;
            let query = $query;
            prog.run_query(query.clone(), query.make(prog.clone())).await
        }
    };
}

#[derive(Clone, Hash, PartialEq, Debug)]
struct MyQuery {
    i: i32,
}

impl Query for MyQuery {
    type Output = i32;
}

impl MyQuery {
    async fn make(self, _prog: Program) -> <Self as Query>::Output {
        println!("Running: {:?}", &self);
        self.i + 1
    }
}

#[derive(Clone, Hash, PartialEq)]
struct MyOtherQuery {
    i: i32,
}

impl Query for MyOtherQuery {
    type Output = i32;
}

impl MyOtherQuery {
    async fn make(self, prog: Program) -> <Self as Query>::Output {
        (*make_query!(prog, MyQuery{i: self.i}).await) * 2 +
        (*make_query!(prog, MyQuery{i: self.i + 1}).await) * 3 + 
        (*make_query!(prog, MyQuery{i: self.i}).await) * 2 +
        (*make_query!(prog, MyQuery{i: self.i + 1}).await) * 3
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let pool = ThreadPool::new()?;
    let result_cache = CHashMap::new();

    // Safety: All futures must run to completion before the end of main. This is assured so long
    // as all tasks are spawned using make_query. Since the async function that calls make_query will
    // always outlive the tasks that are spawned within make_query spawns and the top level task doesn't
    // outlive the pool, no tasks will outlive the objects created at the top of main.
    let prog = Program::new(
        unsafe {std::mem::transmute::<&'_ ThreadPool, &'static ThreadPool>(&pool)},
        unsafe {std::mem::transmute::<&'_ ResultCache, &'static ResultCache>(&result_cache)}
    );

    let ans = block_on(make_query!(prog, MyOtherQuery{i: 2}));
    // All tasks in the threadpool are now finished.

    dbg!(ans);
    Ok(())
}
