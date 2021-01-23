use std::sync::Arc;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use futures::executor::ThreadPool;
use chashmap::CHashMap;
use futures::executor::block_on;
use futures::channel::oneshot::{channel, Sender, Receiver};

use lang3::*;

#[derive(Clone, Hash, PartialEq, Debug)]
struct MyQuery {
    i: i32,
}

impl Query for MyQuery {
    type Output = i32;
}

impl MyQuery {
    async fn make(self, _prog: Arc<Program>) -> <Self as Query>::Output {
        self.i + 1
    }
}

#[derive(Clone, Hash, PartialEq)]
struct MyOtherQuery {
    i: usize,
}

impl Query for MyOtherQuery {
    type Output = i32;
}

impl MyOtherQuery {
    async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        let mut ans = 0;
        for _ in 0..self.i {
            ans = *make_query!(&prog, MyQuery{i: 2}).await
        }
        ans
    }
}

fn repeated_query(n: usize) -> i32 {
    let prog = Arc::new(Program::new("source".to_string()));

    let ans = *block_on(make_query!(&prog, MyOtherQuery{i: n}));
    ans
}

pub fn query_overhead(c: &mut Criterion) {
    c.bench_function("repeated query 2000", |b| b.iter(|| repeated_query(black_box(2000))));
    c.bench_function("repeated query 2", |b| b.iter(|| repeated_query(black_box(2))));
    c.bench_function("repeated query 1", |b| b.iter(|| repeated_query(black_box(1))));
}


fn one_task(n: i32) -> i32 {
    let pool = ThreadPool::new().unwrap();

    let (sender, receiver) = channel();
    
    pool.spawn_ok(async {sender.send(()).unwrap()});
    block_on(receiver).unwrap();
    n
}

pub fn thread_overhead(c: &mut Criterion) {
    c.bench_function("one task in pool", |b| b.iter(|| one_task(black_box(1))));
}


criterion_group!(benches, query_overhead, thread_overhead);
criterion_main!(benches);
