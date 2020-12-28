# Query Based #
Queries form a dag
Query results must be cached, so queries must be hashable.

How to keep the query dag manageable?
How to handle compiler errors? Any query might find errors? Separate phase to check for errors? Separate phase will lead to coupling between error checking and code generation. This means another representation of the language will be needed since the code representation won't allow coupled code to be close together.
Any Query finding errors isn't good either, it will spread error checking out too much.

Queries find errors that would get in the way of the query completing?
Analysis phases check for programmer errors?
Which is the type checker? Not great.

Maybe GetCompileErrors query? I like this best so far.

Some queries find errors, some produce results. Queries that produce results can call queries that find errors and vice versa. How should errors be reported?

Languages seem to like to "keep going" when there is a problem to find more problems. I don't really see the point, further errors tend to be caused by previous errors and mostly just get in the way. Especially if this compiler keeps in memory cache across invocations so that compiling again to get the next error is fast.

Having said that reporting multiple errors so long as they come from independent sub dags should be fine. So this would be like make's keep going.

Maybe some error queries can contextually return multiple errors if it makes sense, for example the parser might be able to do that.

So:
  * Error type that can contain multiple errors
  * The dag collects errors "monadically", so A(B, C) either works if both B and C are Ok, or combines the errors of B and C (either one, the other, or both)
  * The top level of the program can decide to show the errors from the top level query

This fits well with rust's "?" and might work with join!, or I can make my own join!.

## Managing the dag ##
Can only really check for cycles at runtime since some programs might have cycles. `Program` can keep a stack trace of queries to check for cycles.

```rust
trait Query {
    type Output;
}

impl Query for MyQuery {
    type Output = i32;
}

impl MyQuery {
    // can't be in Query trait because async can't be in trait
    async fn make(&self, prog: mut Program, sender: Sender) {
        // make_query is a macro because "make" is not in the Query trait because it is async. So we need to use a macro for duck typing.
        let result = make_query!(prog, OtherQuery{a: 1, b: 2, c: "hello"}).await;
        sender.send(result);
    }
}

make_query(prog, query) => async {
    // receiver is a future, when its poll method is called it checks if a value
    // has already been sent, if not then it sets the waker for sender. When a value
    // is given to sender it will set the return value for receiver and call
    // the waker (if it's been set).
    let (sender, receiver) = make_oneshot_channel(query);
    let fut = query.make(prog.clone(), sender);
    prog.spawn_ok(query, fut);
    let result = receiver.await;
    result
}
```

## Managing the Cache ##
Each entry in the cache could specify which section of the source it relates to, this way whenever the source changes i can quickly strip the cache. The range of the source each query result depends on can be recorded by Program while the query is running.

It might also be necessary to clean the cache to reduce memory usage. LRU will probably work well enough for this. Maybe also reporting to the user that the compiler is low on memory.

Can I use the cache to own the query results? Lifetimes? 

## Builtin Queries ##
The idea is to keep the number of these queries low.
Can these queries be made to look like normal queries?
Should these actually just be methods on Program?
Program will need to split the source up into Items itself so that queries have more precise dependencies. This shouldn't be too bad since the top level syntax can be kept simple.

```rust
impl Program {
    fn source_files(&self) -> &[SourceFile];
    fn find_item(&self, path: &Path, location: &Item) -> &Item;
    fn text(&mut self, item: &Item) -> SourceRange;
}

```
