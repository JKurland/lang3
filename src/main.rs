use libffi::middle::{Arg, Builder, Type, CodePtr};
use libloading::Library;

fn main() -> Result<(), Box<dyn std::error::Error>>{
    let lib = Library::new("./test.so")?;
    let f = unsafe {lib.get::<unsafe extern fn()>(b"f")?};

    let cif = Builder::new()
        .arg(Type::i32())
        .res(Type::i32())
        .into_cif();

    let i = 2;
    let arg = Arg::new(&i);
    unsafe {
        println!("got {}", cif.call::<i32>(CodePtr::from_fun(*f), &[arg]));
    }

    Ok(())
}
