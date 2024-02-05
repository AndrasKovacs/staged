


fn foo(x : u64, y : u64) -> Option<u64> {
    if y == 0 {
        None
    } else {
        Some(x / y)
    }
}

fn main(){
    match foo(20,0){
        Some(x) => println!("{}", x),
        None    => println!("kek")
    }
}
