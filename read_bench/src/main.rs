use std::env::args;
use std::io::Read;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<_> = args().collect();
    let path = &args[1];
    let count = args[2].parse::<usize>().unwrap();

    for _ in 0..count {
        let mut file = std::fs::File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents);
        println!("{}", contents);
    }

    Ok(())
}
