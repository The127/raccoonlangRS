mod source_map;

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    #[test]
    fn test(){
        println!("hello!")
    }
}