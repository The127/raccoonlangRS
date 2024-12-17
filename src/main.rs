mod source_map;
mod put_back_iterator;
mod tokenizer;
mod treeizer;

fn main() {
    println!("Hello, world!");
    dbg!(icu::properties::sets::id_start().contains('1'));
    dbg!(icu::properties::sets::emoji().contains('1'));
    dbg!(icu::properties::sets::id_continue().contains('゛'));
}

#[cfg(test)]
mod test {
    #[test]
    fn test(){
        println!("hello!")
    }
}