mod source_map;
mod tokenizer;
mod treeizer;
mod parser;
mod marking_iterator;
mod errors;

fn main() {
    let mut sources = source_map::SourceCollection::new();
    let mut errors = errors::Errors::new();

    let input = "use foo::bar; mod foo; fn foo (a: int, b: int) -> int { 10 }".to_string();
    let span = sources.load_content(input);
    let tokenizer = tokenizer::tokenize(span, &sources);
    let tt = treeizer::treeize(tokenizer);
    let mut iter = marking_iterator::marking(tt.iter());
    let file_node = parser::file_node::parse_file(&mut iter, &mut errors);
    dbg!(file_node);
}

#[cfg(test)]
mod test {
    #[test]
    fn test(){
        println!("hello!")
    }
}