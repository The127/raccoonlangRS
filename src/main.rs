mod source_map;
mod tokenizer;
mod treeizer;
mod parser;
mod marking_iterator;
mod errors;
mod ast;


fn main() {
    let mut sources = source_map::SourceCollection::new();
    let mut errors = errors::Errors::new();

    let input = "use foo::bar; mod foo; fn qux (a: int, b: int) -> int { 10 }";
    let span = sources.load_content(input);
    let tokenizer = tokenizer::tokenize(span, &sources);
    let tt = treeizer::treeize(tokenizer);
    let mut iter = marking_iterator::marking(tt.iter());
    let file_node = parser::file_node::parse_file(&mut iter, &mut errors);
    let file = ast::file::transform_file(&file_node, &sources);

    dbg!(file);
}

#[cfg(test)]
mod test {
    #[test]
    fn test(){
        println!("hello!")
    }
}