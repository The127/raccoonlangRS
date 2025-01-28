use ustr::Ustr;


// TODO: what about spans?
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Pattern {
    Discard,
    Name(Ustr),
    Tuple(Vec<Pattern>),
}