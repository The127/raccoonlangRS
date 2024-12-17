use crate::tokenizer::TokenType::*;
use crate::tokenizer::{Token, TokenType};
use std::fmt::{Debug, Formatter};

pub fn treeize<I>(tokens: I) -> Vec<TokenTree>
where
    I: Iterator<Item = Token>,
{
    let mut stack = TokenStack::new();

    for token in tokens {
        stack.push(token);
    }

    stack.into_vec()
}

struct TokenStack {
    entries: Vec<TokenStackEntry>,
}

#[derive(Debug)]
struct TokenStackEntry {
    open: Token,
    children: Vec<TokenTree>,
    expected_closer: TokenType,
}

impl TokenStack {
    fn new() -> Self {
        Self {
            entries: vec![TokenStackEntry {
                open: Token::unknown(),
                children: vec![],
                expected_closer: Unknown,
            }],
        }
    }

    fn close(&mut self, closer: Option<Token>) {
        let mut popped_top = self.entries.pop().expect("stack is empty");

        let dest = &mut self.entries
            .last_mut()
            .expect("stack is empty")
            .children;

        if closer.is_none() && popped_top.open.token_type == OpenAngle {
            dest.push(TokenTree::Token(Token {
                token_type: LessThan,
                span: popped_top.open.span,
            }));
            dest.append(&mut popped_top.children);
        } else {
            dest.push(TokenTree::Group(Group {
                open: popped_top.open,
                children: popped_top.children,
                close: closer,
            }));
        }
    }

    fn open_or_append(&mut self, token: Token) {
        if let Some(closer) = get_closer(token.token_type) {
            self.entries.push(TokenStackEntry {
                open: token,
                children: vec![],
                expected_closer: closer,
            });
        } else {
            let token = if token.token_type == CloseAngle {
                Token {
                    token_type: GreaterThan,
                    span: token.span,
                }
            } else {
                token
            };

            self.entries
                .last_mut()
                .expect("stack is empty")
                .children
                .push(TokenTree::Token(token));
        }
    }

    fn push(&mut self, token: Token) {
        let closing_idx = self
            .entries
            .iter()
            .enumerate()
            .rfind(|(_, entry)| entry.expected_closer == token.token_type)
            .map(|(idx, _)| idx);

        if let Some(idx) = closing_idx {
            for _ in (idx+1)..self.entries.len() {
                self.close(None);
            }
            self.close(Some(token));
        }else{
            self.open_or_append(token);
        }
    }

    fn into_vec(mut self) -> Vec<TokenTree> {
        for _ in 1..self.entries.len() {
            self.close(None);
        }
        return self.entries.pop().expect("empty stack").children;
    }
}

fn get_closer(token_type: TokenType) -> Option<TokenType> {
    match token_type {
        OpenCurly => Some(CloseCurly),
        OpenAngle => Some(CloseAngle),
        OpenSquare => Some(CloseSquare),
        OpenParen => Some(CloseParen),
        _ => None,
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Group {
    pub open: Token,
    pub children: Vec<TokenTree>,
    pub close: Option<Token>,
}

impl Debug for Group {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(close) = self.close {
            write!(f, "{{{:?}, {:?}, {:?}}}", self.open, self.children, close)
        } else {
            write!(
                f,
                "{{{:?}, {:?}, -}}",
                self.open, self.children
            )
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum TokenTree {
    Token(Token),
    Group(Group),
}

impl Debug for TokenTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTree::Token(token) => write!(f, "{:?}", token),
            TokenTree::Group(group) => write!(f, "{:?}", group),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::treeizer::*;
    use paste::paste;

    #[test]
    fn treeize_simple_sequence() {
        // arrange
        let token1 = Token {
            token_type: Identifier,
            span: (0..5).into(),
        };
        let token2 = Token {
            token_type: Identifier,
            span: (6..11).into(),
        };

        // act
        let token_stream = treeize(vec![token1, token2].into_iter());

        // assert
        assert_eq!(
            token_stream,
            vec![TokenTree::Token(token1), TokenTree::Token(token2),]
        );
    }

    #[test]
    fn treeize_simple_tree() {
        // arrange
        let open_curly = Token {
            token_type: OpenCurly,
            span: (4..5).into(),
        };
        let identifier = Token {
            token_type: Identifier,
            span: (6..11).into(),
        };
        let close_curly = Token {
            token_type: CloseCurly,
            span: (13..14).into(),
        };

        // act
        let token_stream = treeize(vec![open_curly, identifier, close_curly].into_iter());

        // assert
        assert_eq!(
            token_stream,
            vec![TokenTree::Group(Group {
                open: open_curly,
                children: vec![TokenTree::Token(identifier)],
                close: Some(close_curly),
            })]
        );
    }

    macro_rules! tree_test {
        ($($name:ident: [$($input:ident),*] -> [$($expected:tt),*];)*) => {
        $(
            paste! {
                #[test]
                fn [<treeize_ $name >] () {
                    // arrange
                    let tokens = vec![
                        $(
                        Token {
                            token_type: $input,
                            span: (0..0).into(),
                        }
                        ),*
                    ];

                    // act
                    let token_stream = treeize(tokens.into_iter());

                    // assert
                    assert_eq!(token_stream, vec! [$(token_builder!($expected),)*])
                }
            }
        )*
        }
    }

    macro_rules! token_builder {
        ($name:ident) => {
            TokenTree::Token(Token{
                token_type: $name,
                span: (0..0).into(),
            })
        };
        ({$open:ident, [$($child:tt),*], $close:ident}) => {
                TokenTree::Group(Group {
                open: Token{
                    token_type: $open,
                    span: (0..0).into(),
                },
                children: vec![$(token_builder!($child)),*],
                close: Some(Token{
                    token_type: $close,
                    span: (0..0).into(),
                }),
            })
        };
        ({$open:ident, [$($child:tt),*], -}) => {
                TokenTree::Group(Group {
                open: Token{
                    token_type: $open,
                    span: (0..0).into(),
                },
                children: vec![$(token_builder!($child)),*],
                close: None,
            })
        }
    }

    tree_test!(
        shrimple: [OpenCurly, Identifier, CloseCurly] -> [{OpenCurly, [Identifier], CloseCurly}];
        not_so_shrimple: [
            Identifier, OpenCurly,
                Identifier, OpenCurly,
                    Identifier, Identifier,
                CloseCurly,
            CloseCurly, Identifier
        ] -> [Identifier, {OpenCurly, [Identifier, {OpenCurly, [Identifier, Identifier], CloseCurly}], CloseCurly}, Identifier];
        missing_closer: [OpenCurly] -> [{OpenCurly, [], -}];
        missing_nested_closer: [OpenCurly, OpenParen, CloseCurly] -> [{OpenCurly, [{OpenParen, [], -}], CloseCurly}];
        greater_than: [CloseAngle] -> [GreaterThan];
        less_than: [OpenAngle] -> [LessThan];
        contained_less_than: [OpenCurly, OpenAngle, CloseCurly] -> [{OpenCurly, [LessThan], CloseCurly}];
        contained_greater_than: [OpenCurly, CloseAngle, CloseCurly] -> [{OpenCurly, [GreaterThan], CloseCurly}];
        contained_less_than_with_extra: [OpenCurly, OpenAngle, Identifier, CloseCurly] -> [{OpenCurly, [LessThan, Identifier], CloseCurly}];
        contained_less_than_with_extra_nested: [OpenCurly, OpenAngle, OpenParen, Identifier, CloseParen, CloseCurly] -> [{OpenCurly, [LessThan, {OpenParen, [Identifier], CloseParen}], CloseCurly}];
    );
}
