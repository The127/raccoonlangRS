use crate::tokenizer::TokenType::{CloseAngle, CloseCurly, CloseParen, CloseSquare, Unknown};
use crate::tokenizer::{Token, TokenType};
use std::fmt::{Debug, Formatter};
use paste::paste;

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

    fn expected_closer(&self) -> TokenType {
        self.entries.last().expect("stack is empty").expected_closer
    }

    fn close(&mut self, closer: Token) {
        let popped_top = self.entries.pop().expect("stack is empty");

        if popped_top.expected_closer != closer.token_type {
            panic!("wrong closing token type");
        }

        self.entries
            .last_mut()
            .expect("stack is empty")
            .children
            .push(TokenTree::Group(Group {
                open: popped_top.open,
                children: popped_top.children,
                close: closer,
            }));
    }

    fn open_or_append(&mut self, token: Token) {
        if let Some(closer) = get_closer(token.token_type) {
            self.entries.push(TokenStackEntry {
                open: token,
                children: vec![],
                expected_closer: closer,
            });
        } else {
            self.entries
                .last_mut()
                .expect("stack is empty")
                .children
                .push(TokenTree::Token(token));
        }
    }

    fn push(&mut self, token: Token) {
        if token.token_type == self.expected_closer() {
            self.close(token);
        } else {
            self.open_or_append(token);
        }
    }

    fn into_vec(mut self) -> Vec<TokenTree> {
        if self.entries.len() > 1 {
            panic!("unbalanced");
        }
        return self.entries.pop().expect("empty stack").children;
    }
}

fn get_closer(token_type: TokenType) -> Option<TokenType> {
    match token_type {
        TokenType::OpenCurly => Some(CloseCurly),
        TokenType::OpenAngle => Some(CloseAngle),
        TokenType::OpenSquare => Some(CloseSquare),
        TokenType::OpenParen => Some(CloseParen),
        _ => None,
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Group {
    pub open: Token,
    pub children: Vec<TokenTree>,
    pub close: Token,
}

impl Debug for Group {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{:?}, {:?}, {:?}}}",
            self.open, self.children, self.close
        )
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum TokenTree {
    Token(Token),
    Group(Group),
}

impl TokenTree {
    fn unwrap_group(self) -> Group {
        match self {
            TokenTree::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    fn unwrap_token(self) -> Token {
        match self {
            TokenTree::Token(token) => token,
            _ => panic!("expected token"),
        }
    }
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
    use crate::tokenizer::TokenType::{CloseCurly, Equals, Identifier, OpenCurly};
    use crate::treeizer::*;

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
                close: close_curly,
            })]
        );
    }

    macro_rules! tree_test {
        ($($name:ident: [$($input:ident),*] -> [$($expected:tt),*]),*) => {
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
                    assert_eq!(token_stream, vec! [$(token_thingie!($expected),)*])
                }
            }
        )*
        }
    }

    macro_rules! token_thingie {
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
                children: vec![$(token_thingie!($child)),*],
                close: Token{
                    token_type: $close,
                    span: (0..0).into(),
                },
            })
        }
    }

    tree_test!(
        shrimple: [OpenCurly, Identifier, CloseCurly] -> [{OpenCurly, [Identifier], CloseCurly}],
        not_so_shrimple: [
            Identifier, OpenCurly,
                Identifier, OpenCurly,
                    Identifier, Identifier,
                CloseCurly,
            CloseCurly, Identifier
        ] -> [Identifier, {OpenCurly, [Identifier, {OpenCurly, [Identifier, Identifier], CloseCurly}], CloseCurly}, Identifier]
    );
}
