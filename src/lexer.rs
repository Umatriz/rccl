use std::ops::{Range, RangeBounds};

pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut t = Vec::new();
        let mut chars = source.chars().peekable();

        use Token::*;
        while let Some(c) = chars.next() {
            match c {
                '\n' => {}
                '\r' => {}
                ' ' => {}
                '\t' => {}

                '_' => t.push(Hole),
                '^' => t.push(PushZero),
                '+' => t.push(Increment),
                '-' => t.push(Decrement),
                '*' => t.push(Add),
                '~' => t.push(Subtract),
                '%' => t.push(Reverse),
                '=' => t.push(Assign),
                '!' => t.push(Delete),
                '$' => t.push(Push),
                '&' => t.push(AssignLocal),
                '<' => t.push(Output),
                '>' => t.push(Input),
                '@' => t.push(Call),
                '#' => t.push(End),
                ':' => t.push(Continue),
                '?' => t.push(QuestionMark),
                ';' => t.push(Semi),

                '(' => t.push(LeftParen),
                ')' => t.push(RightParen),
                '[' => t.push(LeftBracket),
                ']' => t.push(RightBracket),
                '{' => t.push(LeftBrace),
                '}' => t.push(RightBrace),

                '/' => while chars.next_if(|c| *c != '\n' || *c != '\r').is_some() {},

                c if c.is_ascii_alphabetic() => {
                    t.push(Ident(c));
                }
                t => panic!("Failed to parse token {t}"),
            }
        }

        Self { tokens: t }
    }
}

pub struct TokensIter<'a> {
    tokens: &'a [Token],
    /// Index of the **next** token
    cursor: usize,
    valid_range: Range<usize>,
}

impl<'a> TokensIter<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: 0,
            valid_range: 0..tokens.len(),
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.cursor).copied()
    }

    pub fn next_indiced(&mut self) -> Option<(usize, Token)> {
        self.next().map(|t| (self.cursor - 1, t))
    }

    pub fn peek_indiced(&mut self) -> Option<(usize, Token)> {
        self.peek().map(|t| (self.cursor, t))
    }

    pub fn child(&self, range: Range<usize>) -> TokensIter<'_> {
        TokensIter {
            tokens: self.tokens,
            cursor: range.start,
            valid_range: range,
        }
    }

    pub fn move_cursor_to(&mut self, place: usize) {
        self.cursor = place;
    }

    pub fn cursor_position(&self) -> usize {
        self.cursor
    }

    pub fn current_token_index(&self) -> usize {
        self.cursor_position() - 1
    }
}

impl Iterator for TokensIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.cursor;

        self.cursor = (self.cursor + 1).min(self.valid_range.end);

        if self.valid_range.contains(&idx) {
            self.tokens.get(idx).copied()
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    /// _
    Hole,
    /// ^
    PushZero,
    /// +
    Increment,
    /// -
    Decrement,
    /// *
    Add,
    /// ~
    Subtract,
    /// %
    Reverse,
    /// =
    Assign,
    /// !
    Delete,
    /// $
    Push,
    /// &
    AssignLocal,
    /// <
    Output,
    /// >
    Input,
    /// @
    Call,
    /// #
    End,
    /// :
    Continue,
    /// ?
    QuestionMark,
    /// ;
    Semi,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    Ident(char),

    Eof,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter_test() {
        let mut iter = TokensIter::new(&[
            Token::Hole,
            Token::PushZero,
            Token::Increment,
            Token::Hole,
            Token::PushZero,
            Token::Increment,
        ]);

        assert_eq!(iter.next(), Some(Token::Hole));

        assert_eq!(iter.peek_indiced(), Some((1, Token::PushZero)));
        assert_eq!(iter.next(), Some(Token::PushZero));

        assert_eq!(iter.next(), Some(Token::Increment));

        assert_eq!(iter.peek(), Some(Token::Hole));
        assert_eq!(iter.next(), Some(Token::Hole));

        assert_eq!(iter.next(), Some(Token::PushZero));
        assert_eq!(iter.next(), Some(Token::Increment));
        assert_eq!(iter.peek(), None);
        assert_eq!(iter.next(), None);

        let mut child = iter.child(3..5);
        assert_eq!(child.next(), Some(Token::Hole));
        assert_eq!(child.next_indiced(), Some((4, Token::PushZero)));
        assert_eq!(child.next(), None);
    }
}
