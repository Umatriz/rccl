use std::{
    fmt::Display,
    io::SeekFrom,
    ops::{Range, RangeBounds},
};

pub struct Lexer {
    tokens: Vec<(Token, TokenContext)>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut t = Vec::new();
        let mut chars = source.chars().enumerate().peekable();

        use Token::*;

        let mut contexts = Vec::new();
        let mut current_line = 0;
        let mut current_column = 0;
        while let Some((idx, c)) = chars.next() {
            current_column += 1;
            match c {
                '\n' => {
                    current_line += 1;
                    current_column = 0;
                    continue;
                }
                '\r' if chars.peek().is_some_and(|(_, c)| *c == '\n') => {
                    chars.next();
                    current_line += 1;
                    current_column = 0;
                    continue;
                }
                '\r' => {
                    current_line += 1;
                    current_column = 0;
                    continue;
                }
                ' ' => {
                    continue;
                }
                '\t' => {
                    current_column += 3;
                    continue;
                }

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

                '/' => {
                    while chars.next_if(|(_, c)| *c != '\n' && *c != '\r').is_some() {}
                    // Since comments don't have a token we skip a context
                    continue;
                }

                c if c.is_ascii_alphabetic() => {
                    t.push(Ident(c));
                }
                t => panic!("Failed to parse token {t}"),
            }

            contexts.push(TokenContext {
                line: current_line,
                // Hope this is right
                column: current_column,
            });
        }

        Self {
            tokens: t.into_iter().zip(contexts).collect(),
        }
    }

    pub fn iter_tokens(&self) -> TokensIter<'_> {
        TokensIter::new(&self.tokens)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenContext {
    pub line: usize,
    pub column: usize,
}

impl Display for TokenContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

pub struct TokensIter<'a> {
    tokens: &'a [(Token, TokenContext)],
    /// Index of the **next** token
    cursor: usize,
    valid_range: Range<usize>,
}

impl<'a> TokensIter<'a> {
    pub fn new(tokens: &'a [(Token, TokenContext)]) -> Self {
        Self {
            tokens,
            cursor: 0,
            valid_range: 0..tokens.len(),
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.cursor).map(|(t, _c)| t).copied()
    }

    pub fn next_indiced(&mut self) -> Option<(usize, Token)> {
        self.next().map(|t| (self.cursor - 1, t))
    }

    pub fn peek_indiced(&mut self) -> Option<(usize, Token)> {
        self.peek().map(|t| (self.cursor, t))
    }

    pub fn get_context(&self, idx: usize) -> Option<TokenContext> {
        self.tokens.get(idx).map(|(_t, c)| c).copied()
    }

    pub fn context(&self, idx: usize) -> TokenContext {
        self.tokens[idx].1
    }

    /// Contexet offeseted by `offset` from the current token.
    pub fn context_offset(&self, offset: isize) -> TokenContext {
        let idx = self.current_token_index() as isize + offset;
        self.tokens[idx as usize].1
    }

    pub fn current_context(&self) -> TokenContext {
        self.tokens[self.cursor - 1].1
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

    pub fn cursor_index(&self) -> usize {
        self.cursor
    }

    pub fn current_token_index(&self) -> usize {
        self.cursor_index() - 1
    }
}

impl Iterator for TokensIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.cursor;

        self.cursor = (self.cursor + 1).min(self.valid_range.end);

        if self.valid_range.contains(&idx) {
            self.tokens.get(idx).map(|(t, _c)| t).copied()
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
