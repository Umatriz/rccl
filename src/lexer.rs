pub struct Lexer {
    /// Tokens stored in a reversed order so it is possible to `pop` and get the first token.
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
                ']' => t.push(RihgtBracket),
                '{' => t.push(LeftBrace),
                '}' => t.push(RightBrace),
                c if c.is_ascii_digit() => {
                    let mut s = String::from(c);
                    while let Some(c) = chars.next_if(|c| c.is_ascii_digit()) {
                        s.push(c);
                    }
                    let num = s.parse::<i16>().expect("Failed to parse number");
                    t.push(Number(num))
                }
                c if c.is_ascii_alphabetic() => {
                    t.push(Ident(c));
                }
                t => panic!("Failed to parse token {t}"),
            }
        }

        t.reverse();
        Self { tokens: t }
    }

    pub fn next_token(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    pub fn peek_token(&self) -> Token {
        self.tokens.last().copied().unwrap_or(Token::Eof)
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
    RihgtBracket,
    LeftBrace,
    RightBrace,

    Ident(char),
    Number(i16),

    Eof,
}
