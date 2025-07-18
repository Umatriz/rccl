use std::io::Write;
use std::ops::Range;
use std::{collections::HashMap, io::Read};

use crate::lexer::{Lexer, Token, TokensIter};

#[derive(Debug, Default)]
pub struct Stack(Vec<i16>);

impl Stack {
    pub fn push(&mut self, val: i16) {
        self.0.push(val);
    }

    pub fn pop(&mut self) -> Option<i16> {
        self.0.pop()
    }

    pub fn top_mut(&mut self) -> Option<&mut i16> {
        self.0.last_mut()
    }
}

#[derive(Debug, Default)]
pub struct Variables(HashMap<char, i16>);

#[derive(Debug, Default)]
pub struct Procedures(HashMap<char, Procedure>);

#[derive(Clone)]
pub struct Procedure {
    body_span: Range<usize>,
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Procedure").field("body", &"{...}").finish()
    }
}

pub struct InterpretationData<I, O> {
    pub stack: Stack,
    /// Output from `Output` calls.
    pub out: O,
    /// Input to read from.
    pub inp: I,
    pub globals: Variables,
    pub locals: Option<Variables>,
    pub procedures: Procedures,
}

impl<I, O> InterpretationData<I, O> {
    /// Attemps to get local variable if it doesn't exists then tries to get a global
    /// one with the same name. Returns `None` in case of a falure.
    pub fn get_local_or_global(&self, name: char) -> Option<i16> {
        self.locals
            .as_ref()
            .and_then(|locals| locals.0.get(&name).copied())
            .or_else(|| self.globals.0.get(&name).copied())
    }

    /// See [`get_local_or_global`] for more information.
    pub fn get_local_or_global_mut(&mut self, name: char) -> Option<&mut i16> {
        self.locals
            .as_mut()
            .and_then(|locals| locals.0.get_mut(&name))
            .or_else(|| self.globals.0.get_mut(&name))
    }

    pub fn set_locals(&mut self, locals: Variables) {
        self.locals = Some(locals);
    }

    pub fn clear_locals(&mut self) {
        self.locals = None
    }
}

/// What was the reason to end interpretation.
#[derive(Debug, PartialEq, Eq)]
pub enum Output {
    /// `End` (`#`) token was met.
    End,
    /// Continue `:` token was met.
    Continue,
    /// `Eof` was met.
    Eof,
}

#[derive(Debug)]
pub enum Error {
    StackIsEmpty,
    UnexpectedToken { got: Token, expected: &'static str },
    VariableDoesNotExists(char),
    ProcedureDoesNotExists(char),
    NegativeNumberFound,
    MustBeProcedure,
    InvalidAsciiCode,
    FailedToWrite,
    FailedToRead,
    NonAsciiCharacter(char),
    UnclosedInfiniteBlock,
}

pub fn interpret<I, O>(
    tokens: &mut TokensIter,
    data: &mut InterpretationData<I, O>,
) -> Result<Output, Error>
where
    I: Read,
    O: Write,
{
    use self::Output as Out;
    use Token::*;

    let out = loop {
        match tokens.next().unwrap_or(Token::Eof) {
            // Hole => todo!(),
            PushZero => {
                data.stack.push(0);
            }
            Increment => {
                let Some(top) = data.stack.top_mut() else {
                    return Err(Error::StackIsEmpty);
                };

                *top += 1;
            }
            Decrement => {
                let Some(top) = data.stack.top_mut() else {
                    return Err(Error::StackIsEmpty);
                };

                *top -= 1;
            }
            Add => {
                let (Some(top), Some(next)) = (data.stack.pop(), data.stack.top_mut()) else {
                    return Err(Error::StackIsEmpty);
                };

                *next += top;
            }
            Subtract => {
                let (Some(top), Some(next)) = (data.stack.pop(), data.stack.top_mut()) else {
                    return Err(Error::StackIsEmpty);
                };

                *next -= top;
            }
            Reverse => match tokens.next().unwrap_or(Token::Eof) {
                Hole => data.stack.0.reverse(),
                Ident(c) => {
                    let Some(var) = data.get_local_or_global(c) else {
                        return Err(Error::VariableDoesNotExists(c));
                    };

                    let len = data.stack.0.len();
                    let start =
                        len - usize::try_from(var).map_err(|_| Error::NegativeNumberFound)?;
                    data.stack.0[start..len].reverse();
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "`_` or a variable",
                    });
                }
            },
            Assign => match tokens.next().unwrap_or(Token::Eof) {
                Hole => {
                    data.stack.pop().into_empty_stack_err()?;
                }
                Ident(c) => {
                    let val = data.stack.pop().into_empty_stack_err()?;
                    data.globals.0.insert(c, val);
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "`_` or a variable",
                    });
                }
            },
            Delete => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    if let Some(locals) = data.locals.as_mut() {
                        if locals.0.remove(&c).is_some() {
                            continue;
                        }
                    }

                    if data.globals.0.remove(&c).is_none() {
                        return Err(Error::VariableDoesNotExists(c));
                    }
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "variable name",
                    });
                }
            },
            Push => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    let Some(val) = data.get_local_or_global(c) else {
                        return Err(Error::VariableDoesNotExists(c));
                    };
                    data.stack.push(val);
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "variable name",
                    });
                }
            },
            AssignLocal => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    let Some(locals) = data.locals.as_mut() else {
                        return Err(Error::MustBeProcedure);
                    };

                    locals.0.insert(c, 0);
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "variable name",
                    });
                }
            },
            Output => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    let Some(var) = data.get_local_or_global(c) else {
                        return Err(Error::VariableDoesNotExists(c));
                    };

                    let ascii = u8::try_from(var).map_err(|_| Error::InvalidAsciiCode)? as char;
                    write!(&mut data.out, "{ascii}").map_err(|_| Error::FailedToWrite)?;
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "variable name",
                    });
                }
            },
            Input => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    let mut buf = [0; 1];
                    data.inp
                        .read_exact(&mut buf)
                        .map_err(|_| Error::FailedToRead)?;
                    let input = buf[0] as char;
                    if !input.is_ascii() {
                        return Err(Error::NonAsciiCharacter(input));
                    }

                    let Some(var) = data.get_local_or_global_mut(c) else {
                        return Err(Error::VariableDoesNotExists(c));
                    };

                    *var = input as u8 as i16;
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "variable name",
                    });
                }
            },
            Call => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    let procedure = data
                        .procedures
                        .0
                        .get(&c)
                        .cloned()
                        .into_result(Error::ProcedureDoesNotExists(c))?;

                    let mut tokens = tokens.child(procedure.body_span);
                    data.set_locals(Variables::default());
                    interpret(&mut tokens, data)?;
                    data.clear_locals();
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "procedure name",
                    });
                }
            },
            End => break Out::End,
            Continue => break Out::Continue,
            QuestionMark => match tokens.next().unwrap_or(Token::Eof) {
                Ident(c) => {
                    let start = tokens.cursor_position();
                    let end = eat_till(tokens, Token::Semi, "expected `;`")?;
                    let var = data
                        .get_local_or_global(c)
                        .into_result(Error::VariableDoesNotExists(c))?;
                    let top = data.stack.pop().into_empty_stack_err()?;

                    // Cursor is moved to the token next to the `;` by `eat_till` function
                    // thus we move it back if the comparsion is true.
                    if var == top {
                        tokens.move_cursor_to(start);
                    } else {
                        continue;
                    }
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "variable name",
                    });
                }
            },
            Semi => {}
            LeftParen => {
                let start = tokens.cursor_position();
                let end = eat_till(tokens, Token::RightParen, "expected `)`")?;

                loop {
                    let mut tokens = tokens.child(start..end);
                    let output = interpret(&mut tokens, data)?;
                    if output == Out::End {
                        break;
                    }
                }
            }
            // RightParen => todo!(),
            // LeftBracket => todo!(),
            // RihgtBracket => todo!(),
            // LeftBrace => todo!(),
            // RightBrace => todo!(),
            Ident(c) => {
                match tokens.next_indiced().unwrap_or((0, Token::Eof)) {
                    // Procedure block
                    (start, LeftBrace) => {
                        let end = eat_till(tokens, Token::RightBrace, "expected `}`")?;

                        data.procedures.0.insert(
                            c,
                            Procedure {
                                // Add one since `start` is the index of `{`
                                body_span: (start + 1)..end,
                            },
                        );
                    }
                    (start, LeftBracket) => {
                        let end = eat_till(tokens, Token::RightBracket, "expected `]`")?;

                        let var = data
                            .get_local_or_global(c)
                            .into_result(Error::VariableDoesNotExists(c))?;

                        for _ in 0..var {
                            // Add one since `start` is the index of `[`
                            let mut tokens = tokens.child((start + 1)..end);
                            let output = interpret(&mut tokens, data)?;
                            if output == Out::End {
                                break;
                            }
                        }
                    }
                    (_, t) => {
                        return Err(Error::UnexpectedToken {
                            got: t,
                            expected: "procedure body",
                        });
                    }
                }
            }
            Eof => break Out::Eof,
            t => {
                return Err(Error::UnexpectedToken {
                    got: t,
                    expected: "",
                });
            }
        }
    };

    Ok(out)
}

/// This function iteratres over tokens untill it meets `stop` token then returns it's index.
fn eat_till(tokens: &mut TokensIter, stop: Token, msg: &'static str) -> Result<usize, Error> {
    loop {
        match tokens.next_indiced() {
            Some((idx, token)) if token == stop => break Ok(idx),
            Some((_, _)) => continue,
            None => {
                break Err(Error::UnexpectedToken {
                    got: Token::Eof,
                    expected: msg,
                });
            }
        }
    }
}

pub trait OptionIntoEmptyStackExt<T>: Sized {
    fn opt(self) -> Option<T>;
    fn into_empty_stack_err(self) -> Result<T, Error> {
        match self.opt() {
            Some(value) => Ok(value),
            None => Err(Error::StackIsEmpty),
        }
    }
    fn into_result<E>(self, error: E) -> Result<T, E> {
        match self.opt() {
            Some(value) => Ok(value),
            None => Err(error),
        }
    }
}

impl<T: Sized> OptionIntoEmptyStackExt<T> for Option<T> {
    fn opt(self) -> Option<T> {
        self
    }
}
