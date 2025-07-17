use std::io::Write;
use std::{collections::HashMap, io::Read};

use crate::lexer::{Lexer, Token};

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

pub struct Procedure {
    locals: Variables,
    body: Box<dyn Fn(&mut Stack, &mut Variables)>,
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Procedure")
            .field("locals", &self.locals)
            .field("body", &"{...}")
            .finish()
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
}

pub enum Error {
    StackIsEmpty,
    UnexpectedToken { got: Token, expected: &'static str },
    VariableDoesNotExists(char),
    NegativeNumberFound,
    MustBeProcedure,
    InvalidAsciiCode,
    FailedToWrite,
    FailedToRead,
    NonAsciiCharacter(char),
}

pub fn interpret<I, O>(lexer: &mut Lexer, data: &mut InterpretationData<I, O>) -> Result<(), Error>
where
    I: Read,
    O: Write,
{
    use Token::*;

    loop {
        match lexer.next_token() {
            Hole => todo!(),
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
            Reverse => match lexer.next_token() {
                Hole => data.stack.0.reverse(),
                Ident(c) => {
                    let Some(var) = data.get_local_or_global(c) else {
                        return Err(Error::VariableDoesNotExists(c));
                    };

                    let len = data.stack.0.len();
                    let start =
                        len - usize::try_from(var).map_err(|_| Error::NegativeNumberFound)?;
                    (&mut data.stack.0[start..len]).reverse();
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "`_` or a variable",
                    });
                }
            },
            Assign => match lexer.next_token() {
                Hole => {
                    data.stack.pop().into_empty_stack()?;
                }
                Ident(c) => {
                    let val = data.stack.pop().into_empty_stack()?;
                    data.globals.0.insert(c, val);
                }
                t => {
                    return Err(Error::UnexpectedToken {
                        got: t,
                        expected: "`_` or a variable",
                    });
                }
            },
            Delete => match lexer.next_token() {
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
            Push => match lexer.next_token() {
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
            AssignLocal => match lexer.next_token() {
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
            Output => match lexer.next_token() {
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
            Input => match lexer.next_token() {
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
            Call => todo!(),
            End => todo!(),
            Continue => todo!(),
            QuestionMark => todo!(),
            Semi => todo!(),
            LeftParen => todo!(),
            RightParen => todo!(),
            LeftBracket => todo!(),
            RihgtBracket => todo!(),
            LeftBrace => todo!(),
            RightBrace => todo!(),
            Ident(_) => todo!(),
            Eof => break,
        }
    }

    Ok(())
}

pub trait OptionIntoEmptyStackExt<T>: Sized {
    fn opt(self) -> Option<T>;
    fn into_empty_stack(self) -> Result<T, Error> {
        match self.opt() {
            Some(value) => Ok(value),
            None => Err(Error::StackIsEmpty),
        }
    }
}

impl<T: Sized> OptionIntoEmptyStackExt<T> for Option<T> {
    fn opt(self) -> Option<T> {
        self
    }
}
