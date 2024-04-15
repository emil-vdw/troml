use std::{error, fmt};

use crate::file::{File, Location};

use self::token::{Segment, Token};

pub mod token;

#[derive(Debug)]
pub struct Error<'a> {
    message: String,
    file: &'a File,
    start: Location,
    end: Location,
}

impl Error<'_> {}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut error_string = String::new();
        dbg!((self.start, self.end));

        self.file.contents.lines().enumerate().skip(self.start.line).take(self.end.line - self.start.line + 1).for_each(
            |(line_number, line)| {
                error_string.push_str(line);
                error_string.push('\n');

                if line_number == self.start.line {
                    error_string.push_str(&" ".repeat(self.start.column));
                }

                let end_column = if self.end.line == self.start.line {
                    self.end.column - self.start.column
                } else {
                    line.len()
                };

                error_string.push_str(&"^".repeat(end_column));
            });

        write!(
            f,
            "{error_string}\n\t{message}",
            message=self.message,
        )
    }
}

impl error::Error for Error<'_> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

macro_rules! lexer_error {
    ($message:expr, $file:expr, $start:expr, $end:expr) => {
        Err(Error { message: String::from($message), file: $file, start: $start.clone(), end: $end.clone() })
    };
}

#[derive(Debug)]
pub struct Lexer<'l> {
    file: &'l File,
    chars: Vec<char>,
    location: Location,
}

impl<'l> Lexer<'l> {
    pub fn new(file: &'l File) -> Self {
        Lexer {
            file,
            chars: file.contents.chars().collect(),
            location: Location::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Segment>, Error> {
        let mut tokens: Vec<Segment> = Vec::new();

        while let Some(current_char) = self.advance() {
            match current_char {
                '=' => tokens.push(
                    Segment::new(Token::Equal, self.location.clone(), self.location.clone())
                ),
                '.' => tokens.push(Segment::new(Token::Dot, self.location.clone(), self.location.clone())),
                ',' => tokens.push(Segment::new(Token::Comma, self.location.clone(), self.location.clone())),

                // Comment
                '#' => {
                    let start = self.location.clone();
                    let mut comment_string = String::new();

                    while let Some(next_char) = self.peek(1) {
                        if next_char == '\n' {
                            tokens.push(
                                Segment::new(
                                    Token::Comment(comment_string),
                                    start,
                                    self.location.clone(),
                                )
                            );
                            break;
                        }
                        comment_string.push(self.advance().unwrap());
                    }
                },

                // String
                '"' => {
                    let string_quote = current_char;
                    let start = self.location.clone();
                    let mut content = String::new();

                    let multiline = match (self.peek(1), self.peek(2)) {
                        (Some('"'), Some('"')) => {
                            self.advance_by(2);
                            true
                        },
                        _ => false
                    };

                    loop {
                        match self.peek(1) {
                            // Escaped character
                            Some('\\') => {
                                if self.peek(2).is_none() {
                                    return lexer_error!(
                                        "unexpected end of file, expected an escaped character",
                                        self.file,
                                        self.location,
                                        self.location + 1
                                    );
                                }

                                content.push_str(&self.advance_by(2).unwrap());
                            },
                            Some('"') => {
                                if multiline {
                                    let mut lookahead_index = 2;
                                    let mut consecutive_quotes = 2;
                                        
                                    while let Some(char_ahead) = self.peek(lookahead_index) {
                                        if char_ahead == '"' {
                                            consecutive_quotes += 1;
                                            lookahead_index += 1;
                                        } else {
                                            break;
                                        }
                                    }

                                    if consecutive_quotes == 3 {
                                        self.advance_by(3);
                                        break;
                                    } else if consecutive_quotes < 3 {
                                        content.push_str(&self.advance_by(lookahead_index).unwrap());
                                    } else {
                                        return lexer_error!(
                                            "",
                                            self.file,
                                            self.location,
                                            self.location + 2
                                        )
                                    }
                                } else {
                                    // String has been terminated.
                                    self.advance();
                                    break;
                                }
                            },
                            Some(next_char) => {
                                content.push(next_char);
                            },
                            None => {
                                return lexer_error!(
                                    "unexpected end of file, expected '\"'",
                                    self.file,
                                    self.location,
                                    self.location
                                )
                            }
                        }
                    }

                    tokens.push(Segment::new(Token::String{content, literal: false, multiline}, start, self.location.clone()))
                }

                // String literal

                // Whitespace
                ' ' | '\t' => {
                    let start = self.location.clone();
                    let mut whitespace_string = String::from(current_char);
                    while let Some(next_char) = self.peek(1) {
                        if next_char == ' ' || next_char == '\t' {
                            whitespace_string.push(self.advance().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(Segment::new(
                        Token::Whitespace(whitespace_string),
                        start,
                        self.location.clone(),
                    ));
                },
                _ => todo!(),
            }
        }

        tokens.push(Segment::new(
            Token::Eof,
            self.location.clone(),
            self.location.clone(),
        ));
        Ok(tokens)
    }

    // Peak ahead by `amount` characters.
    fn peek(&self, amount: usize) -> Option<char> {
        if self.location.char + amount >= self.chars.len() {
            None
        } else {
            Some(self.chars[self.location.char + amount])
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.peek(1) {
            Some(c) => {
                if self.chars[self.location.char] == '\n' {
                    self.location.newline();
                } else {
                    self.location.forward();
                }
                Some(c)
            }
            None => None
        }
    }

    fn advance_by(&mut self, amount: usize) -> Option<String> {
        self.peek(1)?;

        let mut section = String::new();

        for _ in 0..amount {
            if let Some(next_char) = self.advance() {
                section.push(next_char);
            } else {
                return Some(section);
            }
        }

        Some(section)
    }
}
