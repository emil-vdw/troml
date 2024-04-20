use std::{error, fmt};

use crate::file::{File, Location};

use self::token::{Segment, Token};

pub mod token;

#[derive(Debug)]
pub struct Error {
    message: String,
    file: File,
    start: Location,
    end: Location,
}

impl Error {}

impl fmt::Display for Error {
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

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

macro_rules! lexer_error {
    ($message:expr, $file:expr, $start:expr, $end:expr) => {
        Err(Error { message: String::from($message), file: $file.clone(), start: $start, end: $end })
    };
}

#[derive(Debug)]
pub struct Lexer {
    file: File,
    chars: Vec<char>,
    location: Location,
}

impl Lexer {
    pub fn new(file: File) -> Self {
        let chars = file.contents.chars().collect();
        Lexer {
            file,
            chars,
            location: Location::new(),
        }
    }

    fn tokenize_string(&mut self) -> Result<Segment, Error> {
        todo!()
    }

    fn tokenize_multiline_string(&mut self) -> Result<Segment, Error> {
        let start = self.location;
        
        let mut content = String::new();

        while let Some(next_char) = self.advance() {
            match next_char {
                '\\' => if self.peek(1).is_none() {
                    return lexer_error!(
                        "unexpected end of file, expected an escaped character",
                        self.file,
                        self.location,
                        self.location
                    );
                },
                '"' => {
                    // Detect whether the string is being terminated.
                    let mut lookahead_index = 1;
                    let mut consecutive_quotes = 1;

                    loop {
                        match self.peek(lookahead_index) {
                            Some('"') => {
                                consecutive_quotes += 1;
                            },
                            Some('\n') | None => {
                                if consecutive_quotes >= 3 {
                                    // Multiline string has been terminated.
                                    content.push_str(
                                        &self.advance_by(lookahead_index - 1).unwrap()
                                    );
                                    return Ok(
                                        Segment::new(
                                            Token::String { content, literal: false, multiline: true },
                                            start,
                                            self.location
                                        )
                                    )
                                }

                                // Line ends without string being terminated.
                                return lexer_error!(
                                    "unexpected end of line, expected '\"'",
                                    self.file,
                                    self.location,
                                    self.location
                                );
                            },
                            Some(outside_char) => {
                                // Some other character
                                if consecutive_quotes >= 3 {
                                    return lexer_error!(
                                        format!("expected end of line, found '{outside_char}'"),
                                        self.file,
                                        self.location,
                                        self.location
                                    );
                                }

                                content.push_str(
                                    &self.advance_by(lookahead_index - 1).unwrap()
                                );
                                break;
                            },
                        }
                        lookahead_index += 1;
                    }
                },
                _ => content.push(next_char),
            }
        }

        Ok(
            Segment::new(
                Token::String {
                    content, literal: false, multiline: true
                },
                start,
                self.location
            )
        )
    }

    fn tokenize_comment(&mut self) -> Result<Segment, Error> {
        let start = self.location;
        let mut comment_string = String::new();

        while let Some(next_char) = dbg!(self.peek(0)) {
            if next_char == '\n' {
                break;
            }
            comment_string.push(self.advance().unwrap());
        }

        Ok(
            Segment::new(
                Token::Comment(comment_string),
                start,
                self.location,
            )
        )
    }

    pub fn tokenize(&mut self) -> Result<Vec<Segment>, Error> {
        let mut tokens: Vec<Segment> = Vec::new();

        while let Some(current_char) = self.peek(0) {
            match current_char {
                '=' => {
                    self.advance();
                    tokens.push(
                        Segment::new(Token::Equal, self.location, self.location)
                    );
                },
                '.' => {
                    self.advance();
                    tokens.push(Segment::new(Token::Dot, self.location, self.location));
                },
                ',' => {
                    self.advance();
                    tokens.push(Segment::new(Token::Comma, self.location, self.location));
                },

                // Comment
                '#' => tokens.push(self.tokenize_comment()?),

                // String
                '"' => tokens.push(self.tokenize_string()?),

                // String literal

                // Whitespace
                ' ' | '\t' => {
                    let start = self.location;
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
                        self.location,
                    ));
                },
                _ => todo!(),
            }
        }

        tokens.push(Segment::new(
            Token::Eof,
            self.location,
            self.location,
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
        match self.peek(0) {
            Some(current_char) => {
                if current_char == '\n' {
                    self.location.newline();
                } else {
                    self.location.forward();
                }
                Some(current_char)
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

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    fn new_lexer(file_contents: &str) -> Lexer {
        Lexer::new(
            File::new("Test file", file_contents)
        )
    }

    #[test_case(
        "# This is a normal comment", Segment::new(
            Token::Comment("# This is a normal comment".to_string()),
            Location::new(),
            Location::from((0, 26, 26))
        ) ; "A normal comment"
    )]
    #[test_case(
        "#", Segment::new(
            Token::Comment("#".to_string()),
            Location::new(),
            Location::from((0, 1, 1))
        ) ; "An empty comment"
    )]
    fn test_tokenize_comment(
        comment_string: & str,
        expected_segment: Segment
    ) {
        let mut lexer = new_lexer(comment_string);

        assert_eq!(
            lexer.tokenize_comment().unwrap(),
            expected_segment
        );
    }

    #[test_case(
        r#""""Hello world""""#, Segment::new(
            Token::String { content: "Hello world".to_string(), literal: false, multiline: true },
            Location::new(),
            Location::from((0, 17, 17))
        ) ; "A simple multiline string"
    )]
    fn test_tokenize_multiline_string(
        multiline_string: &str, expected_segment: Segment
    ) {
        let mut lexer = new_lexer(multiline_string);

        assert_eq!(
            lexer.tokenize_multiline_string().unwrap(),
            expected_segment
        );
    }
}
