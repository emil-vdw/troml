use std::{error, fmt};

use crate::file::{File, Location};

use self::token::{Segment, Token};

pub mod token;

#[derive(Debug, PartialEq, Eq)]
pub struct LexerError {
    message: String,
    file: File,
    start: Location,
    end: Location,
}

impl LexerError {
    pub fn new(message: &str, file: &File, start: Location, end: Location) -> Self {
        Self {
            message: message.to_string(),
            file: file.clone(),
            start, end,
        }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut error_string = String::new();

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
                    line.len() - self.start.column
                };

                error_string.push_str(&"^".repeat(end_column + 1));
            });

        write!(
            f,
            "{error_string}\n\t{message}",
            message=self.message,
        )
    }
}

impl error::Error for LexerError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

macro_rules! lexer_error {
    ($message:expr, $file:expr, $start:expr, $end:expr) => {
        Err(LexerError { message: String::from($message), file: $file.clone(), start: $start, end: $end })
    };
}


/// A lexer that tokenizes a file.
///
/// # Examples
///
/// ```
/// use lexer::Lexer;
/// use file::File;
///
/// let file = File::new("/path/to/file", "Hello, world!");
/// let mut lexer = Lexer::new(file);
/// let tokens = lexer.tokenize().unwrap();
/// ```
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

    fn tokenize_string(&mut self) -> Result<Segment, LexerError> {
        todo!()
    }

    fn tokenize_multiline_string(&mut self) -> Result<Segment, LexerError> {
        let start = self.location;
        let mut content = String::new();

        // Ignore the quotes, we don't want them to show up in the token.
        self.advance_by(3);

        while let Some(next_char) = self.peek(0) {
            match next_char {
                '\\' => {
                    if self.peek(1).is_none() {
                        return lexer_error!(
                            "unexpected end of file, expected an escaped character",
                            self.file,
                            self.location,
                            self.location
                        );
                    }

                    content.push_str(&self.advance_by(2).unwrap());
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
                                    self.advance_by(lookahead_index).unwrap();
                                    return Ok(
                                        Segment::new(
                                            Token::String { content, literal: false, multiline: true },
                                            start,
                                            self.location
                                        )
                                    )
                                }
                            },
                            Some(outside_char) => {
                                // Some non-quote character after the quotes.
                                if consecutive_quotes >= 3 {
                                    self.advance_by(consecutive_quotes);
                                    return lexer_error!(
                                        format!("expected end of line, found '{outside_char}'"),
                                        self.file,
                                        self.location,
                                        self.location
                                    );
                                }

                                // We have determined that the quotes are valid
                                // so we can add them to the content.
                                content.push_str(
                                    &self.advance_by(consecutive_quotes).unwrap()
                                );
                                break;
                            },
                        }

                        lookahead_index += 1;
                    }
                },
                _ => content.push(self.advance().unwrap()),
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

    fn tokenize_comment(&mut self) -> Result<Segment, LexerError> {
        let start = self.location;
        let mut comment_string = String::new();

        while let Some(next_char) = self.peek(0) {
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

    pub fn tokenize(&mut self) -> Result<Vec<Segment>, LexerError> {
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
                '"' => {
                    match (self.peek(1), self.peek(2)) {
                        (Some('"'), Some('"')) => tokens.push(self.tokenize_multiline_string()?),
                        _ => tokens.push(self.tokenize_string()?),
                    }
                },

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

    /// Peek at the character at `amount` characters ahead.
    /// `0` will return the current character under the cursor.
    /// Returns `None` if the end of the file has been reached.
    fn peek(&self, amount: usize) -> Option<char> {
        if self.location.char + amount >= self.chars.len() {
            None
        } else {
            Some(self.chars[self.location.char + amount])
        }
    }

    /// Advance the lexer by one character.
    /// Returns the character that was advanced over.
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

    /// Advance the lexer by `amount` characters.
    /// Returns the characters that were advanced over.
    fn advance_by(&mut self, amount: usize) -> Option<String> {
        // Return None if the end of the file has been reached.
        self.peek(0)?;

        let mut section = String::new();

        for _ in 0..amount {
            if let Some(next_char) = self.advance() {
                section.push(next_char);
            } else {
                // End of file reached before advancing `amount` characters.
                break;
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
            File::new("/tests/test.toml", file_contents.to_string())
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
        ) ; "simple multiline string"
    )]
    #[test_case(
        "\"\"\"Hello world\"\"\"\n", Segment::new(
            Token::String { content: "Hello world".to_string(), literal: false, multiline: true },
            Location::new(),
            Location::from((0, 17, 17))
        ) ; "simple newline terminated multiline string"
    )]
    #[test_case(
        concat!(r#""""Here are two quotation marks: "". Simple enough.""""#, '\n'), Segment::new(
            Token::String {
                content: r#"Here are two quotation marks: "". Simple enough."#.to_string(),
                literal: false,
                multiline: true
            },
            Location::new(),
            Location::from((0, 54, 54))
        ) ; "multiline string containing quotes"
    )]
    #[test_case(
        concat!(r#""""Here are three quotation marks: ""\".""""#, '\n'), Segment::new(
            Token::String {
                content: "Here are three quotation marks: \"\"\\\".".to_string(),
                literal: false,
                multiline: true
            },
            Location::new(),
            Location::from((0, 43, 43))
        ) ; "three valid consequetive quotes"
    )]
    #[test_case(
        concat!(r#""""Here are fifteen quotation marks: ""\"""\"""\"""\"""\".""""#, '\n'), Segment::new(
            Token::String {
                content: r#"Here are fifteen quotation marks: ""\"""\"""\"""\"""\"."#.to_string(),
                literal: false,
                multiline: true
            },
            Location::new(),
            Location::from((0, 61, 61))
        ) ; "many valid consequetive quotes"
    )]
    fn test_multiline_string(
        multiline_string: &str, expected_segment: Segment
    ) {
        let mut lexer = new_lexer(multiline_string);

        match lexer.tokenize_multiline_string() {
            Ok(segment) => {
                assert_eq!(segment, expected_segment);
            },
            Err(e) => {
                panic!("{}", e);
            }
        }
        
    }


    #[test_case(
        r#""""Hello world"""."""#,
        LexerError::new(
            "expected end of line, found '.'",
            &File::new("/tests/test.toml", String::from(
                r#""""Hello world"""."""#,
            )),
            Location::from((0, 17, 17)),
            Location::from((0, 17, 17))
        )
        ; "Three consequetive unescaped quotes"
    )]
    fn test_invalid_multiline_string(
        multiline_string: &str, expected_error: LexerError
    ) {
        match new_lexer(multiline_string).tokenize_multiline_string() {
            Err(e) => println!("{}", e),
            _ => (),
        }
        assert_eq!(
            new_lexer(multiline_string).tokenize_multiline_string(),
            Err(expected_error)
        );
    }
}
