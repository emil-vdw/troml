use crate::file::Location;

#[derive(Debug, PartialEq)]
pub struct Segment {
    token: Token,
    // raw: String,
    start: Location,
    end: Location,
}

impl Segment {
    pub fn new(token: Token, start: Location, end: Location) -> Self {
        Segment { token, start, end }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    // Basic types
    Comment(String),

    // Literals
    String {
        content: String,
        literal: bool,
        multiline: bool,
    },

    Integer(String), Float(String), Boolean(bool),
    // TODO
    // DateTime (offset date-time?), Date, Time,

    Equal, Dot, Comma,

    OpenBracket,    // [
    CloseBracket,   // ]
    OpenBrace,      // {
    CloseBrace,     // }

    // Keywords
    True, False, Identifier(String),

    // Meta tokens
    Whitespace(String), Newline, Eof,
}

impl Token {
    pub fn is_valid_identifier_char(c: char) -> bool {
        c.is_ascii_alphanumeric()
            || c == '_'
            || c == '-'
    }
}
