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
    Identifier(String),

    // Literals
    String {
        content: String,
        literal: bool,
        multiline: bool,
    },

    Integer(i64), Float(f64), Boolean(bool),
    // TODO
    // DateTime (offset date-time?), Date, Time,

    Equal, Dot, Comma,

    OpenBracket,    // [
    CloseBracket,   // ]
    OpenBrace,      // {
    CloseBrace,     // }

    // Keywords
    True, False,

    // Meta tokens
    Whitespace(String), Newline, Eof,
}
