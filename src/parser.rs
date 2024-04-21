use std::{error, fmt};

use crate::lexer::token::{Segment, Token};

use self::statement::{Node, Statement};

pub mod statement;

#[derive(Debug)]
pub struct Parser {
    file: File,
    tokens: Vec<Segment>,
    current: usize,
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
    file: File,
    token: Segment,
}

impl error::Error for ParseError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

macro_rules! parse_error {
    ($message:expr, $file:expr, $token:expr) => {
        Err(ParseError {
            message: String::from($message),
            file: $file.clone(),
            token: $token,
        })
    };
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<Node, Vec<ParseError>> {
        let mut statements: Vec<Statement> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();

        while self.has_tokens() {
            if self.match_next(|tok| matches!(tok, Token::Newline))
                && self.match_next_non_whitespace(|tok| matches!(tok, Token::Newline || Token::Eof))
            {
                tokens.push(self.advance().unwrap());
                // Parse an empty line that may contain whitespace.
                // We have to parse this and treat it as significant becuase
                // we are going to lint and fix the file.
                while let Some(segment) = self.peek(0) {
                    if !matches!(segment.token, Token::Newline | Token::Eof) {
                        tokens.push(self.advance().unwrap());
                    } else {
                        // The 'empty' line ends when we encounter a newline or EOF.
                        satements.push(Node::new(Satement::Newline, tokens));
                        break;
                    }
                }
            } else {
                match self.parse_statement() {
                    Ok(statement) => statements.push(statement),
                    Err(parse_error) => {
                        errors.push(parse_error);
                        self.synchronize();
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(Node::new(Statement::Document(statements), Vec::new()))
        } else {
            Err(errors)
        }
    }

    fn synchronize(&mut self) {
        todo!()
    }

    /// Parses a statement.
    /// This can be either a comment, a table or a table array.
    fn parse_statement(&mut self) -> Result<Node, ParseError> {
        let mut tokens: Vec<Segment> = Vec::new();

        let next_meaningful_token = self
            .next_non_whitespace()
            .expect("unexpected end of file, expected statement");

        let mut next_significant_index = 0;
        // let mut next_significant_token;

        loop {
            let upcoming_token = self
                .peek(next_significant_index)
                .expect("there should always be another significant token");

            match upcoming_token {
                Token::OpenBracket | Token::Comment(..) => {
                    break;
                }
                _ => next_significant_index += 1,
            }
        }

        match self.peek(next_significant_index).unwrap() {
            Token::Comment(comment_string) => {
                tokens.extend(self.advance_until(|s| matches!(s, Token::Comment(..))));
                tokens.push(self.advance().unwrap());

                return Ok(Node::new(
                    Statement::Comment(comment_string.to_owned()),
                    tokens,
                ));
            }
            Token::OpenBracket => match self.peek(next_significant_index + 1) {
                Some(segment) if matches!(segment.token, Token::OpenBracket) => {
                    return self.parse_table_array();
                }
                _ => {
                    return self.parse_table();
                }
            },
            _ => {
                return parse_error!(
                    format!("expected comment, table, or table array",),
                    self.file,
                    next_meaningful_token
                )
            }
        }

        next_meaningful_token = self.next_non_whitespace();
        todo!()
    }

    fn parse_table(&mut self) -> Result<Node, ParseError> {
        let mut tokens = Vec::new();

        tokens.extend(self.advance_until(|s| matches!(s, Token::OpenBracket)));
    }

    fn parse_table_array(&mut self) -> Result<Node, ParseError> {
        todo!()
    }

    /// Returns the next non-whitespace token.
    fn next_non_whitespace(&self) -> Option<&Segment> {
        let mut lookahead_index = 0;

        while let Some(next_token) = self.peek(lookahead_index) {
            if !matches!(next_token, Token::Whitespace(..)) {
                Some(next_token)
            }
        }

        None
    }

    /// Returns true if the next token matches the given predicate.
    fn match_next(&self, predicate: fn(&Token) -> bool) -> bool {
        self.has_tokens() && predicate(&self.peek(0).unwrap().token)
    }

    fn match_next_non_whitespace(&self, p: fn(&Token) -> bool) -> bool {
        match self.next_non_whitespace() {
            Some(segment) => p(&segment.token),
            _ => false,
        }
    }

    fn has_tokens(&self) -> bool {
        self.current < self.tokens.len()
    }

    fn advance(&mut self) -> Option<Segment> {
        if !self.has_tokens() {
            return None;
        }

        let current_token = Some(self.tokens[self.current]);
        self.current += 1;

        current_token
    }

    /// Advances the parser until the given predicate is true.
    /// Returns the slice of tokens that were advanced.
    /// This does not include the token that caused the predicate to be true.
    /// If the predicate is never true, this function panics.
    fn advance_until(&mut self, predicate: fn(&Token) -> bool) -> Option<&[Segment]> {
        if !self.has_tokens() {
            return None;
        }

        let start = self.current;

        while let Some(next_token) = self.peek(0) {
            if predicate(&next_token.token) {
                return Some(&self.tokens[start..end]);
            }
            self.advance();
        }

        panic!("unexpected end of file");
    }

    fn peek(&self, amount: usize) -> Option<&Segment> {
        if (self.current + amount) == self.tokens.len() {
            return None;
        }

        Some(&self.tokens[self.current + amount])
    }
}
