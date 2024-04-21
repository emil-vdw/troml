use crate::lexer::token::{Segment, Token};

#[derive(Debug)]
pub struct Node {
    statement: Statement,
    tokens: Vec<Segment>,
}

#[derive(Debug)]
pub enum Literal {
    Integer(String),
    Float(String),
    String(String),
    Boolean(bool),
}

#[derive(Debug)]
pub enum Key {
    BareKey(String),
    QuotedKey(String),
    DottedKey(Vec<Key>),
}

#[derive(Debug)]
pub enum Value {
    InlineTable,
    Literal(Literal),
}

#[derive(Debug)]
pub struct Header(Key);

#[derive(Debug)]
pub struct Pair(Key, Value);

#[derive(Debug)]
pub struct Table(Header, TableBody);

#[derive(Debug)]
pub struct TableBody(Vec<Pair>);

#[derive(Debug)]
pub enum Statement {
    Document(Vec<Statement>),

    Literal(Literal),
    Key(Key),
    Value(Value),
    Pair(Pair),

    Header(Header),
    Table(Table),
    TableBody(TableBody),

    Comment(String),

    Newline,
}

impl Node {
    pub fn new(statement: Statement, tokens: Vec<Segment>) -> Self {
        Self { statement, tokens }
    }
}
