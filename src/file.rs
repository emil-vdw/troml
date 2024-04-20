use std::{fmt, fs, ops::Add};

#[derive(Debug, Clone)]
pub struct File {
    pub name: String,
    pub contents: String,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub char: usize,
}

impl File {
    pub fn new(name: &str, contents: &str) -> Self {
        File {
            name: name.to_string(), contents: contents.to_string(),
        }
    }
}

impl Location {
    pub fn new() -> Self {
        Location {
            line: 0, column: 0, char: 0
        }
    }

    pub fn forward(&mut self) {
        self.column += 1;
        self.char += 1;
    }

    pub fn newline(&mut self) {
        self.char += 1;
        self.line += 1;
        self.column = 0;
    }
}

impl From<(usize, usize, usize)> for Location {
    fn from(location: (usize, usize, usize)) -> Self {
        Location {
            line: location.0, column: location.1, char: location.2
        }
    }
}

impl Add<usize> for Location {
    type Output = Self;

    fn add(self, other: usize) -> Self {
        let mut new_location = self.clone();
        new_location.char += other;
        new_location.column += other;

        new_location
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L: {}, C: {} ({})", self.line, self.column, self.char)
    }
}
