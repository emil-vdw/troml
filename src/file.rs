use std::{fmt, fs, ops::Add, path::Path};

#[derive(Debug, Clone, Eq)]
pub struct File {
    pub name: String,
    pub path: String,
    pub contents: String,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub char: usize,
}

impl File {
    pub fn new(path: &str, contents: String) -> Self {
        File {
            path: path.to_string(),
            name: Path::new(path).file_name().expect("must target a file and not a directory").to_os_string().into_string().unwrap(),
            contents,
        }
    }
}

impl PartialEq for File {
    fn eq(&self, other: &File) -> bool {
        self.path == other.path && self.name == other.name
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
