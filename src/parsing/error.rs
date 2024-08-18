use core::fmt;

#[derive(Debug)]
pub struct ParserErrorInfo {
    kind: ParserErrorKind,
    info: Option<String>,
}

impl fmt::Display for ParserErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.info {
            None => write!(f, "ParseError: {}.", self.kind),
            Some(info) => write!(f, "ParseError: {}. Info: {}", self.kind, info),
        }
    }
}

impl ParserErrorInfo {
    pub fn create(kind: ParserErrorKind) -> Self {
        Self { kind, info: None }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    ExpectedCharacter { predicate_info: String },
    EndOfFile,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Self::EndOfFile => write!(f, "End of input"),
            Self::ExpectedCharacter { predicate_info } => {
                write!(f, "Expected character of predicate {}", predicate_info)
            }
        }
    }
}
