use core::fmt;

use super::ast::ExpressionKind;

#[derive(Debug, Clone)]
pub struct ParserErrorInfo {
    kind: ParserErrorKind,
    info: Option<String>,
    level: i32,
}

impl fmt::Display for ParserErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.info {
            None => write!(f, "ParseError (level {}): {}.", self.level, self.kind),
            Some(info) => write!(
                f,
                "ParseError (level {}): {}. Info: {}",
                self.level, self.kind, info
            ),
        }
    }
}

impl ParserErrorInfo {
    pub fn create(kind: ParserErrorKind) -> Self {
        Self {
            kind,
            info: None,
            level: 0,
        }
    }

    pub fn set_info(&mut self, info: String) {
        self.info = Some(info);
    }

    pub fn with_info(&self, info: String) -> Self {
        Self {
            kind: self.kind.clone(),
            info: Some(info),
            level: self.level,
        }
    }

    pub fn set_level(&mut self, level: i32) {
        self.level = level;
    }

    pub fn with_level(&self, level: i32) -> Self {
        Self {
            kind: self.kind.clone(),
            info: self.info.clone(),
            level,
        }
    }

    pub fn increase_level(&mut self, diff: i32) {
        self.level += diff;
    }

    pub fn with_increased_level(&self, diff: i32) -> Self {
        Self {
            kind: self.kind.clone(),
            info: self.info.clone(),
            level: if self.level == 0 {
                0
            } else {
                self.level + diff
            },
        }
    }

    pub fn get_level(&self) -> i32 {
        self.level
    }

    pub fn get_kind(&self) -> &ParserErrorKind {
        &self.kind
    }

    pub fn get_info(&self) -> &Option<String> {
        &self.info
    }
}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    Unknown,
    ExpectedCharacter { predicate_info: String },
    ExpectedToken { token: String },
    EndOfFile,
    ExpectedExpressionKind(ExpressionKind),
    SubErrorList(Vec<ParserErrorInfo>),
    InvalidLiteral,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Self::Unknown => write!(f, "Unknown error"),
            Self::InvalidLiteral => write!(f, "Invalid literal"),
            Self::EndOfFile => write!(f, "End of input"),
            Self::ExpectedToken { token } => {
                write!(f, "Expected token: {}", token)
            }
            Self::ExpectedCharacter { predicate_info } => {
                write!(f, "Expected character of predicate {}", predicate_info)
            }
            Self::ExpectedExpressionKind(kind) => {
                write!(f, "Expected expression kind {}", kind)
            }
            Self::SubErrorList(sub_errs) => {
                write!(f, "Error caused by one of the sub-errors: {:?}", sub_errs)
            }
        }
    }
}
