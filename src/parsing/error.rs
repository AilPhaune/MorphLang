use core::fmt;
use std::rc::Rc;

use crate::preprocessor::PreprocessorError;

use super::ast::ExpressionKind;

#[derive(Debug, Clone)]
pub struct ParserErrorInfo {
    kind: ParserErrorKind,
    info: Option<String>,
    level: i32,
    cause: Option<Rc<ParserErrorInfo>>,
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
            cause: None,
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
            cause: self.cause.clone(),
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
            cause: self.cause.clone(),
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
            cause: self.cause.clone(),
        }
    }

    pub fn set_cause(&mut self, cause: Option<Rc<ParserErrorInfo>>) {
        self.cause = cause;
    }

    pub fn with_cause(&self, cause: Option<Rc<ParserErrorInfo>>) -> Self {
        Self {
            kind: self.kind.clone(),
            info: self.info.clone(),
            level: self.level,
            cause: cause.clone(),
        }
    }

    pub fn with_cause0(&self, cause: &ParserErrorInfo) -> Self {
        self.with_cause(Some(Rc::from(cause.clone())))
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

    pub fn get_cause(&self) -> &Option<Rc<ParserErrorInfo>> {
        &self.cause
    }

    pub fn pretty_print(&self, indent_level: usize, min_level: i32) -> String {
        let mut result = String::new();
        let indent = " ".repeat(indent_level * 2);

        result.push_str(&format!(
            "{}Error[Level {}]: {}\n",
            indent, self.level, self.kind
        ));

        if let Some(info) = &self.info {
            result.push_str(&format!("{}  Info: {}\n", indent, info));
        }

        if let ParserErrorKind::SubErrorList(errors) = &self.kind {
            for (i, sub_error) in errors.iter().enumerate() {
                if sub_error.get_level() < min_level {
                    continue;
                }
                result.push_str(&format!("{}  SubError {}:\n", indent, i + 1));
                result.push_str(&sub_error.pretty_print(indent_level + 2, min_level));
            }
        }

        if let Some(cause) = &self.cause {
            if cause.get_level() >= min_level {
                result.push_str(&format!("{}Caused by:\n", indent));
                result.push_str(&cause.pretty_print(indent_level + 2, min_level));
            }
        }

        result
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
    UnexpectedInput,
    ExpectedIdentifier,
    ExpectedKeyword(String),
    Expected(String),
    Preprocessor(PreprocessorError),
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Self::Unknown => write!(f, "Unknown error"),
            Self::Expected(s) => write!(f, "Expected {}", s),
            Self::InvalidLiteral => write!(f, "Invalid literal"),
            Self::ExpectedIdentifier => write!(f, "Expected identifier"),
            Self::EndOfFile => write!(f, "Unexpected end of input"),
            Self::UnexpectedInput => write!(f, "Unexpected input"),
            Self::ExpectedKeyword(kw) => write!(f, "Expected keyword: {}", kw),
            Self::Preprocessor(err) => write!(f, "Preprocessor error: {}", err),
            Self::ExpectedToken { token } => {
                write!(f, "Expected token: {}", token)
            }
            Self::ExpectedCharacter { predicate_info } => {
                write!(f, "Expected character of predicate {}", predicate_info)
            }
            Self::ExpectedExpressionKind(kind) => {
                write!(f, "Expected expression kind {}", kind)
            }
            Self::SubErrorList(_) => {
                write!(f, "Error caused by sub-error")
            }
        }
    }
}

pub fn elevate_highest_error(diff: i32) -> impl Fn(Vec<ParserErrorInfo>) -> ParserErrorInfo {
    move |errs| {
        let level = errs
            .iter()
            .max_by_key(|e| e.get_level())
            .map(|e| e.get_level())
            .unwrap_or(0);
        ParserErrorInfo::create(ParserErrorKind::SubErrorList(errs)).with_level(if level == 0 {
            0
        } else {
            level + diff
        })
    }
}
