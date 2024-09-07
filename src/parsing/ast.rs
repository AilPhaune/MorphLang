use core::fmt;

use crate::type_checker::types::Type;

use super::combinators::PositionInfo;

#[derive(Debug, Clone)]
pub enum BinaryOperatorPrecedence {
    LeftAssociative(u64, String),
    RightAssociative(u64, String),
}

#[derive(Debug, Clone)]
pub struct UnaryOperatorPrecedence(u64, String);

impl UnaryOperatorPrecedence {
    pub fn create(precedence: u64, operator: String) -> Self {
        Self(precedence, operator)
    }

    pub fn get(&self) -> (&u64, &String) {
        (&self.0, &self.1)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub position: PositionInfo,
}

impl Identifier {
    pub fn new(position: PositionInfo, name: &str) -> Self {
        Self::create(position, name.to_string())
    }

    pub fn create(position: PositionInfo, name: String) -> Self {
        Self { name, position }
    }

    pub fn position(&self) -> &PositionInfo {
        &self.position
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Builtin(PositionInfo),
    LiteralInt(PositionInfo, String, i32),
    BinaryOperation(PositionInfo, Box<Expression>, Box<Expression>, String),
    UnaryOperation(PositionInfo, Box<Expression>, String),
    Block(PositionInfo, Vec<Statement>),
}

impl Expression {
    pub fn position(&self) -> &PositionInfo {
        match self {
            Self::Builtin(pos, ..)
            | Self::LiteralInt(pos, ..)
            | Self::BinaryOperation(pos, ..)
            | Self::UnaryOperation(pos, ..)
            | Self::Block(pos, ..) => pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
    Variable(
        PositionInfo,
        Identifier,
        Option<Type>,
        Option<Box<Expression>>,
    ),
    Function(
        PositionInfo,
        Identifier,
        Vec<(Identifier, Type)>,
        Option<Type>,
        Option<Box<Statement>>,
    ),
    Namespace(PositionInfo, Identifier, Vec<Declaration>),
}

impl Declaration {
    pub fn position(&self) -> &PositionInfo {
        match self {
            Self::Variable(pos, ..) | Self::Function(pos, ..) | Self::Namespace(pos, ..) => pos,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExpressionKind {
    LiteralInt,
    BinaryOperation,
    UnaryOperation,
}

impl fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
