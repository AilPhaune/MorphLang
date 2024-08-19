use core::fmt;

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    PLUS,
    MINUS,
}

#[derive(Debug, Clone)]
pub enum Expression {
    LiteralInt(String, i32),
    BinaryOperation(Box<Expression>, Box<Expression>, BinaryOperator),
}

#[derive(Debug, Clone, Copy)]
pub enum ExpressionKind {
    LiteralInt,
    BinaryOperation,
}

impl fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
