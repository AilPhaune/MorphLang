use core::fmt;

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

#[derive(Debug, Clone)]
pub enum Expression {
    LiteralInt(String, i32),
    BinaryOperation(Box<Expression>, Box<Expression>, String),
    UnaryOperation(Box<Expression>, String),
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
