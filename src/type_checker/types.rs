use crate::parsing::ast::{Expression, Identifier};

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectTypeBase {
    Int32,
    Int64,
    UInt32,
    UInt64,
    Bool,
    Void,
    UserDefined(Identifier),
}

#[derive(Debug, Clone)]
pub enum BoolProperty {
    UserDefined(Identifier),
}

#[derive(Debug, Clone)]
pub enum BoolPropertyFunction {
    UserDefined(Identifier),
}

#[derive(Debug, Clone)]
pub enum Proposition {
    PropertyOfExpression(BoolProperty, Expression),
    FunctionOfExpressions(BoolPropertyFunction, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Object {
        base: ObjectTypeBase,
        generics: Vec<Type>,
    },
    Proposition(Proposition),
}
