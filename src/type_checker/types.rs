use crate::{
    analysis::symbols::SymbolError,
    parsing::ast::{Expression, Identifier},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectTypeBase {
    Int32,
    Int64,
    Int128,
    UInt32,
    UInt64,
    UInt128,
    Bool,
    Void,
    Function(Box<Vec<Type>>),
    UserDefined(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoolProperty {
    UserDefined(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoolPropertyFunction {
    UserDefined(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Proposition {
    PropertyOfExpression(BoolProperty, Expression),
    FunctionOfExpressions(BoolPropertyFunction, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Object {
        base: ObjectTypeBase,
        generics: Option<Vec<Type>>,
    },
    Proposition(Proposition),
    Never,
}

impl Type {
    pub fn void() -> Self {
        Type::Object {
            base: ObjectTypeBase::Void,
            generics: None,
        }
    }

    pub fn is_assignable_from(&self, from: &Type) -> bool {
        from == &Type::Never || self == from
    }

    pub fn is_callable(signature: &[Type], args: &[Type]) -> bool {
        if args.len() >= signature.len() {
            false
        } else {
            for (expected, got) in signature.iter().zip(args.iter()) {
                if !expected.is_assignable_from(got) {
                    return false;
                }
            }
            true
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeError {
    Unknown,
    InvalidRadix(i32),
    IntegerLitteralTooLarge,
    InvalidIntegerLitteral,
    SymbolError(SymbolError),
    OperatorNotDefined(String, Vec<Type>),
    OperatorAmbiguous(String, Vec<Type>),
}

impl From<SymbolError> for TypeError {
    fn from(value: SymbolError) -> Self {
        Self::SymbolError(value)
    }
}
