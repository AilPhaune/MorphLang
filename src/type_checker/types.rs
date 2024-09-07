use crate::{
    analysis::symbols::SymbolError,
    parsing::{
        ast::{Expression, Identifier},
        combinators::PositionInfo,
    },
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
    String,
    Void,
    Function(Box<Vec<Type>>),
    UserDefined(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoolProperty {
    UserDefined(Identifier),
}

impl BoolProperty {
    pub fn position(&self) -> &PositionInfo {
        match self {
            Self::UserDefined(ident) => ident.position(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoolPropertyFunction {
    UserDefined(Identifier),
}

impl BoolPropertyFunction {
    pub fn position(&self) -> &PositionInfo {
        match self {
            Self::UserDefined(ident) => ident.position(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Proposition {
    PropertyOfExpression(PositionInfo, BoolProperty, Expression),
    FunctionOfExpressions(PositionInfo, BoolPropertyFunction, Vec<Expression>),
}

impl Proposition {
    pub fn position(&self) -> &PositionInfo {
        match self {
            Self::PropertyOfExpression(pos, ..) | Self::FunctionOfExpressions(pos, ..) => pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Object {
        position: PositionInfo,
        base: ObjectTypeBase,
        generics: Option<Vec<Type>>,
    },
    Proposition(Proposition),
    Never(PositionInfo),
}

impl Type {
    pub fn void(position: PositionInfo) -> Self {
        Type::Object {
            position,
            base: ObjectTypeBase::Void,
            generics: None,
        }
    }

    pub fn is_assignable_from(&self, from: &Type) -> bool {
        matches!(from, Type::Never(..)) || self == from
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

    pub fn position(&self) -> &PositionInfo {
        match self {
            Self::Never(pos) | Self::Object { position: pos, .. } => pos,
            Self::Proposition(prop) => prop.position(),
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
