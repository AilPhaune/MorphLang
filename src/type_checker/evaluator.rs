use crate::{
    analysis::symbols::{Symbol, SymbolTable},
    parsing::{
        ast::{Expression, Statement},
        combinators::{Either, PositionInfo},
    },
};

use super::types::{ObjectTypeBase, Type, TypeError};

const ALPHABET: &str = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

pub fn parse_int(value: &str, radix: i32) -> Result<u128, TypeError> {
    if radix < 2 || radix as usize >= ALPHABET.len() {
        Err(TypeError::InvalidRadix(radix))
    } else {
        let mut res: u128 = 0;
        for c in value.chars() {
            let cvalue = ALPHABET.find(c).ok_or(TypeError::InvalidIntegerLitteral)?;
            if cvalue >= radix as usize {
                Err(TypeError::InvalidIntegerLitteral)?
            }
            res = res
                .checked_mul(radix as u128)
                .ok_or(TypeError::IntegerLitteralTooLarge)?
                .checked_add(cvalue as u128)
                .ok_or(TypeError::IntegerLitteralTooLarge)?;
        }
        Ok(res)
    }
}

pub fn parse_signed_int(value: &str, radix: i32) -> Result<Either<i128, u128>, TypeError> {
    if value
        .chars()
        .next()
        .ok_or(TypeError::InvalidIntegerLitteral)?
        == '-'
    {
        let value = parse_signed_int(&value[1..], radix)?;
        match value {
            Either::First(i) => {
                if i == i128::MIN {
                    Err(TypeError::IntegerLitteralTooLarge)?
                }
                Ok(Either::First(-i))
            }
            Either::Second(u) => {
                if u > i128::MAX as u128 {
                    Err(TypeError::IntegerLitteralTooLarge)?
                }
                Ok(Either::First(-(u as i128)))
            }
        }
    } else {
        let value = parse_int(value, radix)?;
        if value > i128::MAX as u128 {
            Ok(Either::Second(value))
        } else {
            Ok(Either::First(value as i128))
        }
    }
}

pub struct TypeEvaluator;

impl TypeEvaluator {
    pub fn get_fn_call_return_type(
        signature: &[Type],
        args: &[Type],
        position: PositionInfo,
    ) -> Option<Type> {
        if args.iter().any(|t| matches!(t, Type::Never(..))) {
            return Some(Type::Never(position));
        }
        let res: Vec<Type> = signature.iter().skip(args.len()).cloned().collect();
        if res.is_empty() {
            None
        } else if res.len() == 1 {
            Some(res[0].clone())
        } else {
            Some(Type::Object {
                position,
                base: ObjectTypeBase::Function(Box::new(res)),
                generics: None,
            })
        }
    }

    pub fn evaluate_statement_type(
        symbol_table: &SymbolTable,
        statement: &Statement,
    ) -> Result<Type, TypeError> {
        match statement {
            Statement::Expression(expr) => Self::evaluate_expression_type(symbol_table, expr),
            Statement::Declaration(decl) => Ok(Type::void(*decl.position())),
        }
    }

    pub fn evaluate_expression_type(
        symbol_table: &SymbolTable,
        expression: &Expression,
    ) -> Result<Type, TypeError> {
        match expression {
            Expression::Identifiers(pos, idents) => {
                // TODO:
                Ok(Type::Object {
                    position: *pos,
                    base: ObjectTypeBase::Void,
                    generics: None,
                })
            }
            Expression::LiteralString(pos, _) => Ok(Type::Object {
                position: *pos,
                base: ObjectTypeBase::String,
                generics: None,
            }),
            Expression::LiteralInt(pos, value, radix) => {
                let ival = parse_signed_int(value, *radix)?;
                Ok(Type::Object {
                    position: *pos,
                    base: match ival {
                        Either::Second(_) => ObjectTypeBase::UInt128,
                        Either::First(value) => {
                            if value > u64::MAX as i128 {
                                ObjectTypeBase::Int128
                            } else if value > i64::MAX as i128 {
                                ObjectTypeBase::UInt64
                            } else if value > u32::MAX as i128 {
                                ObjectTypeBase::Int64
                            } else if value > i32::MAX as i128 {
                                ObjectTypeBase::UInt32
                            } else if value > 0 {
                                ObjectTypeBase::Int32
                            } else if value < i64::MIN as i128 {
                                ObjectTypeBase::Int128
                            } else if value < i32::MIN as i128 {
                                ObjectTypeBase::Int64
                            } else {
                                ObjectTypeBase::Int32
                            }
                        }
                    },
                    generics: None,
                })
            }
            Expression::UnaryOperation(pos, expression, operator) => {
                let expr_type = Self::evaluate_expression_type(symbol_table, expression)?;
                let ops: Vec<_> = symbol_table
                    .get_unary_operator(operator)?
                    .ok_or_else(|| {
                        TypeError::OperatorNotDefined(operator.clone(), vec![expr_type.clone()])
                    })?
                    .iter()
                    .filter_map(|op| match op {
                        Symbol::FunctionDeclaration {
                            resolved_signature, ..
                        } => resolved_signature
                            .as_ref()
                            .filter(|signature| Type::is_callable(signature, &[expr_type.clone()])),
                        _ => None,
                    })
                    .collect();

                if ops.is_empty() {
                    Err(TypeError::OperatorNotDefined(
                        operator.clone(),
                        vec![expr_type.clone()],
                    ))?
                } else if ops.len() == 1 {
                    Self::get_fn_call_return_type(ops[0], &[expr_type.clone()], *pos).ok_or_else(
                        || TypeError::OperatorNotDefined(operator.clone(), vec![expr_type.clone()]),
                    )
                } else {
                    Err(TypeError::OperatorAmbiguous(
                        operator.clone(),
                        vec![expr_type.clone()],
                    ))?
                }
            }
            Expression::BinaryOperation(pos, lhs, rhs, operator) => {
                let lhs = Self::evaluate_expression_type(symbol_table, lhs)?;
                let rhs = Self::evaluate_expression_type(symbol_table, rhs)?;
                let ops: Vec<_> = symbol_table
                    .get_binary_operator(operator)?
                    .ok_or_else(|| {
                        TypeError::OperatorNotDefined(
                            operator.clone(),
                            vec![lhs.clone(), rhs.clone()],
                        )
                    })?
                    .iter()
                    .filter_map(|op| match op {
                        Symbol::FunctionDeclaration {
                            resolved_signature, ..
                        } => resolved_signature.as_ref().filter(|signature| {
                            Type::is_callable(signature, &[lhs.clone(), rhs.clone()])
                        }),
                        _ => None,
                    })
                    .collect();

                if ops.is_empty() {
                    Err(TypeError::OperatorNotDefined(
                        operator.clone(),
                        vec![lhs.clone(), rhs.clone()],
                    ))?
                } else if ops.len() == 1 {
                    Self::get_fn_call_return_type(ops[0], &[lhs.clone(), rhs.clone()], *pos)
                        .ok_or_else(|| {
                            TypeError::OperatorNotDefined(
                                operator.clone(),
                                vec![lhs.clone(), rhs.clone()],
                            )
                        })
                } else {
                    Err(TypeError::OperatorAmbiguous(
                        operator.clone(),
                        vec![lhs.clone(), rhs.clone()],
                    ))?
                }
            }
            Expression::Builtin(pos) => Ok(Type::Never(*pos)),
            Expression::Block(pos, statements) => {
                if statements.is_empty() {
                    Ok(Type::void(*pos))
                } else {
                    Self::evaluate_statement_type(
                        symbol_table,
                        statements.last().ok_or_else(|| TypeError::Unknown)?,
                    )
                }
            }
        }
    }
}
