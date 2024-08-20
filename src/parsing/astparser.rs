use std::rc::Rc;

use crate::type_checker::types::{BoolProperty, ObjectTypeBase, Proposition, Type};

use super::{
    ast::{
        BinaryOperatorPrecedence, Expression, ExpressionKind, Identifier, UnaryOperatorPrecedence,
    },
    combinators::{
        and, and_then1, and_then2, any_of, any_of_boxes, delimited, map_parser, map_parser_error,
        map_parser_output, not, parse_then_restart, parser_character, parser_character_predicate,
        parser_nothing, parser_token, repeat_at_least_1, skip_whitespaces, ParserInput,
        PositionInfo,
    },
    error::{elevate_highest_error, ParserErrorInfo, ParserErrorKind},
    parser::{run_parser, Parser},
};

#[derive(Debug, Clone)]
pub struct ASTParserContext {
    pub binary_operators: Vec<BinaryOperatorPrecedence>,
    pub unary_operators: Vec<UnaryOperatorPrecedence>,
}

impl ASTParserContext {
    pub fn create(
        binary_operators: Vec<BinaryOperatorPrecedence>,
        unary_operators: Vec<UnaryOperatorPrecedence>,
    ) -> Self {
        Self {
            binary_operators,
            unary_operators,
        }
    }
}

pub trait IncreaseErrorLevel<InputType: Clone, OutputType> {
    fn increase_error_level(
        &self,
        diff: i32,
    ) -> impl Fn(&InputType) -> Result<(InputType, OutputType), ParserErrorInfo>;
}

impl<InputType: Clone, OutputType, P> IncreaseErrorLevel<InputType, OutputType> for P
where
    P: Parser<InputType, ParserErrorInfo, OutputType>,
{
    fn increase_error_level(
        &self,
        diff: i32,
    ) -> impl Fn(&InputType) -> Result<(InputType, OutputType), ParserErrorInfo> {
        move |input| self.run(input).map_err(|e| e.with_increased_level(diff))
    }
}

#[macro_export]
macro_rules! default_context {
    () => {{
        let ctx = Rc::from(ASTParserContext::create(
            vec![
                BinaryOperatorPrecedence::RightAssociative(1000, "**".to_string()),
                BinaryOperatorPrecedence::LeftAssociative(100, '+'.to_string()),
                BinaryOperatorPrecedence::LeftAssociative(100, '-'.to_string()),
                BinaryOperatorPrecedence::LeftAssociative(500, '*'.to_string()),
                BinaryOperatorPrecedence::LeftAssociative(500, '/'.to_string()),
            ],
            vec![
                UnaryOperatorPrecedence::create(100, "+".to_string()),
                UnaryOperatorPrecedence::create(100, "-".to_string()),
                UnaryOperatorPrecedence::create(100, "~".to_string()),
            ],
        ));
        ctx
    }};
}

pub fn parse_decimal_literal_int(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    run_parser(
        map_parser(
            and(
                repeat_at_least_1(parser_character_predicate(
                    |c| c.is_numeric() || c == '_',
                    "NUMERIC_OR_UNDERSCORE",
                )),
                not(parser_character_predicate(
                    |c| c.is_alphabetic() || c == '_',
                    "ALPHABETIC",
                )),
            ),
            |v| {
                Expression::LiteralInt(
                    v.iter().filter(|&&c| c != '_').collect::<String>(),
                    if v[0] == '0' { 8 } else { 10 },
                )
            },
            |_| {
                ParserErrorInfo::create(ParserErrorKind::ExpectedExpressionKind(
                    ExpressionKind::LiteralInt,
                ))
                .with_info("In `parse_decimal_literal_int`".to_string())
            },
        ),
        input,
    )
}

pub fn parse_hex_literal_int(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    run_parser(
        map_parser(
            and_then2(
                parser_token("0x".to_string()),
                map_parser_error(
                    and(
                        repeat_at_least_1(parser_character_predicate(
                            |c| c.is_ascii_hexdigit() || c == '_',
                            "HEX_DIGIT_OR_UNDERSCORE",
                        )),
                        not(parser_character_predicate(
                            char::is_alphabetic,
                            "ALPHABETIC",
                        )),
                    ),
                    |_| ParserErrorInfo::create(ParserErrorKind::InvalidLiteral).with_level(5),
                ),
            ),
            |v| Expression::LiteralInt(v.iter().filter(|&&c| c != '_').collect::<String>(), 16),
            |e| e.with_info("In `parse_hex_literal_int`".to_string()),
        ),
        input,
    )
}

pub fn parse_oct_literal_int(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    run_parser(
        map_parser(
            and_then2(
                parser_token("0o".to_string()),
                map_parser_error(
                    and(
                        repeat_at_least_1(parser_character_predicate(
                            |c| ('0'..='7').contains(&c) || c == '_',
                            "OCT_DIGIT_OR_UNDERSCORE",
                        )),
                        not(parser_character_predicate(
                            char::is_alphanumeric,
                            "ALPHANUMERIC",
                        )),
                    ),
                    |_| ParserErrorInfo::create(ParserErrorKind::InvalidLiteral).with_level(5),
                ),
            ),
            |v| Expression::LiteralInt(v.iter().filter(|&&c| c != '_').collect::<String>(), 8),
            |e| e.with_info("In `parse_oct_literal_int`".to_string()),
        ),
        input,
    )
}

pub fn parse_bin_literal_int(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    run_parser(
        map_parser(
            and_then2(
                parser_token("0b".to_string()),
                map_parser_error(
                    and(
                        repeat_at_least_1(parser_character_predicate(
                            |c| c == '0' || c == '1' || c == '_',
                            "BIN_DIGIT_OR_UNDERSCORE",
                        )),
                        not(parser_character_predicate(
                            |c| c.is_alphanumeric(),
                            "ALPHANUMERIC",
                        )),
                    ),
                    |_| ParserErrorInfo::create(ParserErrorKind::InvalidLiteral).with_level(5),
                ),
            ),
            |v| Expression::LiteralInt(v.iter().filter(|&&c| c != '_').collect::<String>(), 2),
            |e| e.with_info("In `parse_bin_literal_int`".to_string()),
        ),
        input,
    )
}

pub fn parse_literal_int(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    run_parser(
        map_parser_error(
            any_of(vec![
                parse_hex_literal_int,
                parse_oct_literal_int,
                parse_bin_literal_int,
                parse_decimal_literal_int,
            ]),
            elevate_highest_error(2),
        ),
        input,
    )
}

pub fn parse_parenthesis_expression(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    move |input| {
        run_parser(
            and_then1(
                and_then2(
                    parser_character('('),
                    map_parser_error(parse_expression(context.clone()), |e| {
                        e.with_increased_level(2)
                    }),
                ),
                parser_character(')'),
            ),
            input,
        )
    }
}

pub fn parse_unary_operator(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    move |input| {
        let mut found_op = None;
        let mut found_prec = 0;

        for op in context.unary_operators.iter() {
            let (prec, op_str) = op.get();
            if parser_token(op_str.clone()).run(input).is_ok() && *prec > found_prec {
                found_op = Some(op_str.clone());
                found_prec = *prec;
            }
        }

        if let Some(op_str) = found_op {
            let (_, next_input) = input
                .advance_by(op_str.len())
                .ok_or(ParserErrorInfo::create(ParserErrorKind::EndOfFile))?;
            let (next_input, rhs) = parse_expression_internal(&next_input, &context, found_prec)?;
            Ok((
                next_input,
                Expression::UnaryOperation(Box::new(rhs), op_str),
            ))
        } else {
            Err(ParserErrorInfo::create(ParserErrorKind::Unknown))
        }
    }
}

pub fn parse_primary(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    move |input| {
        run_parser(
            map_parser_error(
                any_of_boxes(vec![
                    Box::from(parse_unary_operator(context.clone())),
                    Box::from(parse_parenthesis_expression(context.clone())),
                    Box::from(parse_literal_int),
                ]),
                elevate_highest_error(2),
            ),
            input,
        )
    }
}

pub fn parse_expression(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    move |input| parse_expression_internal(input, &context, 0)
}

fn parse_expression_internal(
    input: &ParserInput,
    context: &Rc<ASTParserContext>,
    min_precedence: u64,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    let (mut input, mut lhs) = and_then1(
        skip_whitespaces(parse_primary(context.clone())),
        skip_whitespaces(parser_nothing()),
    )
    .run(input)?;

    loop {
        let mut found_op = None;
        let mut found_prec = 0;
        let mut found_assoc = None;

        for op in context.binary_operators.iter() {
            match op {
                BinaryOperatorPrecedence::LeftAssociative(prec, op_str)
                | BinaryOperatorPrecedence::RightAssociative(prec, op_str)
                    if parser_token(op_str.clone()).run(&input).is_ok() =>
                {
                    if *prec >= min_precedence && *prec > found_prec {
                        found_op = Some(op_str.clone());
                        found_prec = *prec;
                        found_assoc = match op {
                            BinaryOperatorPrecedence::LeftAssociative(_, _) => Some("left"),
                            BinaryOperatorPrecedence::RightAssociative(_, _) => Some("right"),
                        }
                    }
                }
                _ => {}
            }
        }

        if let Some(op_str) = found_op {
            if let Some("right") = found_assoc {
                let (_, next_input) = input
                    .advance_by(op_str.len())
                    .ok_or(ParserErrorInfo::create(ParserErrorKind::EndOfFile))?;
                let (next_input, rhs) =
                    parse_expression_internal(&next_input, context, found_prec)?;
                lhs = Expression::BinaryOperation(Box::new(lhs), Box::new(rhs), op_str);
                input = next_input;
            } else if let Some("left") = found_assoc {
                let (_, next_input) = input
                    .advance_by(op_str.len())
                    .ok_or(ParserErrorInfo::create(ParserErrorKind::EndOfFile))?;
                let (next_input, rhs) =
                    parse_expression_internal(&next_input, context, found_prec + 1)?;
                lhs = Expression::BinaryOperation(Box::new(lhs), Box::new(rhs), op_str);
                input = next_input;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok((input, lhs))
}

pub fn parse_identifier(input: &ParserInput) -> Result<(ParserInput, Identifier), ParserErrorInfo> {
    run_parser(
        and_then2(
            parse_then_restart(parser_character_predicate(
                char::is_alphabetic,
                "ALPHABETIC",
            )),
            map_parser(
                repeat_at_least_1(parser_character_predicate(
                    |c| c == '_' || c.is_alphanumeric(),
                    "ALPHANUMERIC_OR_UNDERSCORE",
                )),
                |v| Identifier::create(v.iter().collect()),
                |_| ParserErrorInfo::create(ParserErrorKind::ExpectedIdentifier),
            ),
        ),
        input,
    )
}

pub fn parse_type_base(
    input: &ParserInput,
) -> Result<(ParserInput, ObjectTypeBase), ParserErrorInfo> {
    run_parser(
        map_parser_output(parse_identifier, |v| match v.0.as_str() {
            "i32" => ObjectTypeBase::Int32,
            "i64" => ObjectTypeBase::Int64,
            "u32" => ObjectTypeBase::UInt32,
            "u64" => ObjectTypeBase::UInt64,
            "bool" => ObjectTypeBase::Bool,
            "void" => ObjectTypeBase::Void,
            "Proof" => ObjectTypeBase::Proof,
            _ => ObjectTypeBase::UserDefined(v),
        }),
        input,
    )
}

pub fn parse_generics(
    context: &Rc<ASTParserContext>,
    input: &ParserInput,
) -> Result<(ParserInput, Vec<Type>), ParserErrorInfo> {
    match run_parser(parser_character('<'), input) {
        Ok((input, _)) => run_parser(
            and_then1(
                delimited(
                    skip_whitespaces(parser_character(',')),
                    skip_whitespaces(parse_type(context.clone())),
                ),
                skip_whitespaces(parser_character('>')),
            ),
            &input,
        ),
        Err(_) => Ok((input.clone(), Vec::new())),
    }
}

pub fn parse_type(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Type), ParserErrorInfo> {
    move |input| {
        let (input, type_base) = parse_type_base(input)?;

        if type_base == ObjectTypeBase::Proof {
            let (input, _) = run_parser(parser_character('<').increase_error_level(5), &input)?;
            let (input, prop) = run_parser(
                parse_proposition(context.clone()).increase_error_level(5),
                &input,
            )?;
            let (input, _) = run_parser(parser_character('>'), &input)?;

            Ok((input, Type::Proof(prop)))
        } else {
            let (input, generics) = parse_generics(&context, &input)?;

            Ok((
                input,
                Type::Object {
                    base: type_base,
                    generics,
                },
            ))
        }
    }
}

pub fn parse_keyword(
    kw: String,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, PositionInfo), ParserErrorInfo> {
    move |input| {
        let start = PositionInfo::from_parser_input_position(input);
        let (input, ident) = parse_identifier(input)?;
        if ident.0 != kw {
            Err(ParserErrorInfo::create(ParserErrorKind::ExpectedKeyword(
                kw.clone(),
            )))
        } else {
            let end = PositionInfo::from_parser_input_position(&input);
            Ok((input, start.until(&end)))
        }
    }
}

pub fn parse_proposition_property_of_expression(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Proposition), ParserErrorInfo> {
    move |input| {
        let (input, expr) = parse_expression(context.clone()).run(input)?;
        let (input, _) = skip_whitespaces(parse_keyword("is".to_string())).run(&input)?;
        let (input, property) =
            skip_whitespaces(parse_identifier.increase_error_level(5)).run(&input)?;

        Ok((
            input,
            Proposition::PropertyOfExpression(BoolProperty::UserDefined(property), expr),
        ))
    }
}

pub fn parse_proposition(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Proposition), ParserErrorInfo> {
    move |input| {
        run_parser(
            skip_whitespaces(map_parser_error(
                any_of(vec![parse_proposition_property_of_expression(
                    context.clone(),
                )]),
                elevate_highest_error(2),
            )),
            input,
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_eq_parse_input, assert_is_error_print_ok, parsing::parser::run_parser,
        traits::has_len::HasLen,
    };

    use super::*;

    macro_rules! assert_is_expression_literal_int {
        ($parsed: expr, $value: expr, $radix: expr) => {
            if let Expression::LiteralInt(value, radix) = $parsed {
                assert_eq!(value, $value);
                assert_eq!(radix, $radix);
            } else {
                panic!(
                    "Expected Expression::LiteralInt({},{}), got {:?}",
                    $value, $radix, $parsed
                );
            }
        };
    }

    #[test]
    fn test_parse_literal_int() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("123 + 456");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            assert_is_expression_literal_int!(parsed, "123", 10);
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("25_912_956 + 18");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            assert_is_expression_literal_int!(parsed, "25912956", 10);
            assert_eq_parse_input!(rest, 10, 0, 10);
        }
        {
            let input = ParserInput::create("195f");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        {
            let input = ParserInput::create("0x1_fA_0E + 18");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            assert_is_expression_literal_int!(parsed, "1fA0E", 16);
            assert_eq_parse_input!(rest, 9, 0, 9);
        }
        {
            let input = ParserInput::create("0x1h");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        {
            let input = ParserInput::create("0b1001+5");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            assert_is_expression_literal_int!(parsed, "1001", 2);
            assert_eq_parse_input!(rest, 6, 0, 6);
        }
        {
            let input = ParserInput::create("0b1100_1001 + 19");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            assert_is_expression_literal_int!(parsed, "11001001", 2);
            assert_eq_parse_input!(rest, 11, 0, 11);
        }
        {
            let input = ParserInput::create("0b105");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        {
            let input = ParserInput::create("0b1011_1105");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        {
            let input = ParserInput::create("015 + 19");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();

            assert_is_expression_literal_int!(parsed, "015", 8);
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("0o4157 + 19");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            assert_is_expression_literal_int!(parsed, "4157", 8);
            assert_eq_parse_input!(rest, 6, 0, 6);
        }
        {
            let input = ParserInput::create("0o18");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        Ok(())
    }

    #[test]
    fn test_expression_with_literal_int() {
        let context = default_context!();
        {
            let input = ParserInput::create("156");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            assert_is_expression_literal_int!(parsed, "156", 10);
        }
        {
            let input = ParserInput::create("0x25");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            assert_is_expression_literal_int!(parsed, "25", 16);
        }
    }

    #[test]
    fn test_expression_with_binary_ops() {
        let context = default_context!();
        {
            let input = ParserInput::create("15 + 2 * 5");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::BinaryOperation(lhs, rhs, op) = parsed.clone() {
                assert_is_expression_literal_int!(*lhs, "15", 10);
                assert_eq!(op, "+");
                if let Expression::BinaryOperation(lhs, rhs, op) = *rhs {
                    assert_is_expression_literal_int!(*lhs, "2", 10);
                    assert_is_expression_literal_int!(*rhs, "5", 10);
                    assert_eq!(op, "*");
                } else {
                    panic!("Invalid parsed expression: {:?}", parsed);
                }
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
        {
            let input = ParserInput::create("(63 - 7) * 0x11");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::BinaryOperation(lhs, rhs, op) = parsed.clone() {
                assert_is_expression_literal_int!(*rhs, "11", 16);
                assert_eq!(op, "*");
                if let Expression::BinaryOperation(lhs, rhs, op) = *lhs {
                    assert_is_expression_literal_int!(*lhs, "63", 10);
                    assert_is_expression_literal_int!(*rhs, "7", 10);
                    assert_eq!(op, "-");
                } else {
                    panic!("Invalid parsed expression: {:?}", parsed);
                }
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
        {
            let input = ParserInput::create("(63 + 7) * 0x11");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::BinaryOperation(lhs, rhs, op) = parsed.clone() {
                assert_is_expression_literal_int!(*rhs, "11", 16);
                assert_eq!(op, "*");
                if let Expression::BinaryOperation(lhs, rhs, op) = *lhs {
                    assert_is_expression_literal_int!(*lhs, "63", 10);
                    assert_is_expression_literal_int!(*rhs, "7", 10);
                    assert_eq!(op, "+");
                } else {
                    panic!("Invalid parsed expression: {:?}", parsed);
                }
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
    }

    #[test]
    fn test_expression_with_unary_ops() {
        let context = default_context!();
        {
            let input = ParserInput::create("-71");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::UnaryOperation(expr, op) = parsed {
                assert_is_expression_literal_int!(*expr, "71", 10);
                assert_eq!(op, "-");
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
        {
            let input = ParserInput::create("--71");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::UnaryOperation(expr, op) = parsed.clone() {
                assert_eq!(op, "-");
                if let Expression::UnaryOperation(expr, op) = *expr {
                    assert_is_expression_literal_int!(*expr, "71", 10);
                    assert_eq!(op, "-");
                } else {
                    panic!("Invalid parsed expression: {:?}", parsed);
                }
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
        {
            let input = ParserInput::create("-~+-0b1101_1000");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::UnaryOperation(expr, op) = parsed.clone() {
                assert_eq!(op, "-");
                if let Expression::UnaryOperation(expr, op) = *expr {
                    assert_eq!(op, "~");
                    if let Expression::UnaryOperation(expr, op) = *expr {
                        assert_eq!(op, "+");
                        if let Expression::UnaryOperation(expr, op) = *expr {
                            assert_eq!(op, "-");
                            assert_is_expression_literal_int!(*expr, "11011000", 2);
                        } else {
                            panic!("Invalid parsed expression: {:?}", parsed);
                        }
                    } else {
                        panic!("Invalid parsed expression: {:?}", parsed);
                    }
                } else {
                    panic!("Invalid parsed expression: {:?}", parsed);
                }
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
    }
}
