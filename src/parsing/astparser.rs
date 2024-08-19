use std::rc::Rc;

use super::{
    ast::{BinaryOperatorPrecedence, Expression, ExpressionKind, UnaryOperatorPrecedence},
    combinators::{
        and, and_then1, and_then2, any_of, any_of_boxes, map_parser, map_parser_error, not,
        parser_character, parser_character_predicate, parser_token, repeat_at_least_1, ParserInput,
    },
    error::{ParserErrorInfo, ParserErrorKind},
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
                map_parser_error(parser_token("0x".to_string()), |_| ()),
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
            ),
            |v| Expression::LiteralInt(v.iter().filter(|&&c| c != '_').collect::<String>(), 16),
            |_| {
                ParserErrorInfo::create(ParserErrorKind::ExpectedExpressionKind(
                    ExpressionKind::LiteralInt,
                ))
            },
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
                map_parser_error(parser_token("0o".to_string()), |_| ()),
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
            ),
            |v| Expression::LiteralInt(v.iter().filter(|&&c| c != '_').collect::<String>(), 8),
            |_| {
                ParserErrorInfo::create(ParserErrorKind::ExpectedExpressionKind(
                    ExpressionKind::LiteralInt,
                ))
            },
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
                map_parser_error(parser_token("0b".to_string()), |_| ()),
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
            ),
            |v| Expression::LiteralInt(v.iter().filter(|&&c| c != '_').collect::<String>(), 2),
            |_| {
                ParserErrorInfo::create(ParserErrorKind::ExpectedExpressionKind(
                    ExpressionKind::LiteralInt,
                ))
            },
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
            |errs| ParserErrorInfo::create(ParserErrorKind::SubErrorList(errs)),
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
                and_then2(parser_character('('), parse_expression(context.clone())),
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
            let (next_input, rhs) = parse_primary(context.clone()).run(&next_input)?;
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
                |errs| ParserErrorInfo::create(ParserErrorKind::SubErrorList(errs)),
            ),
            input,
        )
    }
}

// TODO: test
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
    let (mut input, mut lhs) = parse_primary(context.clone()).run(input)?;

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

#[cfg(test)]
mod tests {
    use crate::{assert_eq_parse_input, assert_is_error_print_ok, parsing::parser::run_parser};

    use super::*;

    #[test]
    fn test_parse_literal_int() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("123 + 456");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "123");
                    assert_eq!(radix, 10)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("25_912_956 + 18");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "25912956");
                    assert_eq!(radix, 10)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
            assert_eq_parse_input!(rest, 10, 0, 10);
        }
        {
            let input = ParserInput::create("195f");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        {
            let input = ParserInput::create("0x1_fA_0E + 18");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "1fA0E");
                    assert_eq!(radix, 16)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
            assert_eq_parse_input!(rest, 9, 0, 9);
        }
        {
            let input = ParserInput::create("0x1h");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        {
            let input = ParserInput::create("0b1001+5");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "1001");
                    assert_eq!(radix, 2)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
            assert_eq_parse_input!(rest, 6, 0, 6);
        }
        {
            let input = ParserInput::create("0b1100_1001 + 19");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "11001001");
                    assert_eq!(radix, 2)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
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
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "015");
                    assert_eq!(radix, 8)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("0o4157 + 19");
            let (rest, parsed) = run_parser(parse_literal_int, &input).unwrap();
            match parsed {
                Expression::LiteralInt(v, radix) => {
                    assert_eq!(v, "4157");
                    assert_eq!(radix, 8)
                }
                _ => panic!("Expected LiteralInt, got: {:?}", parsed),
            }
            assert_eq_parse_input!(rest, 6, 0, 6);
        }
        {
            let input = ParserInput::create("0o18");
            assert_is_error_print_ok!(run_parser(parse_literal_int, &input));
        }
        Ok(())
    }
}
