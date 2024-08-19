use super::{
    ast::{Expression, ExpressionKind},
    combinators::{
        and, and_then, any_of, map_parser, map_parser_error, not, parser_character_predicate,
        parser_token, repeat_at_least_1, ParserInput,
    },
    error::{ParserErrorInfo, ParserErrorKind},
    parser::run_parser,
};

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
            and_then(
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
            and_then(
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
            and_then(
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
