use std::rc::Rc;

use crate::{
    traits::has_len::HasLen,
    type_checker::types::{ObjectTypeBase, Type},
};

use super::{
    ast::{
        BinaryOperatorPrecedence, Declaration, Expression, ExpressionKind, Identifier, Statement,
        UnaryOperatorPrecedence,
    },
    combinators::{
        and, and_then1, and_then2, any_of, any_of_boxes, delimited, expect_input, map_parser,
        map_parser_error, map_parser_output, not, optional, parse_then_restart, parser_character,
        parser_character_predicate, parser_nothing, parser_token, repeat_at_least_0,
        repeat_at_least_1, skip_whitespaces, ParserInput, PositionInfo,
    },
    error::{elevate_highest_error, ParserErrorInfo, ParserErrorKind},
    parser::{run_parser, Parser},
};

#[derive(Debug, Clone, Default)]
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

pub struct KeyWords;

impl KeyWords {
    pub fn kw_is() -> String {
        "is".to_owned()
    }

    pub fn kw_let() -> String {
        "let".to_owned()
    }

    pub fn kw_namespace() -> String {
        "namespace".to_owned()
    }

    pub fn kw_fn() -> String {
        "fn".to_owned()
    }
}

pub trait IncreaseErrorLevel<InputType: Clone, OutputType> {
    fn increase_error_level(
        &self,
        diff: i32,
    ) -> impl Fn(&InputType) -> Result<(InputType, OutputType), ParserErrorInfo>;
    fn force_increase_error_level(
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

    fn force_increase_error_level(
        &self,
        diff: i32,
    ) -> impl Fn(&InputType) -> Result<(InputType, OutputType), ParserErrorInfo> {
        move |input| {
            self.run(input)
                .map_err(|e| e.with_level(e.get_level() + diff))
        }
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
            |v, begin, end| {
                Expression::LiteralInt(
                    PositionInfo::from_begin_and_end(begin, end),
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
            |v, begin, end| {
                Expression::LiteralInt(
                    PositionInfo::from_begin_and_end(begin, end),
                    v.iter().filter(|&&c| c != '_').collect::<String>(),
                    16,
                )
            },
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
            |v, begin, end| {
                Expression::LiteralInt(
                    PositionInfo::from_begin_and_end(begin, end),
                    v.iter().filter(|&&c| c != '_').collect::<String>(),
                    8,
                )
            },
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
            |v, begin, end| {
                Expression::LiteralInt(
                    PositionInfo::from_begin_and_end(begin, end),
                    v.iter().filter(|&&c| c != '_').collect::<String>(),
                    2,
                )
            },
            |e| e.with_info("In `parse_bin_literal_int`".to_string()),
        ),
        input,
    )
}

pub fn parse_literal_int(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    let (rest, parsed) = optional(parser_character('-'))
        .run(input)
        .map_err(|()| ParserErrorInfo::create(ParserErrorKind::Unknown))?;
    let (rest, int) = map_parser_error(
        any_of(vec![
            parse_hex_literal_int,
            parse_oct_literal_int,
            parse_bin_literal_int,
            parse_decimal_literal_int,
        ]),
        elevate_highest_error(2),
    )
    .run(&rest)?;
    match parsed {
        None => Ok((rest, int)),
        Some(c) => match int {
            Expression::LiteralInt(mut position, mut value, radix) => {
                value.insert(0, c);
                position.start_index -= 1;
                position.start_line_index -= 1;
                Ok((rest, Expression::LiteralInt(position, value, radix)))
            }
            _ => Err(ParserErrorInfo::create(ParserErrorKind::Unknown)),
        },
    }
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
        let mut found_pos = PositionInfo::from_parser_input_position(input);

        for op in context.unary_operators.iter() {
            let (prec, op_str) = op.get();
            if *prec > found_prec {
                if let Ok((_, pos)) = parser_token(op_str.clone()).run(input) {
                    found_op = Some(op_str.clone());
                    found_prec = *prec;
                    found_pos = pos;
                }
            }
        }

        if let Some(op_str) = found_op {
            let (_, next_input) = input
                .advance_by(op_str.len())
                .ok_or(ParserErrorInfo::create(ParserErrorKind::EndOfFile))?;
            let (next_input, rhs) = parse_expression_internal(&next_input, &context, found_prec)?;
            Ok((
                next_input,
                Expression::UnaryOperation(found_pos.until(rhs.position()), Box::new(rhs), op_str),
            ))
        } else {
            Err(ParserErrorInfo::create(ParserErrorKind::Unknown))
        }
    }
}

pub fn parse_identifier_chain(
    input: &ParserInput,
) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    let begin = PositionInfo::from_parser_input_position(input);
    let (rest, ident) = parse_identifier.run(input)?;
    let end = PositionInfo::from_parser_input_position(&rest);

    // TODO: Parse chains later, now just supports one identifier
    Ok((
        rest,
        Expression::Identifiers(begin.until(&end), vec![ident]),
    ))
}

pub fn parse_primary(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    move |input| {
        run_parser(
            map_parser_error(
                any_of_boxes(vec![
                    Box::from(map_parser_output(
                        parse_keyword("builtin".to_string()),
                        |_, begin, end| {
                            Expression::Builtin(PositionInfo::from_begin_and_end(begin, end))
                        },
                    )),
                    Box::from(parse_literal_int),
                    Box::from(parse_identifier_chain),
                    Box::from(parse_unary_operator(context.clone())),
                    Box::from(parse_parenthesis_expression(context.clone())),
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
                    .advance_by(op_str.chars().count())
                    .ok_or(ParserErrorInfo::create(ParserErrorKind::EndOfFile))?;
                let (next_input, rhs) = parse_expression_internal(&next_input, context, found_prec)
                    .map_err(|e| {
                        ParserErrorInfo::create(ParserErrorKind::EndOfFile)
                            .with_cause0(&e)
                            .with_level(e.get_level() + 5)
                    })?;
                lhs = Expression::BinaryOperation(
                    lhs.position().until(rhs.position()),
                    Box::new(lhs),
                    Box::new(rhs),
                    op_str,
                );
                input = next_input;
            } else if let Some("left") = found_assoc {
                let (_, next_input) = input
                    .advance_by(op_str.chars().count())
                    .ok_or(ParserErrorInfo::create(ParserErrorKind::EndOfFile))?;
                let (next_input, rhs) =
                    parse_expression_internal(&next_input, context, found_prec + 1).map_err(
                        |e| {
                            ParserErrorInfo::create(ParserErrorKind::EndOfFile)
                                .with_cause0(&e)
                                .with_level(e.get_level() + 5)
                        },
                    )?;
                lhs = Expression::BinaryOperation(
                    lhs.position().until(rhs.position()),
                    Box::new(lhs),
                    Box::new(rhs),
                    op_str,
                );
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
            map_parser_error(
                parse_then_restart(parser_character_predicate(
                    char::is_alphabetic,
                    "ALPHABETIC",
                )),
                |e| ParserErrorInfo::create(ParserErrorKind::ExpectedIdentifier).with_cause0(&e),
            ),
            map_parser(
                repeat_at_least_1(parser_character_predicate(
                    |c| c == '_' || c.is_alphanumeric() || c.is_ascii_digit(),
                    "ALPHANUMERIC_OR_DIGIT_OR_UNDERSCORE",
                )),
                |v, begin, end| {
                    Identifier::create(
                        PositionInfo::from_begin_and_end(begin, end),
                        v.iter().collect(),
                    )
                },
                |_| ParserErrorInfo::create(ParserErrorKind::ExpectedIdentifier),
            ),
        ),
        input,
    )
}

pub fn parse_type_base(
    input: &ParserInput,
) -> Result<(ParserInput, ObjectTypeBase), ParserErrorInfo> {
    expect_input(input)?;
    run_parser(
        map_parser_output(parse_identifier, |v, _, _| match v.name.as_str() {
            "i32" => ObjectTypeBase::Int32,
            "i64" => ObjectTypeBase::Int64,
            "u32" => ObjectTypeBase::UInt32,
            "u64" => ObjectTypeBase::UInt64,
            "bool" => ObjectTypeBase::Bool,
            "void" => ObjectTypeBase::Void,
            _ => ObjectTypeBase::UserDefined(v),
        }),
        input,
    )
}

pub fn parse_generics(
    context: &Rc<ASTParserContext>,
    input: &ParserInput,
) -> Result<(ParserInput, Option<Vec<Type>>), ParserErrorInfo> {
    match run_parser(parser_character('<'), input) {
        Ok((input, _)) => run_parser(
            and_then1(
                delimited(
                    skip_whitespaces(parser_character(',')),
                    skip_whitespaces(parse_type(
                        context.clone(),
                        parser_character_predicate(
                            |c| c == ',' || c == '>',
                            "PREDICATE_COMMA_OR_END_OF_GENERICS",
                        ),
                    )),
                    true,
                ),
                skip_whitespaces(parser_character('>')),
            ),
            &input,
        )
        .map(|(rest, generics)| (rest, Some(generics))),
        Err(_) => Ok((input.clone(), None)),
    }
}

pub fn parse_type_object(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Type), ParserErrorInfo> {
    move |input| {
        expect_input(input)?;
        let begin = PositionInfo::from_parser_input_position(input);
        let (input, type_base) = parse_type_base.run(input)?;
        let (input, generics) = parse_generics(&context, &input)?;
        let end = PositionInfo::from_parser_input_position(&input);

        Ok((
            input,
            Type::Object {
                position: begin.until(&end),
                base: type_base,
                generics,
            },
        ))
    }
}

pub fn parse_type_proposition(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Type), ParserErrorInfo> {
    move |input| {
        map_parser_output(parse_expression(context.clone()), |expr, _, _| {
            Type::Proposition(expr)
        })
        .increase_error_level(5)
        .run(input)
    }
}

pub fn parse_type<T: 'static, P>(
    context: Rc<ASTParserContext>,
    ends_with_parser: P,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Type), ParserErrorInfo>
where
    P: Fn(&ParserInput) -> Result<(ParserInput, T), ParserErrorInfo>,
{
    move |input| {
        let mut errs: Vec<ParserErrorInfo> = Vec::new();

        match parse_type_object(context.clone()).run(input) {
            Err(e) => errs.push(e),
            Ok(v) => match ends_with_parser.run(&v.0) {
                Err(e) => errs.push(e),
                Ok(..) => return Ok(v),
            },
        }

        match parse_expression(context.clone()).run(input) {
            Err(e) => errs.push(e),
            Ok(v) => match ends_with_parser.run(&v.0) {
                Err(e) => errs.push(e),
                Ok(..) => return Ok((v.0, Type::Proposition(v.1))),
            },
        }

        Err(elevate_highest_error(2)(errs))
    }
}

pub fn parse_keyword(
    kw: String,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, PositionInfo), ParserErrorInfo> {
    move |input| {
        let start = PositionInfo::from_parser_input_position(input);
        let (input, ident) = parse_identifier(input).map_err(|e| {
            ParserErrorInfo::create(ParserErrorKind::Expected(format!("Keyword {}", kw)))
                .with_level(e.get_level())
        })?;
        if ident.name != kw {
            Err(ParserErrorInfo::create(ParserErrorKind::ExpectedKeyword(
                kw.clone(),
            )))
        } else {
            let end = PositionInfo::from_parser_input_position(&input);
            Ok((input, start.until(&end)))
        }
    }
}

pub fn parse_variable_declaration(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Declaration), ParserErrorInfo> {
    move |input| {
        let begin = PositionInfo::from_parser_input_position(input);

        let (input, _) = parse_keyword(KeyWords::kw_let()).run(input)?;
        let (mut input, name_ident) =
            skip_whitespaces(parse_identifier.force_increase_error_level(5)).run(&input)?;

        let mut var_type = None;

        if let Ok((new_input, _)) = skip_whitespaces(parser_character(':')).run(&input) {
            let (new_input, parsed_type) = skip_whitespaces(parse_type(
                context.clone(),
                skip_whitespaces(parser_character('=')),
            ))
            .run(&new_input)?;
            var_type = Some(parsed_type);
            input = new_input;
        }

        if let Ok((new_input, _)) = skip_whitespaces(parser_character(';')).run(&input) {
            let position = begin.until(&PositionInfo::from_parser_input_position(&new_input));
            return Ok((
                new_input,
                Declaration::Variable(position, name_ident, var_type, None),
            ));
        }

        let (input, _) = skip_whitespaces(parser_character('='))
            .force_increase_error_level(2)
            .run(&input)
            .map_err(|e| {
                ParserErrorInfo::create(ParserErrorKind::Expected(
                    if var_type.is_none() {
                        "';' or type hint or variable initialization"
                    } else {
                        "';' or variable initialization"
                    }
                    .to_string(),
                ))
                .with_level(e.get_level())
            })?;
        let (input, expr) =
            skip_whitespaces(parse_expression(context.clone()).force_increase_error_level(5))
                .run(&input)
                .map_err(|e| {
                    ParserErrorInfo::create(ParserErrorKind::Expected("expression".to_string()))
                        .with_level(e.get_level())
                })?;

        Ok((
            input,
            Declaration::Variable(
                begin.until(expr.position()),
                name_ident,
                var_type,
                Some(Box::new(expr)),
            ),
        ))
    }
}

pub fn expect_semicolon<OutputType, P>(
    parser: P,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, OutputType), ParserErrorInfo>
where
    P: Parser<ParserInput, ParserErrorInfo, OutputType>,
{
    and_then1(
        parser,
        map_parser_error(parser_character(';'), |e| {
            ParserErrorInfo::create(ParserErrorKind::Expected("';'".to_string()))
                .with_level(e.get_level() + 3)
        }),
    )
}

pub fn parse_program(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Declaration), ParserErrorInfo> {
    move |input| {
        let (rest, namespace) = parse_namespace(context.clone()).run(input)?;
        let (rest, _) = skip_whitespaces(parser_nothing()).run(&rest)?;
        if rest.is_empty() {
            Ok((rest, namespace))
        } else {
            Err(ParserErrorInfo::create(ParserErrorKind::Expected(
                "End of file".to_string(),
            )))
        }
    }
}

pub fn parse_namespace(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Declaration), ParserErrorInfo> {
    move |input| {
        let (rest, kw_pos) =
            skip_whitespaces(parse_keyword(KeyWords::kw_namespace())).run(input)?;
        let (rest, name) = skip_whitespaces(parse_identifier)
            .force_increase_error_level(5)
            .run(&rest)?;
        let (mut rest, _) = skip_whitespaces(parser_character('{'))
            .force_increase_error_level(5)
            .run(&rest)?;
        let mut decls = Vec::new();
        loop {
            (rest, _) = skip_whitespaces(parser_nothing()).run(&rest)?;
            if rest.is_empty() {
                return Err(ParserErrorInfo::create(ParserErrorKind::EndOfFile));
            }
            match parser_character('}').run(&rest) {
                Err(_) => {}
                Ok((remaining, _)) => {
                    let position =
                        kw_pos.until(&PositionInfo::from_parser_input_position(&remaining));
                    return Ok((remaining, Declaration::Namespace(position, name, decls)));
                }
            }
            let parsed;
            (rest, parsed) = parse_namespace_declaration(context.clone()).run(&rest)?;
            decls.push(parsed);
        }
    }
}

pub fn parse_function_parameter(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, (Identifier, Type)), ParserErrorInfo> {
    move |input| {
        let (rest, name) = skip_whitespaces(parse_identifier).run(input)?;
        let (rest, _) = skip_whitespaces(parser_character(':'))
            .force_increase_error_level(1)
            .run(&rest)?;
        let (rest, ptype) = parse_type(
            context.clone(),
            skip_whitespaces(parser_character_predicate(
                |c| c == ',' || c == ')',
                "PREDICATE_COMMA_OR_END_FUNCTION_PARAMETERS",
            )),
        )
        .force_increase_error_level(2)
        .run(&rest)?;

        Ok((rest, (name, ptype)))
    }
}

pub fn parse_function_declaration(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Declaration), ParserErrorInfo> {
    move |input| {
        let (rest, kw_pos) = skip_whitespaces(parse_keyword(KeyWords::kw_fn())).run(input)?;

        let (rest, fn_name) = skip_whitespaces(parse_identifier)
            .force_increase_error_level(5)
            .run(&rest)?;

        let (rest, _) = skip_whitespaces(parser_character('('))
            .force_increase_error_level(5)
            .run(&rest)?;

        let (rest, args) = delimited(
            and_then1(
                skip_whitespaces(parser_character(',')),
                skip_whitespaces(parser_nothing()),
            ),
            parse_function_parameter(context.clone()),
            false,
        )
        .run(&rest)?;

        let (rest, _) = skip_whitespaces(parser_character(')'))
            .force_increase_error_level(5)
            .run(&rest)?;

        let (rest, rtype) = match skip_whitespaces(parser_character(':')).run(&rest) {
            Err(_) => (rest, None),
            Ok((rest, _)) => map_parser_output(
                skip_whitespaces(parse_type(
                    context.clone(),
                    skip_whitespaces(parser_character('{')),
                )),
                |v, _, _| Some(v),
            )
            .force_increase_error_level(5)
            .run(&rest)?,
        };

        let (rest, statement) = skip_whitespaces(parse_statement(context.clone()))
            .force_increase_error_level(5)
            .run(&rest)?;

        let position = kw_pos.until(&PositionInfo::from_parser_input_position(&rest));
        Ok((
            rest,
            Declaration::Function(position, fn_name, args, rtype, Some(Box::from(statement))),
        ))
    }
}

pub fn parse_namespace_declaration(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Declaration), ParserErrorInfo> {
    move |input| {
        expect_input(input)?;
        run_parser(
            map_parser_error(
                any_of_boxes(vec![
                    Box::from(parse_namespace(context.clone())),
                    Box::from(parse_function_declaration(context.clone())),
                ]),
                elevate_highest_error(2),
            ),
            input,
        )
    }
}

pub fn parse_statement_declaration(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Declaration), ParserErrorInfo> {
    move |input| {
        expect_input(input)?;
        run_parser(
            map_parser_error(
                any_of_boxes(vec![Box::from(expect_semicolon(
                    parse_variable_declaration(context.clone()),
                ))]),
                elevate_highest_error(2),
            ),
            input,
        )
    }
}

pub fn parse_statement(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Statement), ParserErrorInfo> {
    move |input| {
        expect_input(input)?;
        run_parser(
            map_parser_error(
                any_of_boxes(vec![
                    Box::from(parse_block(context.clone())),
                    Box::from(map_parser_output(
                        parse_statement_declaration(context.clone()),
                        |decl, _, _| Statement::Declaration(decl),
                    )),
                    Box::from(map_parser_output(
                        expect_semicolon(parse_expression(context.clone())),
                        |expr, _, _| Statement::Expression(expr),
                    )),
                ]),
                elevate_highest_error(2),
            ),
            input,
        )
    }
}

pub fn parse_block(
    context: Rc<ASTParserContext>,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, Statement), ParserErrorInfo> {
    move |input| {
        run_parser(
            and_then2(
                skip_whitespaces(parser_character('{')),
                and_then1(
                    map_parser(
                        repeat_at_least_0(skip_whitespaces(parse_statement(context.clone()))),
                        |statements, begin, end| {
                            Statement::Expression(Expression::Block(
                                PositionInfo::from_begin_and_end(begin, end),
                                statements,
                            ))
                        },
                        |_| ParserErrorInfo::create(ParserErrorKind::Unknown),
                    ),
                    skip_whitespaces(parser_character('}')),
                ),
            ),
            input,
        )
    }
}

pub fn parse_string(input: &ParserInput) -> Result<(ParserInput, Expression), ParserErrorInfo> {
    let begin = PositionInfo::from_parser_input_position(input);
    let (mut input, _) = map_parser_error(parser_character('"'), |e| {
        ParserErrorInfo::create(ParserErrorKind::Expected("string litteral".to_string()))
            .with_level(e.get_level())
            .with_cause0(&e)
    })
    .run(input)?;
    let mut result: String = String::new();
    let mut is_escaping = false;
    let mut string_terminated = false;
    while let Ok((rest, c)) = parser_character_predicate(|_| true, "ANY_CHARACTER").run(&input) {
        input = rest;
        if c == '\r' {
            continue;
        }
        if is_escaping {
            result.push(match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\'' => '\'',
                '"' => '"',
                '0' => '\0',
                _ => c,
            });
            is_escaping = false;
            continue;
        }
        if c == '\\' {
            is_escaping = true;
            continue;
        }
        if c == '\n' {
            Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral).with_level(5))?
        }
        if c == '"' {
            string_terminated = true;
            break;
        }
        result.push(c);
    }
    if !string_terminated {
        Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral).with_level(5))?
    }

    let expr = Expression::LiteralString(
        begin.until(&PositionInfo::from_parser_input_position(&input)),
        result,
    );

    Ok((input, expr))
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
            if let Expression::LiteralInt(_, value, radix) = $parsed {
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
            if let Expression::BinaryOperation(_, lhs, rhs, op) = parsed.clone() {
                assert_is_expression_literal_int!(*lhs, "15", 10);
                assert_eq!(op, "+");
                if let Expression::BinaryOperation(_, lhs, rhs, op) = *rhs {
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
            if let Expression::BinaryOperation(_, lhs, rhs, op) = parsed.clone() {
                assert_is_expression_literal_int!(*rhs, "11", 16);
                assert_eq!(op, "*");
                if let Expression::BinaryOperation(_, lhs, rhs, op) = *lhs {
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
            if let Expression::BinaryOperation(_, lhs, rhs, op) = parsed.clone() {
                assert_is_expression_literal_int!(*rhs, "11", 16);
                assert_eq!(op, "*");
                if let Expression::BinaryOperation(_, lhs, rhs, op) = *lhs {
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
            assert_is_expression_literal_int!(parsed, "-71", 10);
        }
        {
            let input = ParserInput::create("--71");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::UnaryOperation(_, expr, op) = parsed.clone() {
                assert_eq!(op, "-");
                assert_is_expression_literal_int!(*expr, "-71", 10);
            } else {
                panic!("Invalid parsed expression: {:?}", parsed);
            }
        }
        {
            let input = ParserInput::create("-~+-0b1101_1000");
            let (rest, parsed) = run_parser(parse_expression(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            if let Expression::UnaryOperation(_, expr, op) = parsed.clone() {
                assert_eq!(op, "-");
                if let Expression::UnaryOperation(_, expr, op) = *expr {
                    assert_eq!(op, "~");
                    if let Expression::UnaryOperation(_, expr, op) = *expr {
                        assert_eq!(op, "+");
                        assert_is_expression_literal_int!(*expr, "-11011000", 2);
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

    #[test]
    pub fn test_statement() {
        let context = default_context!();
        {
            let input = ParserInput::create("15+6;");
            let (rest, parsed) = run_parser(parse_statement(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            match parsed {
                Statement::Expression(e) => {
                    assert_eq!(
                        e,
                        Expression::BinaryOperation(
                            PositionInfo::create(0, 0, 0, 0)
                                .until(&PositionInfo::create(4, 0, 4, 0)),
                            Box::new(Expression::LiteralInt(
                                PositionInfo::create(0, 0, 0, 0)
                                    .until(&PositionInfo::create(2, 0, 2, 0)),
                                "15".to_string(),
                                10
                            )),
                            Box::new(Expression::LiteralInt(
                                PositionInfo::create(3, 0, 3, 0)
                                    .until(&PositionInfo::create(4, 0, 4, 0)),
                                "6".to_string(),
                                10
                            )),
                            "+".to_string()
                        )
                    );
                }
                _ => panic!("Invalid parsed statement: {:?}", parsed),
            }
        }
    }

    #[test]
    pub fn test_statement_declaration() {
        let context = default_context!();
        {
            let input = ParserInput::create("let x = 0x15;");
            let (rest, parsed) =
                run_parser(parse_statement_declaration(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            assert_eq!(
                parsed,
                Declaration::Variable(
                    PositionInfo::create(0, 0, 0, 0).until(&PositionInfo::create(12, 0, 12, 0)),
                    Identifier::new(
                        PositionInfo::create(4, 0, 4, 0).until(&PositionInfo::create(5, 0, 5, 0)),
                        "x"
                    ),
                    None,
                    Some(Box::from(Expression::LiteralInt(
                        PositionInfo::create(8, 0, 8, 0).until(&PositionInfo::create(12, 0, 12, 0)),
                        "15".to_string(),
                        16
                    )))
                )
            )
        }
        {
            let input = ParserInput::create("let yy: u64 = -0b110;");
            let (rest, parsed) =
                run_parser(parse_statement_declaration(context.clone()), &input).unwrap();
            assert!(rest.is_empty());
            assert_eq!(
                parsed,
                Declaration::Variable(
                    PositionInfo::create(0, 0, 0, 0).until(&PositionInfo::create(20, 0, 20, 0)),
                    Identifier::new(
                        PositionInfo::create(4, 0, 4, 0).until(&PositionInfo::create(6, 0, 6, 0)),
                        "yy"
                    ),
                    Some(Type::Object {
                        position: PositionInfo::create(8, 0, 8, 0)
                            .until(&PositionInfo::create(11, 0, 11, 0)),
                        base: ObjectTypeBase::UInt64,
                        generics: None
                    }),
                    Some(Box::from(Expression::LiteralInt(
                        PositionInfo::create(14, 0, 14, 0)
                            .until(&PositionInfo::create(20, 0, 20, 0)),
                        "-110".to_string(),
                        2
                    )))
                )
            )
        }
    }
}
