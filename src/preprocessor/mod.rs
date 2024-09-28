use core::fmt;

use crate::{
    parsing::{
        ast::{BinaryOperatorPrecedence, Expression, UnaryOperatorPrecedence},
        astparser::{
            parse_keyword, parse_literal_int, parse_string, ASTParserContext, IncreaseErrorLevel,
        },
        combinators::{map_parser_error, or, skip_whitespaces, Either, ParserInput},
        error::{ParserErrorInfo, ParserErrorKind},
        parser::Parser,
    },
    type_checker::evaluator::parse_signed_int,
};

#[derive(Clone, Debug, Default)]
pub struct Preprocessor {
    pub context: ASTParserContext,
}

impl fmt::Display for PreprocessorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReservedOperator(op) => write!(f, "Operator \"{}\" is reserved", op),
            Self::OperatorAlreadyDefined(op) => write!(f, "Operator \"{}\" already defined", op),
            Self::UnknownDirective(directive, ..) => {
                write!(f, "Unknown directive \"{}\"", directive)
            }
        }
    }
}

fn to_lines(input: String, lines: &mut Vec<String>) {
    let mut line = String::new();

    let mut escaping = false;

    for character in input.chars() {
        if character == '\r' {
            continue;
        }
        if escaping {
            escaping = false;
            line.push(character);
            continue;
        }
        if character == '\n' {
            lines.push(line);
            line = String::new();
            continue;
        }
        if character == '\\' {
            escaping = true;
            continue;
        }
        line.push(character);
    }
    lines.push(line);
}

struct PreprocessorKeyword;

impl PreprocessorKeyword {
    pub fn kw_operator() -> String {
        "operator".to_owned()
    }

    pub fn kw_unary() -> String {
        "unary".to_owned()
    }

    pub fn kw_binary() -> String {
        "binary".to_owned()
    }

    pub fn kw_left_assoc() -> String {
        "assoc_left".to_owned()
    }

    pub fn kw_right_assoc() -> String {
        "assoc_right".to_owned()
    }
}

fn parse_operator_definition(
    input: &ParserInput,
) -> Result<
    (
        ParserInput,
        Either<UnaryOperatorPrecedence, BinaryOperatorPrecedence>,
    ),
    ParserErrorInfo,
> {
    let (input, _) = parse_keyword(PreprocessorKeyword::kw_operator()).run(input)?;
    let (input, optype) = skip_whitespaces(map_parser_error(
        or(
            parse_keyword(PreprocessorKeyword::kw_unary()),
            parse_keyword(PreprocessorKeyword::kw_binary()),
        ),
        |_| ParserErrorInfo::create(ParserErrorKind::Expected("'binary' or 'unary'".to_string())),
    ))
    .force_increase_error_level(5)
    .run(&input)?;

    let (input, operator) = skip_whitespaces(parse_string)
        .force_increase_error_level(5)
        .run(&input)?;

    let operator = match operator {
        Expression::LiteralString(_, value) => value,
        _ => Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
            .with_info("Operator must be a string litteral".to_string()))?,
    };

    match optype {
        Either::First(_) => {
            let (input, precedence) = skip_whitespaces(parse_literal_int)
                .force_increase_error_level(5)
                .run(&input)?;

            let precedence = match precedence {
                Expression::LiteralInt(_, value, radix) => {
                    let ival = parse_signed_int(&value, radix).map_err(|e| {
                        ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                            .with_info(format!("{:?}", e))
                    })?;
                    let ival = match ival {
                        Either::First(i) => {
                            if i > u64::MAX as i128 {
                                Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                                    .with_info("Illegal negative precedence".to_string()))?
                            }
                            i as u128
                        }
                        Either::Second(u) => u,
                    };
                    if ival > u64::MAX as u128 {
                        Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                            .with_info("Precedence out of range for u64".to_string()))?
                    }
                    ival as u64
                }
                _ => Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                    .with_info("Precedence must be a u64 litteral".to_string()))?,
            };

            Ok((
                input,
                Either::First(UnaryOperatorPrecedence::create(precedence, operator)),
            ))
        }
        Either::Second(_) => {
            let (input, associativity) = skip_whitespaces(map_parser_error(
                or(
                    parse_keyword(PreprocessorKeyword::kw_left_assoc()),
                    parse_keyword(PreprocessorKeyword::kw_right_assoc()),
                ),
                |_| {
                    ParserErrorInfo::create(ParserErrorKind::Expected(
                        "'assoc_left' or 'assoc_right'".to_string(),
                    ))
                },
            ))
            .run(&input)?;

            let (input, precedence) = skip_whitespaces(parse_literal_int)
                .force_increase_error_level(5)
                .run(&input)?;

            let precedence = match precedence {
                Expression::LiteralInt(_, value, radix) => {
                    let ival = parse_signed_int(&value, radix).map_err(|e| {
                        ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                            .with_info(format!("{:?}", e))
                    })?;
                    let ival = match ival {
                        Either::First(i) => {
                            if i > u64::MAX as i128 {
                                Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                                    .with_info("Illegal negative precedence".to_string()))?
                            }
                            i as u128
                        }
                        Either::Second(u) => u,
                    };
                    if ival > u64::MAX as u128 {
                        Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                            .with_info("Precedence out of range for u64".to_string()))?
                    }
                    ival as u64
                }
                _ => Err(ParserErrorInfo::create(ParserErrorKind::InvalidLiteral)
                    .with_info("Precedence must be a u64 litteral".to_string()))?,
            };

            Ok((
                input,
                Either::Second(match associativity {
                    Either::First(_) => {
                        BinaryOperatorPrecedence::LeftAssociative(precedence, operator)
                    }
                    Either::Second(_) => {
                        BinaryOperatorPrecedence::RightAssociative(precedence, operator)
                    }
                }),
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub enum PreprocessorError {
    UnknownDirective(String, Vec<ParserErrorInfo>),
    OperatorAlreadyDefined(String),
    ReservedOperator(String),
}

impl Preprocessor {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    fn parse_preprocessor_line(
        &mut self,
        line: &mut String,
        idx: usize,
    ) -> Result<(), PreprocessorError> {
        let input = ParserInput::create_from_string(line[idx..].to_string());

        let mut errors = Vec::new();

        match parse_operator_definition.run(&input) {
            Ok((_, parsed)) => {
                match parsed {
                    Either::First(unary) => {
                        if unary.get().1 == "=" {
                            Err(PreprocessorError::ReservedOperator("=".to_string()))?
                        }
                        if self
                            .context
                            .unary_operators
                            .iter()
                            .any(|op| op.is_same_operator(&unary))
                        {
                            Err(PreprocessorError::OperatorAlreadyDefined(
                                unary.get().1.clone(),
                            ))?
                        }
                        self.context.unary_operators.push(unary);
                    }
                    Either::Second(binary) => {
                        if binary.get().1 == "=" {
                            Err(PreprocessorError::ReservedOperator("=".to_string()))?
                        }
                        if self
                            .context
                            .binary_operators
                            .iter()
                            .any(|op| op.is_same_operator(&binary))
                        {
                            Err(PreprocessorError::OperatorAlreadyDefined(
                                binary.get().1.clone(),
                            ))?
                        }
                        self.context.binary_operators.push(binary);
                    }
                }
                line.clear();
                return Ok(());
            }

            Err(e) => errors.push(e),
        }

        Err(PreprocessorError::UnknownDirective(
            line.to_string(),
            errors,
        ))
    }

    fn parse_line(&mut self, line: &mut String) -> Result<(), PreprocessorError> {
        let mut idx = 0;
        let mut preprocessor_line = false;
        while let Some(character) = line.chars().nth(idx) {
            idx += 1;
            if character == '#' {
                preprocessor_line = true;
                break;
            }
            if character == '/' && line.chars().nth(idx) == Some('/') {
                line.clear();
                break;
            }
            if !character.is_whitespace() {
                break;
            }
        }
        if !preprocessor_line {
            Ok(())
        } else {
            self.parse_preprocessor_line(line, idx)
        }
    }

    pub fn preprocess(&mut self, input: String) -> Result<String, PreprocessorError> {
        let mut lines = Vec::new();
        to_lines(input, &mut lines);

        for line in lines.iter_mut() {
            self.parse_line(line)?;
        }

        Ok(lines
            .iter()
            .filter_map(|line| {
                if line.chars().any(|c| !c.is_whitespace()) {
                    Some(line.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<String>>()
            .join("\n"))
    }
}
