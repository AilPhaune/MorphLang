use std::{char, rc::Rc};

use super::{
    error::{ParserErrorInfo, ParserErrorKind},
    parser::Parser,
};

#[macro_export]
macro_rules! assert_eq_parse_input {
    ($parser_input:expr, $current_index:expr, $current_line:expr, $current_line_index:expr) => {
        assert_eq!(
            $parser_input.current_index, $current_index,
            "Wrong current_index."
        );
        assert_eq!(
            $parser_input.current_line, $current_line,
            "Wrong current_line."
        );
        assert_eq!(
            $parser_input.current_line_index, $current_line_index,
            "Wrong current_line_index."
        );
    };
}

#[macro_export]
macro_rules! assert_eq_position_info {
    ($pos_info: expr, $start_index: expr, $start_line: expr, $start_line_index: expr, $end_index: expr, $end_line: expr, $end_line_index: expr) => {
        assert_eq!($pos_info.start_index, $start_index, "Wrong start_index.");
        assert_eq!($pos_info.start_line, $start_line, "Wrong start_line.");
        assert_eq!(
            $pos_info.start_line_index, $start_line_index,
            "Wrong start_line_index."
        );
        assert_eq!($pos_info.end_index, $end_index, "Wrong end_index.");
        assert_eq!($pos_info.end_line, $end_line, "Wrong end_line.");
        assert_eq!(
            $pos_info.end_line_index, $end_line_index,
            "Wrong end_line_index."
        );
    };
}

#[derive(Debug, Clone)]
pub struct ParserInput {
    pub code: Rc<String>,
    pub current_index: usize,
    pub current_line: usize,
    pub current_line_index: usize,
}

impl ParserInput {
    pub fn create(code: &str) -> Self {
        Self {
            code: Rc::new(code.to_string()),
            current_index: 0,
            current_line: 0,
            current_line_index: 0,
        }
    }

    pub fn get_char(&self) -> Option<char> {
        self.code.chars().nth(self.current_index)
    }

    pub fn get_char_relative(&self, index: isize) -> Option<char> {
        self.code.chars().nth(self.current_index + (index as usize))
    }

    pub fn advance(&self) -> Option<(char, ParserInput)> {
        self.get_char().map(|c| {
            (
                c,
                if c == '\n' {
                    Self {
                        code: self.code.to_owned(),
                        current_index: self.current_index + 1,
                        current_line: self.current_line + 1,
                        current_line_index: 0,
                    }
                } else {
                    Self {
                        code: self.code.to_owned(),
                        current_index: self.current_index + 1,
                        current_line: self.current_line,
                        current_line_index: self.current_line_index + 1,
                    }
                },
            )
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PositionInfo {
    pub start_index: usize,
    pub start_line: usize,
    pub start_line_index: usize,
    pub end_index: usize,
    pub end_line: usize,
    pub end_line_index: usize,
}

impl PositionInfo {
    pub fn create(start_index: usize, start_line: usize, start_line_index: usize) -> Self {
        Self {
            start_index,
            start_line,
            start_line_index,
            end_index: start_index,
            end_line: start_line,
            end_line_index: start_line,
        }
    }

    pub fn from_parser_input_position(p: &ParserInput) -> Self {
        Self {
            start_index: p.current_index,
            start_line: p.current_line,
            start_line_index: p.current_line_index,
            end_index: p.current_index,
            end_line: p.current_line,
            end_line_index: p.current_line_index,
        }
    }

    pub fn until(&self, end: &Self) -> Self {
        Self {
            start_index: self.start_index,
            start_line: self.start_line,
            start_line_index: self.start_line_index,
            end_index: end.end_index,
            end_line: end.end_line,
            end_line_index: end.end_line_index,
        }
    }
}

pub fn parser_character_predicate(
    predicate: fn(char) -> bool,
    predicate_info: &str,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, char), ParserErrorInfo> {
    let pred = predicate_info.to_string();
    move |input: &ParserInput| match input.advance() {
        None => Err(ParserErrorInfo::create(ParserErrorKind::EndOfFile)),
        Some((ch, rest)) => {
            if predicate(ch) {
                Ok((rest, ch))
            } else {
                Err(ParserErrorInfo::create(
                    ParserErrorKind::ExpectedCharacter {
                        predicate_info: pred.clone(),
                    },
                ))
            }
        }
    }
}

pub fn skip_whitespaces<ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, OutputType), ErrorType>
where
    P: Parser<ParserInput, ErrorType, OutputType>,
{
    move |input| {
        let (remaining_input, _) = repeat_at_least_0(parser_character_predicate(
            char::is_whitespace,
            "WHITESPACE",
        ))
        .run(input)
        .unwrap();
        parser.run(&remaining_input)
    }
}

pub fn repeat_at_least_0<InputType: Clone, ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&InputType) -> Result<(InputType, Vec<OutputType>), ()>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| {
        let mut output: Vec<OutputType> = Vec::new();
        let mut remaining_input: InputType = input.clone();
        loop {
            match parser.run(&remaining_input) {
                Err(_) => {
                    return Ok((remaining_input, output));
                }
                Ok((rest, value)) => {
                    remaining_input = rest;
                    output.push(value);
                }
            }
        }
    }
}

pub fn repeat_at_least_1<InputType: Clone, ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&InputType) -> Result<(InputType, Vec<OutputType>), ()>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| {
        let mut output: Vec<OutputType> = Vec::new();
        let mut remaining_input: InputType = input.clone();
        loop {
            match parser.run(&remaining_input) {
                Err(_) => {
                    if output.is_empty() {
                        return Err(());
                    }
                    return Ok((remaining_input, output));
                }
                Ok((rest, value)) => {
                    remaining_input = rest;
                    output.push(value);
                }
            }
        }
    }
}

pub fn parser_token(
    token: String,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, PositionInfo), ParserErrorInfo> {
    move |input| {
        let pos = PositionInfo::from_parser_input_position(input);
        let mut remaining_input = input.clone();
        for c in token.chars() {
            if let Some((ch, rest)) = remaining_input.advance() {
                if ch != c {
                    return Err(ParserErrorInfo::create(ParserErrorKind::ExpectedToken {
                        token: token.clone(),
                    }));
                }
                remaining_input = rest;
            } else {
                return Err(ParserErrorInfo::create(ParserErrorKind::EndOfFile));
            }
        }
        let end_pos = PositionInfo::from_parser_input_position(&remaining_input);
        Ok((remaining_input, pos.until(&end_pos)))
    }
}

pub fn map_parser_error<InputType: Clone, ErrorType1, OutputType, ErrorType2, P, F>(
    parser: P,
    mapper_function: F,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType), ErrorType2>
where
    P: Parser<InputType, ErrorType1, OutputType>,
    F: Fn(ErrorType1) -> ErrorType2,
{
    move |input| parser.run(input).map_err(&mapper_function)
}

pub fn and_then<InputType: Clone, ErrorType, OutputType1, OutputType2, P1, P2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType2), ErrorType>
where
    P1: Parser<InputType, ErrorType, OutputType1>,
    P2: Parser<InputType, ErrorType, OutputType2>,
{
    move |input| {
        let (remaining_input, _) = parser1.run(input)?;
        parser2.run(&remaining_input)
    }
}

pub fn and<InputType: Clone, ErrorType, OutputType1, OutputType2, P1, P2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType1), ErrorType>
where
    P1: Parser<InputType, ErrorType, OutputType1>,
    P2: Parser<InputType, ErrorType, OutputType2>,
{
    move |input| {
        let (remaining_input, parsed) = parser1.run(input)?;
        parser2.run(&remaining_input)?;
        Ok((remaining_input, parsed))
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_is_error_print_ok, parsing::parser::run_parser};

    use super::*;

    #[test]
    fn test_char_predicate() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("abc");
            let (rest, parsed_char) = run_parser(
                parser_character_predicate(char::is_alphabetic, "ALPHABETIC"),
                &input,
            )?;
            assert_eq!(parsed_char, 'a');
            assert_eq_parse_input!(rest, 1, 0, 1);
        }
        {
            let input = ParserInput::create("abc");
            assert_is_error_print_ok!(run_parser(
                parser_character_predicate(char::is_numeric, "NUMERIC"),
                &input,
            ));
        }
        {
            let input = ParserInput::create("1234");
            let (rest, parsed_char) = run_parser(
                parser_character_predicate(char::is_numeric, "NUMERIC"),
                &input,
            )?;
            assert_eq!(parsed_char, '1');
            assert_eq_parse_input!(rest, 1, 0, 1);
        }
        Ok(())
    }

    #[test]
    fn test_repeat_at_least_0() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("abcd012");
            let (rest, parsed) = run_parser(
                repeat_at_least_0(parser_character_predicate(
                    char::is_alphabetic,
                    "ALPHABETIC",
                )),
                &input,
            )
            .unwrap();
            assert_eq!(parsed.into_iter().collect::<String>(), "abcd");
            assert_eq_parse_input!(rest, 4, 0, 4);
        }
        {
            let input = ParserInput::create("189ivkc");
            let (rest, parsed) = run_parser(
                repeat_at_least_0(parser_character_predicate(char::is_numeric, "NUMERIC")),
                &input,
            )
            .unwrap();
            assert_eq!(parsed.into_iter().collect::<String>(), "189");
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("ddsc993");
            let (rest, parsed) = run_parser(
                repeat_at_least_0(parser_character_predicate(char::is_numeric, "NUMERIC")),
                &input,
            )
            .unwrap();
            assert!(parsed.is_empty());
            assert_eq_parse_input!(rest, 0, 0, 0);
        }
        Ok(())
    }

    #[test]
    fn test_repeat_at_least_1() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("abcd012");
            let (rest, parsed) = run_parser(
                repeat_at_least_1(parser_character_predicate(
                    char::is_alphabetic,
                    "ALPHABETIC",
                )),
                &input,
            )
            .unwrap();
            assert_eq!(parsed.into_iter().collect::<String>(), "abcd");
            assert_eq_parse_input!(rest, 4, 0, 4);
        }
        {
            let input = ParserInput::create("189ivkc");
            let (rest, parsed) = run_parser(
                repeat_at_least_1(parser_character_predicate(char::is_numeric, "NUMERIC")),
                &input,
            )
            .unwrap();
            assert_eq!(parsed.into_iter().collect::<String>(), "189");
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("ddsc993");
            assert_is_error_print_ok!(run_parser(
                repeat_at_least_1(parser_character_predicate(char::is_numeric, "NUMERIC")),
                &input,
            ));
        }
        Ok(())
    }

    #[test]
    fn test_parse_token() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("int foo = 4;");
            let (rest, parsed) = run_parser(parser_token("int".to_string()), &input).unwrap();
            assert_eq_position_info!(parsed, 0, 0, 0, 3, 0, 3);
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("int* foo = 4;");
            let (rest1, parsed1) = run_parser(parser_token("int".to_string()), &input).unwrap();
            assert_eq_position_info!(parsed1, 0, 0, 0, 3, 0, 3);
            assert_eq_parse_input!(rest1, 3, 0, 3);

            let (rest2, parsed2) = run_parser(parser_token("*".to_string()), &rest1).unwrap();
            assert_eq_position_info!(parsed2, 3, 0, 3, 4, 0, 4);
            assert_eq_parse_input!(rest2, 4, 0, 4);
        }
        {
            let input = ParserInput::create("char foo = '4';");
            assert_is_error_print_ok!(run_parser(parser_token("character".to_string()), &input));
        }
        Ok(())
    }

    #[test]
    fn test_skip_whitespace() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("  \n\r\n15;");
            let (rest, parsed) =
                run_parser(skip_whitespaces(parser_token("15".to_string())), &input).unwrap();
            assert_eq_position_info!(parsed, 5, 2, 0, 7, 2, 2);
            assert_eq_parse_input!(rest, 7, 2, 2);
        }
        {
            let input = ParserInput::create("double x;");
            let (rest, parsed) =
                run_parser(skip_whitespaces(parser_token("double".to_string())), &input).unwrap();
            assert_eq_position_info!(parsed, 0, 0, 0, 6, 0, 6);
            assert_eq_parse_input!(rest, 6, 0, 6);
        }
        Ok(())
    }

    #[test]
    fn test_and_then() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("int x = 5;");

            let parser = and_then(
                skip_whitespaces(parser_token("int".to_string())),
                skip_whitespaces(map_parser_error(
                    repeat_at_least_1(parser_character_predicate(
                        char::is_alphabetic,
                        "ALPHABETIC",
                    )),
                    |_| ParserErrorInfo::create(ParserErrorKind::Unknown),
                )),
            );

            let (rest, parsed) = run_parser(parser, &input).unwrap();
            assert_eq!(parsed.into_iter().collect::<String>(), "x");
            assert_eq_parse_input!(rest, 5, 0, 5);
        }
        Ok(())
    }

    #[test]
    fn test_and() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("int x = 5;");

            let parser = and(
                skip_whitespaces(map_parser_error(
                    repeat_at_least_1(parser_character_predicate(
                        char::is_alphabetic,
                        "ALPHABETIC",
                    )),
                    |_| ParserErrorInfo::create(ParserErrorKind::Unknown),
                )),
                skip_whitespaces(map_parser_error(
                    repeat_at_least_1(parser_character_predicate(
                        char::is_alphabetic,
                        "ALPHABETIC",
                    )),
                    |_| ParserErrorInfo::create(ParserErrorKind::Unknown),
                )),
            );

            let (rest, parsed) = run_parser(parser, &input).unwrap();
            assert_eq!(parsed.into_iter().collect::<String>(), "int");
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        Ok(())
    }
}
