use std::{char, rc::Rc};

use crate::traits::has_len::HasLen;

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
pub enum Either<A, B> {
    First(A),
    Second(B),
}

impl<A, B> Either<A, B> {
    pub fn get<R>(self) -> R
    where
        R: From<A> + From<B>,
    {
        match self {
            Either::First(a) => R::from(a),
            Either::Second(b) => R::from(b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Either3<A, B, C> {
    First(A),
    Second(B),
    Third(C),
}

#[derive(Debug, Clone)]
pub struct ParserInput {
    pub code: Rc<String>,
    pub source_code_file_index: usize,
    pub current_index: usize,
    pub current_line: usize,
    pub current_line_index: usize,
}

impl ParserInput {
    pub fn create(code: &str) -> Self {
        Self {
            source_code_file_index: 0,
            code: Rc::new(code.to_string()),
            current_index: 0,
            current_line: 0,
            current_line_index: 0,
        }
    }

    pub fn create_from_string(code: String) -> Self {
        Self {
            source_code_file_index: 1,
            code: Rc::new(code),
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
                        source_code_file_index: self.source_code_file_index,
                        code: self.code.to_owned(),
                        current_index: self.current_index + 1,
                        current_line: self.current_line + 1,
                        current_line_index: 0,
                    }
                } else {
                    Self {
                        source_code_file_index: self.source_code_file_index,
                        code: self.code.to_owned(),
                        current_index: self.current_index + 1,
                        current_line: self.current_line,
                        current_line_index: self.current_line_index + 1,
                    }
                },
            )
        })
    }

    pub fn advance_by(&self, count: usize) -> Option<(Vec<char>, ParserInput)> {
        let mut advanced = Vec::new();
        let mut state = self.clone();
        for _ in 0..count {
            if let Some((c, rest)) = state.advance() {
                state = rest;
                advanced.push(c);
            } else {
                return None;
            }
        }
        Some((advanced, state))
    }

    pub fn get_as_string(&self) -> String {
        self.code
            .chars()
            .skip(self.current_index)
            .collect::<String>()
    }

    pub fn get_before(&self) -> PositionInfo {
        PositionInfo::create(0, 0, 0, self.source_code_file_index)
            .until(&PositionInfo::from_parser_input_position(self))
    }
}

impl HasLen for ParserInput {
    fn len(&self) -> usize {
        let l = self.code.len();
        if self.current_index >= l {
            0
        } else {
            l - self.current_index
        }
    }

    fn is_empty(&self) -> bool {
        self.current_index >= self.code.len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PositionInfo {
    pub source_code_file_index: usize,
    pub start_index: usize,
    pub start_line: usize,
    pub start_line_index: usize,
    pub end_index: usize,
    pub end_line: usize,
    pub end_line_index: usize,
}

impl PositionInfo {
    pub fn create(
        start_index: usize,
        start_line: usize,
        start_line_index: usize,
        source_code_file_index: usize,
    ) -> Self {
        Self {
            source_code_file_index,
            start_index,
            start_line,
            start_line_index,
            end_index: start_index,
            end_line: start_line,
            end_line_index: start_line_index,
        }
    }

    pub fn from_begin_and_end(begin: &ParserInput, end: &ParserInput) -> Self {
        Self::from_parser_input_position(begin).until(&Self::from_parser_input_position(end))
    }

    pub fn from_parser_input_position(p: &ParserInput) -> Self {
        Self {
            source_code_file_index: p.source_code_file_index,
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
            source_code_file_index: self.source_code_file_index,
            start_index: self.start_index,
            start_line: self.start_line,
            start_line_index: self.start_line_index,
            end_index: end.end_index,
            end_line: end.end_line,
            end_line_index: end.end_line_index,
        }
    }

    pub fn len(&self) -> usize {
        if self.is_empty() {
            0
        } else {
            self.end_index - self.start_index
        }
    }

    pub fn is_empty(&self) -> bool {
        self.end_index <= self.start_index
    }

    pub fn get_as_string(&self, parser_input: ParserInput) -> String {
        parser_input
            .code
            .chars()
            .skip(self.start_index)
            .take(self.len())
            .collect::<String>()
    }
}

/// Parses a character based on a predicate. Returns the character if it matches the predicate, or a `ParserErrorInfo` of the kind `ParserErrorKind::ExpectedCharacter`. <br>
/// The `predicate_info` field allows the error to carry more information about what kind of character was expected.
///
/// # Examples
///
/// Example 1: Parsing an alphanumeric character
/// ```
/// use crate::morphlang::parsing::combinators::{parser_character_predicate, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = parser_character_predicate(char::is_alphanumeric, "ALPHANUMERIC");
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail, as the first character 'y' is alphanumeric !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq!(parsed, 'y');
///         assert_eq!(remaining_input.get_as_string(), "our input");
///     },
/// }
/// ```
pub fn parser_character_predicate(
    predicate: fn(char) -> bool,
    predicate_info: &str,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, char), ParserErrorInfo> {
    let pred = predicate_info.to_string();
    move |input| match input.advance() {
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

/// Parses one unique character, fails if it encounters a different character.
pub fn parser_character(
    c: char,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, char), ParserErrorInfo> {
    move |input| match input.advance() {
        None => Err(ParserErrorInfo::create(ParserErrorKind::EndOfFile)),
        Some((ch, rest)) => {
            if ch == c {
                Ok((rest, c))
            } else {
                Err(ParserErrorInfo::create(
                    ParserErrorKind::ExpectedCharacter {
                        predicate_info: c.to_string(),
                    },
                ))
            }
        }
    }
}

/// Used to wrap another parser so it allows the input to start with whitespaces.<br>
/// It returns the output of the wrapped parser after having skipped the whitespaces at the start of the input.
///
/// # Examples
///
/// Example 1: Parsing a token, skipping the whitespaces before that token
/// ```
/// use crate::morphlang::parsing::combinators::{skip_whitespaces, parser_token, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create(" \r  \n\n\t  \t\r\nyour input");
/// let parser = skip_whitespaces(parser_token("your".to_string()));
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         // here, `parsed` is a `PositionInfo`
///         assert_eq!(remaining_input.get_as_string(), " input");
///     },
/// }
/// ```
pub fn skip_whitespaces<ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&ParserInput) -> Result<(ParserInput, OutputType), ErrorType>
where
    P: Parser<ParserInput, ErrorType, OutputType>,
{
    move |input| {
        if input.is_empty() {
            return parser.run(input);
        }
        let (remaining_input, _) = repeat_at_least_0(parser_character_predicate(
            char::is_whitespace,
            "WHITESPACE",
        ))
        .run(input)
        .unwrap();
        parser.run(&remaining_input)
    }
}

/// Returns a parser that repeatedly parses using the given parser, returning a `Vec` containing all the successful parses, and stops when the given parser fails.<br>
/// The returned parser never fails.<br>
///
/// # Examples
///
/// Example 1: Parsing at least 0 alphabetic characters
/// ```
/// use crate::morphlang::parsing::combinators::{repeat_at_least_0, parser_character_predicate, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = repeat_at_least_0(parser_character_predicate(char::is_alphabetic, "ALPHABETIC"));
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq!(parsed.iter().collect::<String>(), "your");
///     },
/// }
/// ```
///
/// Example 2: Parsing at least 0 numeric characters
/// ```
/// use crate::morphlang::parsing::combinators::{repeat_at_least_0, parser_character_predicate, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = repeat_at_least_0(parser_character_predicate(char::is_numeric, "NUMERIC"));
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert!(parsed.is_empty()); // Parses nothing
///     },
/// }
/// ```
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

/// Returns a parser that repeatedly parses using the given parser, returning a `Vec` containing all the successful parses, and stops when the given parser fails.<br>
/// The returned parser fails if and only if the underlying parser fails on its first call, i.e. when the output vector is empty. In that case, it returns a unit error type.
///
/// # Examples
///
/// Example 1: Parsing a sequence of alphabetic characters
/// ```
/// use crate::morphlang::parsing::combinators::{repeat_at_least_1, parser_character_predicate, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = repeat_at_least_1(parser_character_predicate(char::is_alphabetic, "ALPHABETIC"));
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq!(parsed.iter().collect::<String>(), "your");
///     },
/// }
/// ```
///
/// Example 2: Parsing a sequence of numeric characters
/// ```
/// use crate::morphlang::parsing::combinators::{repeat_at_least_1, parser_character_predicate, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = repeat_at_least_1(parser_character_predicate(char::is_numeric, "NUMERIC"));
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?} which was intended !", e);
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
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

/// Parses an exact sequence of characters
///
/// # Examples
///
/// Example 1: Fail case
/// ```
/// use crate::morphlang::parsing::combinators::{parser_token, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("yourinput");
/// let parser = parser_token("your ".to_string());
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?} which was intended !", e);
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
///
/// Example 2: Success case
/// ```
/// use crate::morphlang::parsing::combinators::{parser_token, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
/// use crate::morphlang::assert_eq_position_info;
///
/// let input = ParserInput::create("yourinput");
/// let parser = parser_token("your".to_string());
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq_position_info!(parsed, 0, 0, 0, 4, 0, 4);
///         assert_eq!(remaining_input.get_as_string(), "input");
///     },
/// }
/// ```
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

/// Returns the same parser as the one given, but allows to map its potential error output to another type, using the mapper_function
///
/// # Examples
///
/// Example 1:
/// ```
/// use crate::morphlang::parsing::combinators::{map_parser_error, parser_character_predicate, repeat_at_least_1, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = repeat_at_least_1(parser_character_predicate(char::is_numeric, "NUMERIC")); // This parser return a unit error type when it fails
/// let mapped_parser = map_parser_error(parser, |_| "Could not find a numeric char".to_string());
///
/// match mapped_parser.run(&input) {
///     Err(e) => assert_eq!(e, "Could not find a numeric char"),
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
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

/// Returns the same parser as the one given, but allows to map its potential parse output to another type, using the mapper_function
///
/// # Examples
///
/// Example 1:
/// ```
/// use crate::morphlang::parsing::combinators::{map_parser_output, parser_character_predicate, repeat_at_least_1, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = repeat_at_least_1(parser_character_predicate(char::is_alphabetic, "ALPHABETIC")); // This parser return a Vec<char>
/// let mapped_parser = map_parser_output(parser, |v, _begin, _end| v.iter().collect::<String>());
///
/// match mapped_parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq!(parsed, "your");
///         assert_eq!(remaining_input.get_as_string(), " input");
///     },
/// }
/// ```
pub fn map_parser_output<InputType: Clone, OutputType1, OutputType2, ErrorType, P, F>(
    parser: P,
    mapper_function: F,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType2), ErrorType>
where
    P: Parser<InputType, ErrorType, OutputType1>,
    F: Fn(OutputType1, &InputType, &InputType) -> OutputType2,
{
    move |input| {
        parser.run(input).map(|(remaining_input, parsed)| {
            let output = mapper_function(parsed, input, &remaining_input);
            (remaining_input, output)
        })
    }
}

/// Used to map both the output and the error types of the given parser. Works like chaining `map_parser_output` and `map_parser_error`.
pub fn map_parser<InputType: Clone, OutputType1, OutputType2, ErrorType1, ErrorType2, P, F1, F2>(
    parser: P,
    output_mapper: F1,
    error_mapper: F2,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType2), ErrorType2>
where
    P: Parser<InputType, ErrorType1, OutputType1>,
    F1: Fn(OutputType1, &InputType, &InputType) -> OutputType2,
    F2: Fn(ErrorType1) -> ErrorType2,
{
    move |input| {
        parser
            .run(input)
            .map(|(remaining_input, parsed)| {
                let output = output_mapper(parsed, input, &remaining_input);
                (remaining_input, output)
            })
            .map_err(&error_mapper)
    }
}

/// Matches parser1 then parser2, and returns only the result of parser 2
///
/// # Examples
///
/// Example 1: Success case
/// ```
/// use crate::morphlang::parsing::combinators::{and_then2, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
/// use crate::morphlang::assert_eq_position_info;
/// use crate::morphlang::traits::has_len::HasLen;
///
/// let input = ParserInput::create("your input");
/// let parser = and_then2(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq_position_info!(parsed, 5, 0, 5, 10, 0, 10);
///         assert!(remaining_input.is_empty());
///     },
/// }
/// ```
///
/// Example 2: Fail case
/// ```
/// use crate::morphlang::parsing::combinators::{and_then2, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your different input");
/// let parser = and_then2(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?}, which was intended !", e)    
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
pub fn and_then2<InputType: Clone, ErrorType, OutputType1, OutputType2, P1, P2>(
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

/// Matches parser1 then parser2, and returns only the result of parser 2
///
/// # Examples
///
/// Example 1: Success case
/// ```
/// use crate::morphlang::parsing::combinators::{and_then_both, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
/// use crate::morphlang::assert_eq_position_info;
/// use crate::morphlang::traits::has_len::HasLen;
///
/// let input = ParserInput::create("your input");
/// let parser = and_then_both(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, (parsed1, parsed2))) => {
///         assert_eq_position_info!(parsed1, 0, 0, 0, 4, 0, 4);
///         assert_eq_position_info!(parsed2, 5, 0, 5, 10, 0, 10);
///         assert!(remaining_input.is_empty());
///     },
/// }
/// ```
///
/// Example 2: Fail case
/// ```
/// use crate::morphlang::parsing::combinators::{and_then_both, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your different input");
/// let parser = and_then_both(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?}, which was intended !", e)    
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
pub fn and_then_both<InputType: Clone, ErrorType1, ErrorType2, OutputType1, OutputType2, P1, P2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&InputType) -> Result<(InputType, (OutputType1, OutputType2)), Either<ErrorType1, ErrorType2>>
where
    P1: Parser<InputType, ErrorType1, OutputType1>,
    P2: Parser<InputType, ErrorType2, OutputType2>,
{
    move |input| {
        let (input, res1) = parser1.run(input).map_err(Either::First)?;
        let (input, res2) = parser2.run(&input).map_err(Either::Second)?;
        Ok((input, (res1, res2)))
    }
}

/// Matches parser1 then parser2, and returns only the result of parser 1
///
/// # Examples
///
/// Example 1: Success case
/// ```
/// use crate::morphlang::parsing::combinators::{and_then1, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
/// use crate::morphlang::assert_eq_position_info;
/// use crate::morphlang::traits::has_len::HasLen;
///
/// let input = ParserInput::create("your input");
/// let parser = and_then1(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq_position_info!(parsed, 0, 0, 0, 4, 0, 4);
///         assert!(remaining_input.is_empty());
///     },
/// }
/// ```
///
/// Example 2: Fail case
/// ```
/// use crate::morphlang::parsing::combinators::{and_then1, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your different input");
/// let parser = and_then1(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?}, which was intended !", e)    
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
pub fn and_then1<InputType: Clone, ErrorType, OutputType1, OutputType2, P1, P2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType1), ErrorType>
where
    P1: Parser<InputType, ErrorType, OutputType1>,
    P2: Parser<InputType, ErrorType, OutputType2>,
{
    move |input| {
        let (remaining_input, parsed) = parser1.run(input)?;
        let (remaining_input, _) = parser2.run(&remaining_input)?;
        Ok((remaining_input, parsed))
    }
}

/// Matches parser1 then parser2, and returns only the result of parser 1 and ignores the input consumed by parser 2
///
/// # Examples
///
/// Example 1: Success case
/// ```
/// use crate::morphlang::parsing::combinators::{and, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
/// use crate::morphlang::assert_eq_position_info;
///
/// let input = ParserInput::create("your input");
/// let parser = and(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, parsed)) => {
///         assert_eq_position_info!(parsed, 0, 0, 0, 4, 0, 4);
///         assert_eq!(remaining_input.get_as_string(), " input");
///     },
/// }
/// ```
///
/// Example 2: Fail case
/// ```
/// use crate::morphlang::parsing::combinators::{and, parser_token, skip_whitespaces, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your different input");
/// let parser = and(
///     parser_token("your".to_string()),
///     skip_whitespaces(parser_token("input".to_string()))
/// );
///
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?}, which was intended !", e)
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
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

/// Matches parser1 OR parser2, and returns the result of the first one that succeeds, or both errors if both fail
pub fn or<InputType: Clone, ErrorType1, ErrorType2, OutputType1, OutputType2, P1, P2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&InputType) -> Result<(InputType, Either<OutputType1, OutputType2>), (ErrorType1, ErrorType2)>
where
    P1: Parser<InputType, ErrorType1, OutputType1>,
    P2: Parser<InputType, ErrorType2, OutputType2>,
{
    move |input| match parser1.run(input) {
        Err(err1) => match parser2.run(input) {
            Err(err2) => Err((err1, err2)),
            Ok((rest, parsed)) => Ok((rest, Either::Second(parsed))),
        },
        Ok((rest, parsed)) => Ok((rest, Either::First(parsed))),
    }
}

/// Returns a unit type error if the parser succeeds, or returns a unit output type with the parser input unchanged if the parser fails
///
/// # Examples
///
/// Example 1: Success case
/// ```
/// use crate::morphlang::parsing::combinators::{not, parser_token, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = not(parser_token("youri".to_string()));
///
/// match parser.run(&input) {
///     Err(e) => panic!("Parser wasn't supposed to fail !"),
///     Ok((remaining_input, _)) => {
///         assert_eq!(remaining_input.get_as_string(), "your input");
///     },
/// }
/// ```
///
/// Example 2: Fail case
/// ```
/// use crate::morphlang::parsing::combinators::{not, parser_token, ParserInput};
/// use crate::morphlang::parsing::parser::Parser;
///
/// let input = ParserInput::create("your input");
/// let parser = not(parser_token("your".to_string()));
///
/// match parser.run(&input) {
///     Err(e) => {
///         println!("Parser failed with error {:?}, which was intended !", e)
///     },
///     Ok(v) => panic!("Parser was supposed to fail, got Ok({:?})", v),
/// }
/// ```
pub fn not<InputType: Clone, ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&InputType) -> Result<(InputType, ()), ()>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| match parser.run(input) {
        Ok(_) => Err(()),
        Err(_) => Ok((input.clone(), ())),
    }
}

/// Similarly to `or`, takes a vector of parsers, runs them in order and return the first one that succeeds, or a vector of all the errors if they all fail.
pub fn any_of<InputType: Clone, ErrorType, OutputType, P>(
    parsers: Vec<P>,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType), Vec<ErrorType>>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| {
        let mut errs: Vec<ErrorType> = Vec::new();
        for parser in parsers.iter() {
            match parser.run(input) {
                Err(e) => errs.push(e),
                Ok(v) => return Ok(v),
            }
        }
        Err(errs)
    }
}

/// Same as `any_of`, but with a vec of `Box`es
pub fn any_of_boxes<InputType: Clone, ErrorType, OutputType>(
    parsers: Vec<Box<dyn Parser<InputType, ErrorType, OutputType>>>,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType), Vec<ErrorType>> {
    move |input| {
        let mut errs: Vec<ErrorType> = Vec::new();
        for parser in parsers.iter() {
            match parser.run(input) {
                Err(e) => errs.push(e),
                Ok(v) => return Ok(v),
            }
        }
        Err(errs)
    }
}

pub fn parser_nothing<InputType: Clone, ErrorType>(
) -> impl Fn(&InputType) -> Result<(InputType, ()), ErrorType> {
    move |input| Ok((input.clone(), ()))
}

pub fn expect_consumed_all<InputType: Clone + HasLen, ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType), Option<ErrorType>>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| match parser.run(input) {
        Err(e) => Err(Some(e)),
        Ok((remaining_input, parsed)) => {
            if remaining_input.len() > 0 {
                Err(None)
            } else {
                Ok((remaining_input, parsed))
            }
        }
    }
}

pub fn parse_then_restart<InputType: Clone, ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&InputType) -> Result<(InputType, OutputType), ErrorType>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| parser.run(input).map(|(_, v)| (input.clone(), v))
}

pub fn delimited<InputType: Clone, ErrorType1, ErrorType2, OutputType1, OutputType2, P1, P2>(
    delimiter: P1,
    element: P2,
    at_least_one: bool,
) -> impl Fn(&InputType) -> Result<(InputType, Vec<OutputType2>), ErrorType2>
where
    P1: Parser<InputType, ErrorType1, OutputType1>,
    P2: Parser<InputType, ErrorType2, OutputType2>,
{
    move |input| {
        let mut values = Vec::new();
        let (mut input, value) = match element.run(input) {
            Ok(v) => v,
            Err(e) => {
                if at_least_one {
                    Err(e)?
                } else {
                    return Ok((input.clone(), values));
                }
            }
        };
        values.push(value);

        loop {
            if let Ok((input1, _)) = delimiter.run(&input) {
                let (input2, value) = element.run(&input1)?;
                values.push(value);
                input = input2;
            } else {
                return Ok((input, values));
            }
        }
    }
}

pub fn optional<InputType: Clone, ErrorType, OutputType, P>(
    parser: P,
) -> impl Fn(&InputType) -> Result<(InputType, Option<OutputType>), ()>
where
    P: Parser<InputType, ErrorType, OutputType>,
{
    move |input| match parser.run(input) {
        Ok((remaining_input, parsed)) => Ok((remaining_input, Some(parsed))),
        Err(_) => Ok((input.clone(), None)),
    }
}

pub fn expect_input(input: &ParserInput) -> Result<(), ParserErrorInfo> {
    if input.is_empty() {
        Err(ParserErrorInfo::create(ParserErrorKind::EndOfFile).with_level(1))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_is_error_print_ok, parsing::parser::run_parser};

    use super::*;

    #[test]
    fn test_character_predicate() -> Result<(), ParserErrorInfo> {
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
    fn test_character() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("abc");
            let (rest, parsed_char) = run_parser(parser_character('a'), &input)?;
            assert_eq!(parsed_char, 'a');
            assert_eq_parse_input!(rest, 1, 0, 1);
        }
        {
            let input = ParserInput::create("abc");
            assert_is_error_print_ok!(run_parser(parser_character('b'), &input));
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
    fn test_token() -> Result<(), ParserErrorInfo> {
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
    fn test_skip_whitespaces() -> Result<(), ParserErrorInfo> {
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
    fn test_and_then2() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("int x = 5;");

            let parser = and_then2(
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
    fn test_and_then1() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("int x = 5;");

            let parser = and_then1(
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
            assert_eq_position_info!(parsed, 0, 0, 0, 3, 0, 3);
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

    fn test_template_or0<P, ErrorType: std::fmt::Debug>(parser: P) -> Result<(), ErrorType>
    where
        P: Parser<ParserInput, ErrorType, PositionInfo>,
    {
        {
            let input = ParserInput::create("int x = 5;");
            let (rest, parsed) = parser.run(&input)?;
            assert_eq_position_info!(parsed, 0, 0, 0, 3, 0, 3);
            assert_eq_parse_input!(rest, 3, 0, 3);
        }
        {
            let input = ParserInput::create("void foo() {}");
            let (rest, parsed) = parser.run(&input)?;
            assert_eq_position_info!(parsed, 0, 0, 0, 4, 0, 4);
            assert_eq_parse_input!(rest, 4, 0, 4);
        }
        {
            let input = ParserInput::create("float x = 5.2f;");
            let (rest, parsed) = parser.run(&input)?;
            assert_eq_position_info!(parsed, 0, 0, 0, 5, 0, 5);
            assert_eq_parse_input!(rest, 5, 0, 5);
        }
        {
            let input = ParserInput::create("double x = 5.2;");
            let (rest, parsed) = parser.run(&input)?;
            assert_eq_position_info!(parsed, 0, 0, 0, 6, 0, 6);
            assert_eq_parse_input!(rest, 6, 0, 6);
        }
        {
            let input = ParserInput::create("long x = 7;");
            assert_is_error_print_ok!(parser.run(&input));
        }
        Ok(())
    }

    #[test]
    fn test_or() -> Result<(), ParserErrorInfo> {
        let parser = map_parser_output(
            or(
                or(
                    parser_token("int".to_string()),
                    parser_token("float".to_string()),
                ),
                or(
                    parser_token("void".to_string()),
                    parser_token("double".to_string()),
                ),
            ),
            |o, _, _| o.get::<Either<PositionInfo, PositionInfo>>().get(),
        );
        test_template_or0(parser).map_err(|_| ParserErrorInfo::create(ParserErrorKind::Unknown))?;
        Ok(())
    }

    #[test]
    fn test_not() -> Result<(), ParserErrorInfo> {
        let parser_letters = repeat_at_least_1(parser_character_predicate(
            char::is_alphabetic,
            "ALPHABETIC",
        ));
        let parser_not_digit = not(parser_character_predicate(char::is_numeric, "NUMERIC"));
        let parser = and(
            map_parser_error(parser_letters, |_| {
                ParserErrorInfo::create(ParserErrorKind::Unknown)
                    .with_info("Parser of a string of letters failed because it didn't find a letter to start the string".to_string())
            }),
            map_parser_error(parser_not_digit, |_| {
                ParserErrorInfo::create(ParserErrorKind::Unknown)
                    .with_info("Parser of a non-digit failed because it found a digit".to_string())
            }),
        ); // a parser for a string of letters that does not end in a digit, only getting the letter string part
        {
            let input = ParserInput::create("asdfasdf;");
            let (rest, parsed) = run_parser(&parser, &input).unwrap();
            assert_eq!(parsed.iter().collect::<String>(), "asdfasdf");
            assert_eq_parse_input!(rest, 8, 0, 8);
        }
        {
            let input = ParserInput::create("qqqO;");
            let (rest, parsed) = run_parser(&parser, &input).unwrap();
            assert_eq!(parsed.iter().collect::<String>(), "qqqO");
            assert_eq_parse_input!(rest, 4, 0, 4);
        }
        {
            let input = ParserInput::create("print(18)");
            let (rest, parsed) = run_parser(&parser, &input).unwrap();
            assert_eq!(parsed.iter().collect::<String>(), "print");
            assert_eq_parse_input!(rest, 5, 0, 5);
        }
        {
            let input = ParserInput::create("repeat0();");
            match run_parser(&parser, &input) {
                Ok(v) => panic!("Expected parser to fail, got Ok({:?}).", v),
                Err(e) => {
                    if let Some(err) = e.get_info() {
                        if !err.starts_with("Parser of a non-digit") {
                            panic!("Parser failed for the wrong reason: {:?}", e);
                        }
                    } else {
                        panic!("Parser failed for the wrong reason: {:?}", e);
                    }
                }
            }
        }
        Ok(())
    }

    #[test]
    fn test_map_parser_error() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("abcd");
            let parser = repeat_at_least_1(parser_character_predicate(char::is_numeric, "NUMERIC"));
            let mapped_parser = map_parser_error(parser, |_| "mapped error");
            match run_parser(&mapped_parser, &input) {
                Ok(v) => panic!("Expected parser to fail, got Ok({:?}).", v),
                Err(e) => assert_eq!(e, "mapped error"),
            }
        }
        Ok(())
    }

    #[test]
    fn test_map_parser_output() -> Result<(), ParserErrorInfo> {
        {
            let input = ParserInput::create("abcd");
            let parser = repeat_at_least_1(parser_character_predicate(
                char::is_alphabetic,
                "ALPHABETIC",
            ));
            let mapped_parser = map_parser_output(parser, |v, _, _| {
                v.iter().collect::<String>().to_ascii_uppercase()
            });
            let (remaining_input, parsed) = run_parser(&mapped_parser, &input).unwrap();
            assert_eq!(parsed, "ABCD");
            assert!(remaining_input.is_empty());
        }
        Ok(())
    }

    #[test]
    fn test_map_parser() -> Result<(), ParserErrorInfo> {
        let parser = repeat_at_least_1(parser_character_predicate(
            char::is_alphabetic,
            "ALPHABETIC",
        ));
        let mapped_parser = map_parser(
            parser,
            |v, _, _| v.iter().collect::<String>().to_ascii_uppercase(),
            |_| "mapped error",
        );
        {
            let input = ParserInput::create("abcd");
            let (remaining_input, parsed) = run_parser(&mapped_parser, &input).unwrap();
            assert_eq!(parsed, "ABCD");
            assert!(remaining_input.is_empty());
        }
        {
            let input = ParserInput::create("1234");
            match run_parser(&mapped_parser, &input) {
                Ok(v) => panic!("Expected parser to fail, got Ok({:?}).", v),
                Err(e) => assert_eq!(e, "mapped error"),
            }
        }
        Ok(())
    }

    #[test]
    fn test_any_of() -> Result<(), ParserErrorInfo> {
        let parser = any_of(vec![
            parser_token("int".to_string()),
            parser_token("float".to_string()),
            parser_token("void".to_string()),
            parser_token("double".to_string()),
        ]);
        test_template_or0(parser)
            .map_err(|e| ParserErrorInfo::create(ParserErrorKind::SubErrorList(e)))?;
        Ok(())
    }

    #[test]
    fn test_any_of_boxes() -> Result<(), ParserErrorInfo> {
        let parser = any_of_boxes(vec![
            Box::from(parser_token("int".to_string())),
            Box::from(parser_token("float".to_string())),
            Box::from(parser_token("void".to_string())),
            Box::from(parser_token("double".to_string())),
        ]);
        test_template_or0(parser)
            .map_err(|e| ParserErrorInfo::create(ParserErrorKind::SubErrorList(e)))?;
        Ok(())
    }
}
