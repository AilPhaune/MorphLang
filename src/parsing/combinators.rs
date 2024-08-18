use std::{char, rc::Rc};

use super::error::{ParserErrorInfo, ParserErrorKind};

#[macro_export]
macro_rules! assert_eq_parse_input {
    ($parser_input:expr, $current_index:expr, $current_line:expr, $current_line_index:expr) => {
        assert_eq!($parser_input.current_index, $current_index);
        assert_eq!($parser_input.current_line, $current_line);
        assert_eq!($parser_input.current_line_index, $current_line_index);
    };
}

#[derive(Debug)]
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

    pub fn get_char_relative(&self, idx: isize) -> Option<char> {
        self.code.chars().nth(self.current_index + (idx as usize))
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
}
