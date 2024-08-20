use std::{env, fs, io::Result, rc::Rc};

use morphlang::{
    default_context,
    parsing::{
        ast::{BinaryOperatorPrecedence, UnaryOperatorPrecedence},
        astparser::{parse_expression, ASTParserContext},
        combinators::{expect_consumed_all, map_parser_error, ParserInput},
        error::{ParserErrorInfo, ParserErrorKind},
        parser::run_parser,
    },
};

pub fn main() -> Result<()> {
    let arg_data = parse_args();

    let input_data = fs::read_to_string(arg_data.input_file)?;
    let input = ParserInput::create_from_string(input_data);

    let context: Rc<ASTParserContext> = default_context!();
    let parser = map_parser_error(expect_consumed_all(parse_expression(context)), |e| {
        e.unwrap_or(ParserErrorInfo::create(ParserErrorKind::UnexpectedInput))
    });

    let result = match run_parser(&parser, &input) {
        Err(e) => {
            panic!("Error: {:?}", e);
        }
        Ok((_, ast)) => ast,
    };

    println!("{:?}", result);

    Ok(())
}

pub struct ArgData {
    pub program_name: String,
    pub input_file: String,
}

pub fn parse_args() -> ArgData {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }

    ArgData {
        program_name: args[0].clone(),
        input_file: args[1].clone(),
    }
}
