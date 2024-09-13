use std::{env, fs, io::Result, rc::Rc};

use morphlang::{
    analysis::symbols::SymbolTable,
    parsing::{
        astparser::{parse_program, ASTParserContext},
        combinators::{expect_consumed_all, map_parser_error, Either3, ParserInput},
        error::{ParserErrorInfo, ParserErrorKind},
        parser::run_parser,
    },
    preprocessor::Preprocessor,
    type_checker::tree_descent::TreeDescent,
};

pub fn main() -> Result<()> {
    let arg_data = parse_args();

    let input_data = fs::read_to_string(arg_data.input_file)?;

    let mut preprocessor = Preprocessor::new();
    let preprocessed_input = preprocessor.preprocess(input_data).unwrap();

    println!("Preprocessed input:\n{}\n", preprocessed_input);

    println!("Preprocessor state:\n{:#?}\n", preprocessor);

    let input = ParserInput::create_from_string(preprocessed_input);

    let context: Rc<ASTParserContext> = Rc::from(preprocessor.context);
    let parser = map_parser_error(expect_consumed_all(parse_program(context)), |e| {
        e.unwrap_or(ParserErrorInfo::create(ParserErrorKind::UnexpectedInput))
    });

    let result = match run_parser(&parser, &input) {
        Err(e) => {
            panic!("Error: {:#?}\n\n{}", e, e.pretty_print(0, 1));
        }
        Ok((_, ast)) => ast,
    };

    println!("Parsed: {:#?}", result);

    let mut symbol_table = SymbolTable::new().unwrap();
    TreeDescent::declarations_pass1(&mut symbol_table, Either3::Third(&result), "").unwrap();

    println!("Symbol table: {:#?}", symbol_table);

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
