use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    execute,
    style::{Color, ResetColor, SetForegroundColor},
    terminal::{self, ClearType},
    Result,
};
use morphlang::{
    default_context,
    parsing::{
        ast::{BinaryOperatorPrecedence, UnaryOperatorPrecedence},
        astparser::{
            parse_bin_literal_int, parse_decimal_literal_int, parse_expression,
            parse_hex_literal_int, parse_identifier, parse_literal_int, parse_oct_literal_int,
            parse_proposition, parse_type, parse_type_base, ASTParserContext,
        },
        combinators::{delimited, parser_character, ParserInput},
        error::ParserErrorInfo,
        parser::Parser,
    },
};
use std::{
    io::{stdout, Write},
    rc::Rc,
};

fn main() -> Result<()> {
    let mut stdout = stdout();

    terminal::enable_raw_mode()?;
    execute!(
        stdout,
        terminal::Clear(ClearType::All),
        cursor::MoveTo(0, 0)
    )?;

    let mut input = String::new();
    let mut cursor_pos: u16 = 0;
    let mut len_counter: usize = 0;

    let context = default_context!();

    update_display(&input, &mut cursor_pos, &context, &mut len_counter)?;

    loop {
        if event::poll(std::time::Duration::from_millis(1))? {
            if let Event::Key(key_event) = event::read()? {
                if key_event.kind == KeyEventKind::Release {
                    continue;
                }
                match key_event.code {
                    KeyCode::Char(c) => {
                        if (c == 'C' || c == 'c')
                            && key_event.modifiers.contains(KeyModifiers::CONTROL)
                        {
                            execute!(
                                stdout,
                                cursor::MoveTo(0, 0),
                                terminal::Clear(ClearType::All),
                            )?;
                            break;
                        }
                        if !c.is_ascii() {
                            continue;
                        }
                        input.insert(cursor_pos as usize, c);
                        cursor_pos += 1;
                        update_display(&input, &mut cursor_pos, &context, &mut len_counter)?;
                    }
                    KeyCode::Left => {
                        let mut last_char = '\0';
                        let mut count: u16 = 0;
                        while cursor_pos > 0
                            && is_same_type(
                                last_char,
                                input.chars().nth(cursor_pos as usize - 1).unwrap(),
                            )
                        {
                            last_char = input.chars().nth(cursor_pos as usize - 1).unwrap();
                            cursor_pos -= 1;
                            count += 1;

                            if !key_event.modifiers.contains(KeyModifiers::CONTROL) {
                                break;
                            }
                        }
                        if count > 0 {
                            execute!(stdout, cursor::MoveLeft(count))?;
                        }
                    }
                    KeyCode::Right => {
                        let mut last_char = '\0';
                        let mut count: u16 = 0;
                        while (cursor_pos as usize) < input.len()
                            && is_same_type(
                                last_char,
                                input.chars().nth(cursor_pos as usize).unwrap(),
                            )
                        {
                            last_char = input.chars().nth(cursor_pos as usize).unwrap();
                            cursor_pos += 1;
                            count += 1;

                            if !key_event.modifiers.contains(KeyModifiers::CONTROL) {
                                break;
                            }
                        }
                        if count > 0 {
                            execute!(stdout, cursor::MoveRight(count))?;
                        }
                    }
                    KeyCode::Backspace => {
                        let mut last_char = '\0';
                        while cursor_pos > 0
                            && is_same_type(
                                last_char,
                                input.chars().nth(cursor_pos as usize - 1).unwrap(),
                            )
                        {
                            last_char = if cursor_pos as usize >= input.len() {
                                input.pop().unwrap()
                            } else {
                                input.remove(cursor_pos as usize - 1)
                            };

                            cursor_pos -= 1;
                            if !key_event.modifiers.contains(KeyModifiers::CONTROL) {
                                break;
                            }
                        }
                        update_display(&input, &mut cursor_pos, &context, &mut len_counter)?;
                    }
                    KeyCode::Delete => {
                        let mut last_char = '\0';
                        while (cursor_pos as usize) < input.len()
                            && is_same_type(
                                last_char,
                                input.chars().nth(cursor_pos as usize).unwrap(),
                            )
                        {
                            last_char = input.remove(cursor_pos as usize);

                            if !key_event.modifiers.contains(KeyModifiers::CONTROL) {
                                break;
                            }
                        }
                        update_display(&input, &mut cursor_pos, &context, &mut len_counter)?;
                    }
                    KeyCode::Enter => {
                        print_ast(&input, &mut cursor_pos, &context, &mut len_counter)?;
                    }
                    _ => {}
                }
            }
        }
    }

    terminal::disable_raw_mode()?;
    Ok(())
}

fn is_same_type(c1: char, c2: char) -> bool {
    if c1 == '\0' {
        true
    } else if c1.is_ascii_digit() {
        c2.is_ascii_digit()
    } else if c1.is_ascii_alphabetic() {
        c2.is_ascii_alphabetic()
    } else if c1.is_ascii_whitespace() {
        c2.is_ascii_whitespace()
    } else {
        false
    }
}

fn print_ast(
    input: &str,
    cursor_pos: &mut u16,
    context: &Rc<ASTParserContext>,
    len_counter: &mut usize,
) -> Result<()> {
    let mut stdout = stdout();

    execute!(
        stdout,
        terminal::Clear(ClearType::All),
        cursor::MoveToNextLine(1)
    )?;

    let pinput = ParserInput::create(input);
    match parse_expression(context.clone()).run(&pinput) {
        Err(e) => {
            writeln!(stdout, "\nError while parsing:")?;
            execute!(stdout, SetForegroundColor(Color::Red))?;
            write!(stdout, "{:?}", e)
        }
        Ok((_, ast)) => {
            writeln!(stdout, "\nAST:")?;
            execute!(stdout, SetForegroundColor(Color::Green))?;
            write!(stdout, "{:?}", ast)
        }
    }?;

    execute!(stdout, ResetColor)?;

    update_display(input, cursor_pos, context, len_counter)?;

    stdout.flush()?;
    Ok(())
}

fn update_display(
    input: &str,
    cursor_pos: &mut u16,
    context: &Rc<ASTParserContext>,
    len_counter: &mut usize,
) -> Result<()> {
    let mut stdout = stdout();

    execute!(stdout, cursor::MoveTo(0, 0))?;

    if *cursor_pos as usize > input.len() {
        *cursor_pos = input.len() as u16;
    }

    // ADD COMBINATORS HERE /!\
    write_combinator_output(parse_decimal_literal_int, input, "DEC int: ", len_counter)?;
    write_combinator_output(parse_hex_literal_int, input, "HEX int: ", len_counter)?;
    write_combinator_output(parse_oct_literal_int, input, "OCT int: ", len_counter)?;
    write_combinator_output(parse_bin_literal_int, input, "BIN int: ", len_counter)?;
    write_combinator_output(parse_literal_int, input, "int lit: ", len_counter)?;
    write_combinator_output(parse_identifier, input, "identifier: ", len_counter)?;
    write_combinator_output(
        delimited(parser_character(','), parse_expression(context.clone())),
        input,
        "',' delimited expr: ",
        len_counter,
    )?;
    write_combinator_output(parse_type_base, input, "type base: ", len_counter)?;
    write_combinator_output(parse_type(context.clone()), input, "type: ", len_counter)?;
    write_combinator_output(
        parse_proposition(context.clone()),
        input,
        "proposition: ",
        len_counter,
    )?;
    write_combinator_output(
        parse_expression(context.clone()),
        input,
        "expression: ",
        len_counter,
    )?;

    for _ in 0..(*len_counter - 1) {
        write!(stdout, ">")?;
    }
    write!(stdout, " {}", input)?;
    execute!(
        stdout,
        terminal::Clear(ClearType::UntilNewLine),
        cursor::MoveToColumn(*len_counter as u16 + *cursor_pos)
    )?;

    stdout.flush()?;
    Ok(())
}

fn write_combinator_output<O, P>(
    combinator: P,
    input: &str,
    prefix: &str,
    len_counter: &mut usize,
) -> Result<()>
where
    P: Parser<ParserInput, ParserErrorInfo, O>,
{
    let mut stdout = stdout();

    if *len_counter < prefix.len() {
        *len_counter = prefix.len()
    };

    let input = ParserInput::create(input);
    write!(stdout, "{}", prefix)?;
    if *len_counter > prefix.len() {
        let diff = *len_counter - prefix.len();
        for _ in 0..diff {
            write!(stdout, " ")?;
        }
    }

    match combinator.run(&input) {
        Err(_) => {
            execute!(stdout, SetForegroundColor(Color::Red))?;
            write!(stdout, "{}", input.get_as_string())?;
        }
        Ok((remaining_input, _)) => {
            execute!(stdout, SetForegroundColor(Color::Green))?;
            write!(
                stdout,
                "{}",
                remaining_input.get_before().get_as_string(input)
            )?;
            execute!(stdout, SetForegroundColor(Color::Red))?;
            write!(stdout, "{}", remaining_input.get_as_string())?;
        }
    }

    execute!(
        stdout,
        ResetColor,
        terminal::Clear(ClearType::UntilNewLine),
        cursor::MoveToNextLine(1)
    )?;
    Ok(())
}
