use crate::{
    analysis::symbols::{Symbol, SymbolError, SymbolTable},
    parsing::{
        ast::{Declaration, Expression, Statement},
        combinators::Either3,
    },
};

pub struct TreeDescent;

impl TreeDescent {
    pub fn declarations_pass1(
        symbol_table: &mut SymbolTable,
        node: Either3<&Statement, &Expression, &Declaration>,
        parent: &str,
    ) -> Result<(), SymbolError> {
        match node {
            Either3::First(statement) => match statement {
                Statement::Declaration(decl) => {
                    Self::declarations_pass1(symbol_table, Either3::Third(decl), parent)
                }
                Statement::Expression(expr) => {
                    Self::declarations_pass1(symbol_table, Either3::Second(expr), parent)
                }
            },
            Either3::Second(expression) => match expression {
                Expression::Builtin(..) | Expression::LiteralInt(..) => Ok(()),
                Expression::BinaryOperation(_, lhs, rhs, _) => {
                    Self::declarations_pass1(symbol_table, Either3::Second(lhs), parent)?;
                    Self::declarations_pass1(symbol_table, Either3::Second(rhs), parent)?;
                    Ok(())
                }
                Expression::UnaryOperation(_, expr, _) => {
                    Self::declarations_pass1(symbol_table, Either3::Second(expr), parent)?;
                    Ok(())
                }
                Expression::Block(pos, statements) => {
                    let block = symbol_table.get_anonymous_block_name(parent)?;
                    symbol_table.add_symbol(&block, Symbol::Block(pos.clone()))?;
                    for statement in statements.iter() {
                        Self::declarations_pass1(symbol_table, Either3::First(statement), &block)?;
                    }
                    Ok(())
                }
            },
            Either3::Third(declaration) => match declaration {
                Declaration::Namespace(pos, namespace, declarations) => {
                    let child = symbol_table.get_child_name(parent, &namespace.name)?;
                    symbol_table.add_symbol(&child, Symbol::Namespace(pos.clone()))?;
                    for decl in declarations.iter() {
                        Self::declarations_pass1(symbol_table, Either3::Third(decl), &child)?;
                    }
                    Ok(())
                }
                Declaration::Function(pos, name, args, rtype, body) => {
                    let function = symbol_table.get_child_name(parent, &name.name)?;
                    symbol_table.add_function(
                        &function,
                        args.clone(),
                        rtype.clone(),
                        pos.clone(),
                    )?;
                    for (argi, argt) in args.iter() {
                        let argname = symbol_table.get_child_name(&function, &argi.name)?;
                        symbol_table.add_variable(
                            &argname,
                            Some(argt.clone()),
                            argi.position().until(argt.position()),
                        )?;
                    }
                    if let Some(body) = body {
                        Self::declarations_pass1(symbol_table, Either3::First(body), &function)?;
                    }
                    Ok(())
                }
                Declaration::Variable(pos, name, vtype, ..) => {
                    let variable = symbol_table.get_child_name(parent, &name.name)?;
                    symbol_table.add_variable(&variable, vtype.clone(), pos.clone())?;
                    Ok(())
                }
            },
        }
    }
}
