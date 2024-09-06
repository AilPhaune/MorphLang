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
                Expression::Builtin | Expression::LiteralInt(..) => Ok(()),
                Expression::BinaryOperation(lhs, rhs, _) => {
                    Self::declarations_pass1(symbol_table, Either3::Second(lhs), parent)?;
                    Self::declarations_pass1(symbol_table, Either3::Second(rhs), parent)?;
                    Ok(())
                }
                Expression::UnaryOperation(expr, _) => {
                    Self::declarations_pass1(symbol_table, Either3::Second(expr), parent)?;
                    Ok(())
                }
                Expression::Block(statements) => {
                    let block = symbol_table.get_anonymous_block_name(parent)?;
                    symbol_table.add_symbol(&block, Symbol::Block)?;
                    for statement in statements.iter() {
                        Self::declarations_pass1(symbol_table, Either3::First(statement), &block)?;
                    }
                    Ok(())
                }
            },
            Either3::Third(declaration) => match declaration {
                Declaration::Namespace(namespace, declarations) => {
                    let child = symbol_table.get_child_name(parent, &namespace.0)?;
                    symbol_table.add_symbol(&child, Symbol::Namespace)?;
                    for decl in declarations.iter() {
                        Self::declarations_pass1(symbol_table, Either3::Third(decl), &child)?;
                    }
                    Ok(())
                }
                Declaration::Function(name, args, rtype, body) => {
                    let function = symbol_table.get_child_name(parent, &name.0)?;
                    symbol_table.add_function(&function, args.clone(), rtype.clone())?;
                    for (argi, argt) in args.iter() {
                        let argname = symbol_table.get_child_name(&function, &argi.0)?;
                        symbol_table.add_variable(&argname, Some(argt.clone()))?;
                    }
                    if let Some(body) = body {
                        Self::declarations_pass1(symbol_table, Either3::First(body), &function)?;
                    }
                    Ok(())
                }
                Declaration::Variable(name, vtype, ..) => {
                    let variable = symbol_table.get_child_name(parent, &name.0)?;
                    symbol_table.add_variable(&variable, vtype.clone())?;
                    Ok(())
                }
            },
        }
    }
}
