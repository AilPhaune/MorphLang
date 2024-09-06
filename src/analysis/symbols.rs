use std::collections::HashMap;

use crate::{parsing::ast::Identifier, type_checker::types::Type};

#[derive(Clone, Debug)]
pub enum SymbolError {
    SymbolAlreadyExists(String),
    SymbolInexistant(String),
    InvalidChild,
}

#[derive(Clone, Copy, Debug)]
pub enum DeclaredType {
    /// An abstract type is a virtual type that can't be constructed at runtime and has no specific representation and whose implementation is not specified
    Abstract,
    /// A runtime native type
    Native,
}

#[derive(Clone, Debug)]
pub enum Symbol {
    /// The declaration of a type
    TypeDeclaration(DeclaredType),

    /// The declaration of a function
    FunctionDeclaration {
        declared_signature: Vec<Option<Type>>,
        resolved_signature: Option<Vec<Type>>,
    },

    /// The declaration of a variable
    VariableDeclaration {
        declared_type: Option<Type>,
        resolved_type: Option<Type>,
    },

    /// The declaration of a namespace
    Namespace,

    /// The declaration of an anonymous block
    Block,
}

impl Symbol {
    pub fn is_same(&self, other: &Self) -> bool {
        match self {
            Self::Namespace => matches!(other, Self::Namespace),
            Self::TypeDeclaration(_) => matches!(other, Self::TypeDeclaration(_)),
            Self::FunctionDeclaration { .. } | Self::VariableDeclaration { .. } => matches!(
                other,
                Self::FunctionDeclaration { .. } | Self::VariableDeclaration { .. }
            ),
            Self::Block => matches!(other, Self::Block),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    binop_count: usize,
    unop_count: usize,
    block_count: usize,
}

impl SymbolTable {
    pub fn new() -> Result<Self, SymbolError> {
        let mut def = Self::default();
        def.add_symbol("", Symbol::Namespace)?;
        def.add_symbol("@binops", Symbol::Namespace)?;
        def.add_symbol("@unops", Symbol::Namespace)?;
        Ok(def)
    }

    pub fn get_child_name(&self, path: &str, child: &str) -> Result<String, SymbolError> {
        match self.find_symbol(path)? {
            Symbol::TypeDeclaration(_) | Symbol::Namespace | Symbol::Block => {
                Ok(format!("{}::{}", path, child))
            }
            Symbol::FunctionDeclaration { .. } => Ok(format!("{}${}", path, child)),
            Symbol::VariableDeclaration { .. } => Err(SymbolError::InvalidChild),
        }
    }

    pub fn get_anonymous_block_name(&mut self, path: &str) -> Result<String, SymbolError> {
        let name = self.get_child_name(path, self.block_count.to_string().as_str())?;
        self.block_count += 1;
        Ok(name)
    }

    pub fn add_symbol(&mut self, path: &str, symbol: Symbol) -> Result<(), SymbolError> {
        if self.symbols.contains_key(path) {
            Err(SymbolError::SymbolAlreadyExists(path.to_string()))?
        }
        self.symbols.insert(path.to_string(), symbol);
        Ok(())
    }

    pub fn add_variable(
        &mut self,
        path: &str,
        declared_type: Option<Type>,
    ) -> Result<(), SymbolError> {
        self.add_symbol(
            path,
            Symbol::VariableDeclaration {
                declared_type,
                resolved_type: None,
            },
        )
    }

    pub fn add_function(
        &mut self,
        path: &str,
        args: Vec<(Identifier, Type)>,
        rtype: Option<Type>,
    ) -> Result<(), SymbolError> {
        let mut declared_signature: Vec<Option<Type>> =
            args.iter().map(|(_, t)| Some(t).cloned()).collect();
        if declared_signature.is_empty() {
            declared_signature.push(Some(Type::void()));
        }
        declared_signature.push(rtype);
        self.add_symbol(
            path,
            Symbol::FunctionDeclaration {
                declared_signature,
                resolved_signature: None,
            },
        )
    }

    pub fn find_symbol(&self, path: &str) -> Result<&Symbol, SymbolError> {
        match self.symbols.get(path) {
            None => Err(SymbolError::SymbolInexistant(path.to_string())),
            Some(value) => Ok(value),
        }
    }

    pub fn get_binary_operator(&self, operator: &str) -> Result<Option<Vec<&Symbol>>, SymbolError> {
        Ok(Some(
            (0..self.binop_count)
                .filter_map(|i| {
                    self.find_symbol(&format!("@binops::{}::f{}", operator, i))
                        .ok()
                })
                .collect(),
        ))
    }

    pub fn add_binary_operator(
        &mut self,
        operator: &str,
        symbol: Symbol,
    ) -> Result<(), SymbolError> {
        self.add_symbol(
            &format!("@binops::{}::f{}", operator, self.binop_count),
            symbol,
        )?;
        self.binop_count += 1;
        Ok(())
    }

    pub fn get_unary_operator(&self, operator: &str) -> Result<Option<Vec<&Symbol>>, SymbolError> {
        Ok(Some(
            (0..self.unop_count)
                .filter_map(|i| {
                    self.find_symbol(&format!("@unops::{}::f{}", operator, i))
                        .ok()
                })
                .collect(),
        ))
    }

    pub fn add_unary_operator(
        &mut self,
        operator: &str,
        symbol: Symbol,
    ) -> Result<(), SymbolError> {
        self.add_symbol(
            &format!("@unops::{}::f{}", operator, self.unop_count),
            symbol,
        )?;
        self.unop_count += 1;
        Ok(())
    }
}
