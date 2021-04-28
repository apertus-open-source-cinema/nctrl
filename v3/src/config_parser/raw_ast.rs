pub use super::ast::BindingExpr;
use super::span::Spanned;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Device {
    pub name: Option<Spanned<String>>,
    pub ty: Spanned<String>,
    pub subdevices: Vec<Spanned<Device>>,
    pub parent_binding_expr: Option<Spanned<BindingExpr>>,
    pub args: HashMap<Spanned<String>, Spanned<ArgumentValue>>,
}

#[derive(Debug)]
pub enum ArgumentValue {
    Bool(bool),
    String(String),
    Int(i64),
    Float(f64),
    Array(Vec<Spanned<ArgumentValue>>),
    AssociativeArray(HashMap<Spanned<String>, Spanned<ArgumentValue>>),
    BindingReference { name: Spanned<String>, expr: Spanned<BindingExpr> },
    File { path: Spanned<String> },
}

#[derive(Debug)]
pub struct RawAst {
    pub devices: Vec<Spanned<Device>>,
}
