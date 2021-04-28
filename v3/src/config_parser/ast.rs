use std::collections::{HashMap, HashSet};

use super::{span::Spanned, ParserError};

#[derive(Debug)]
pub struct Device {
    pub name: Spanned<String>,
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
    File(FileRef),
}

impl ArgumentValue {
    pub fn from_raw<'a>(
        source_contents: &'a str,
        value: super::raw_ast::ArgumentValue,
    ) -> Result<ArgumentValue, ParserError<'a>> {
        use super::raw_ast::ArgumentValue::*;

        Ok(match value {
            Bool(b) => ArgumentValue::Bool(b),
            String(s) => ArgumentValue::String(s),
            Int(i) => ArgumentValue::Int(i),
            Float(f) => ArgumentValue::Float(f),
            Array(a) => ArgumentValue::Array(
                a.into_iter()
                    .map(|v| v.try_map(|v| ArgumentValue::from_raw(source_contents, v)))
                    .collect::<Result<_, _>>()?,
            ),
            AssociativeArray(a) => ArgumentValue::AssociativeArray(
                a.into_iter()
                    .map(|(k, v)| {
                        v.try_map(|v| ArgumentValue::from_raw(source_contents, v)).map(|v| (k, v))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            BindingReference { name, expr } => ArgumentValue::BindingReference { name, expr },
            File { path } => {
                let contents = std::fs::read(*path).map_err(|error| ParserError::FileError {
                    source_file: source_contents,
                    path,
                    error,
                })?;
                let file_ref = FileRef { path, contents };
                ArgumentValue::File(file_ref)
            }
        })
    }
}

#[derive(Debug)]
pub struct FileRef {
    path: Spanned<String>,
    contents: Vec<u8>,
}

#[derive(Debug)]
pub struct BindingExpr {
    pub expr: String,
}

// Compared to the RawAst, this names unnamed Devices according to `ty#num` and
// reads in the Files (to make diffing of the files possible)
#[derive(Debug)]
pub struct Ast {
    devices: Vec<Spanned<Device>>,
}

impl Ast {
    pub fn from_raw<'a>(
        source_contents: &'a str,
        raw_ast: super::raw_ast::RawAst,
    ) -> Result<Ast, ParserError<'a>> {
        fn convert_devices<'a>(
            source_contents: &'a str,
            device_names: &mut HashSet<String>,
            device_type_counts: &mut HashMap<String, usize>,
            devices: Vec<Spanned<super::raw_ast::Device>>,
        ) -> Result<Vec<Spanned<Device>>, ParserError<'a>> {
            Ok(devices
                .into_iter()
                .map(|device| {
                    Ok(device.try_map(|device| {
                        let super::raw_ast::Device {
                            name,
                            ty,
                            subdevices,
                            args,
                            parent_binding_expr,
                        } = device;
                        let subdevices = convert_devices(
                            source_contents,
                            device_names,
                            device_type_counts,
                            subdevices,
                        )?;
                        let name = match name {
                            Some(name) => name,
                            None => {
                                device_type_counts
                                    .entry((*ty).clone())
                                    .and_modify(|e| *e += 1)
                                    .or_insert(0);
                                ty.clone().map(|_| format!("{}#{}", *ty, device_type_counts[&*ty]))
                            }
                        };

                        if !device_names.insert((*name).clone()) {
                            Err(ParserError::DuplicateDeviceName {
                                source_file: source_contents,
                                name,
                            })
                        } else {
                            let args = args
                                .into_iter()
                                .map(|(k, v)| {
                                    v.try_map(|v| Ok(ArgumentValue::from_raw(source_contents, v)?))
                                        .map(|v| (k, v))
                                })
                                .collect::<Result<_, _>>()?;
                            Ok(Device { name, ty, subdevices, args, parent_binding_expr })
                        }
                    })?)
                })
                .collect::<Result<_, _>>()?)
        }

        let mut device_type_counts: HashMap<String, usize> = HashMap::new();
        let mut device_names: HashSet<String> = HashSet::new();

        Ok(Ast {
            devices: convert_devices(
                source_contents,
                &mut device_names,
                &mut device_type_counts,
                raw_ast.devices,
            )?,
        })
    }
}

#[derive(Debug)]
pub struct FlatAst(Vec<Device>);
