use lalrpop_util::{lalrpop_mod, lexer::Token, ParseError};
use std::fmt::Display;
use thiserror::Error;

use self::span::Spanned;

mod ast;
mod parser_helpers;
mod raw_ast;
mod span;

pub use ast::Ast;

lalrpop_mod!(parser, "/config_parser/parser.rs");


#[derive(Error, Debug)]
pub enum ParserError<'a> {
    #[error("{}", ParserError::format_parse_error(.source_file, .error))]
    ParsingError { source_file: &'a str, error: ParseError<usize, Token<'a>, &'a str> },
    #[error("{}", ParserError::format_duplicate_device_name(.source_file, .name))]
    DuplicateDeviceName { source_file: &'a str, name: Spanned<String> },
    #[error("{}", ParserError::format_file_error(.source_file, .path, .error))]
    FileError { source_file: &'a str, path: Spanned<String>, error: () },
}

impl<'a> ParserError<'a> {
    fn format_file_error(source_file: &'a str, path: &'a Spanned<String>, error: &'a ()) -> String {
        todo!()
    }

    fn format_duplicate_device_name(source_file: &'a str, name: &'a str) -> String { todo!() }

    fn find_context(source_file: &'a str, location: usize) -> (&'a str, String, usize) {
        let mut start = location;
        let mut end = location;

        let mut broke_start = false;
        while &source_file[start..start + 1] != "\n" {
            if start > 0 {
                start -= 1;
            } else {
                broke_start = true;
                break
            }
        }
        if !broke_start {
            start += 1;
        }

        while &source_file[end..end + 1] != "\n" {
            if end + 1 == source_file.len() {
                end += 1;
                break
            } else {
                end += 1;
            }
        }

        let num_spacing = location - start;
        let spacing = " ".repeat(num_spacing);
        let lineno = &source_file[..start + 1].chars().filter(|x| *x == '\n').count() + 1;

        (&source_file[start..end], spacing, lineno)
    }

    fn format_parse_error(
        source_file: &'a str,
        error: &'a ParseError<usize, Token<'a>, &'a str>,
    ) -> String {
        match error {
            ParseError::InvalidToken { location } => {
                let (context_line, spacing, lineno) =
                    ParserError::find_context(source_file, *location);

                format!(
                    r#"Invalid token in line {}:
{}∨
{}"#,
                    lineno, spacing, context_line
                )
            }
            ParseError::UnrecognizedEOF { location, expected } => {
                let lineno =
                    &source_file[..location + 1].chars().filter(|x| *x == '\n').count() + 1;
                format!(
                    r#"Unrecognized EOF in line {}
Expected one of: {}"#,
                    lineno,
                    ParserError::display_vec(expected),
                )
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let (context_line, spacing, lineno) =
                    ParserError::find_context(source_file, token.0);
                let length = token.2 - token.0;
                let marker = "∨".repeat(length);

                format!(
                    r#"Unrecognized token in line {}:
{}{}
{}
Expected one of: {}"#,
                    lineno,
                    spacing,
                    marker,
                    context_line,
                    ParserError::display_vec(expected),
                )
            }
            _ => {
                format!("{}", error)
            }
        }
    }

    fn display_vec<T: Display>(v: &Vec<T>) -> String {
        let mut res = String::new();
        for e in v {
            res += &format!("{}, ", e);
        }
        (&res[..res.len() - 2]).to_owned()
    }
}

pub fn parse<'a>(contents: &'a str) -> Result<raw_ast::RawAst, ParserError<'a>> {
    parser::ConfigFileParser::new()
        .parse(contents)
        .map_err(|e| ParserError::ParsingError { source_file: contents, error: e })
}
