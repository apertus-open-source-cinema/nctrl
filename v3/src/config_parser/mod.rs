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

#[derive(Debug, Copy, Clone)]
pub struct SourceFile<'a> {
    pub contents: &'a str,
    pub file_name: &'a str,
}


#[derive(Error, Debug)]
pub enum ParserError<'a> {
    #[error("{}", ParserError::format_parse_error(.source_file, .error))]
    ParsingError { source_file: SourceFile<'a>, error: ParseError<usize, Token<'a>, &'a str> },
    #[error("{}", ParserError::format_duplicate_device_name(.source_file, .name))]
    DuplicateDeviceName { source_file: SourceFile<'a>, name: Spanned<String> },
    #[error("{}", ParserError::format_file_error(.source_file, .path, .error))]
    FileError { source_file: SourceFile<'a>, path: Spanned<String>, error: std::io::Error },
}

impl<'a> ParserError<'a> {
    fn format_file_error(
        source_file: &SourceFile<'a>,
        path: &'a Spanned<String>,
        error: &'a std::io::Error,
    ) -> String {
        path.print_note_and_message_with_context(
            source_file.file_name,
            source_file.contents,
            &format!("{}", error),
            "",
        )
    }

    fn format_duplicate_device_name(
        source_file: &SourceFile<'a>,
        name: &Spanned<String>,
    ) -> String {
        name.print_note_and_message_with_context(
            source_file.file_name,
            source_file.contents,
            "duplicate device name",
            "",
        )
    }

    fn format_parse_error(
        source_file: &SourceFile<'a>,
        error: &'a ParseError<usize, Token<'a>, &'a str>,
    ) -> String {
        match error {
            ParseError::InvalidToken { location } => Spanned::new(*location, location + 1, ())
                .print_note_and_message_with_context(
                    source_file.file_name,
                    source_file.contents,
                    "invalid token",
                    "",
                ),
            ParseError::UnrecognizedEOF { location, expected } => {
                Spanned::new(*location, location + 1, ()).print_note_and_message_with_context(
                    source_file.file_name,
                    source_file.contents,
                    "unrecognized EOF",
                    &format!("expected one of: {}", ParserError::display_vec(expected)),
                )
            }
            ParseError::UnrecognizedToken { token, expected } => Spanned::new(token.0, token.2, ())
                .print_note_and_message_with_context(
                    source_file.file_name,
                    source_file.contents,
                    "unrecognized token",
                    &format!("expected one of: {}", ParserError::display_vec(expected)),
                ),
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

pub fn parse<'a>(source_file: SourceFile<'a>) -> Result<raw_ast::RawAst, ParserError<'a>> {
    parser::ConfigFileParser::new()
        .parse(source_file.contents)
        .map_err(|e| ParserError::ParsingError { source_file, error: e })
}
