//! Barq Compilation Unit
//!
//! A structure containing the resources that gets shared between pipeline

use crate::ast::{Ast, Parser, ParserResult};

pub struct Bcu {
    pub global_assembly: String,
}

impl Bcu {
    pub const fn new() -> Bcu {
        Bcu {
            global_assembly: String::new(),
        }
    }

    pub fn parse(&mut self, file: &BcuFile) -> ParserResult<Ast> {
        Parser::new(self, file).parse()
    }
}

pub struct BcuFile {
    pub path: String,
    pub buffer: String,
}

impl BcuFile {
    pub const fn new(path: String, buffer: String) -> BcuFile {
        BcuFile { path, buffer }
    }
}
