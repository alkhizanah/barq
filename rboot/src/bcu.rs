use crate::ast::{Module, Parser, ParserResult};

pub struct Bcu {
    pub global_assembly: String,
}

impl Bcu {
    pub const fn new() -> Bcu {
        Bcu {
            global_assembly: String::new(),
        }
    }

    pub fn parse(&mut self, file: &BcuFile) -> ParserResult<Module> {
        let mut parser = Parser::new(self, file);

        parser.parse()?;

        Ok(parser.module)
    }
}

pub struct BcuFile {
    pub path: String,
    pub buffer: String,
}

impl BcuFile {
    pub const fn new(path: String, buffer: String) -> BcuFile {
        BcuFile {
            path,
            buffer,
        }
    }
}
