use std::process::ExitCode;

pub mod ast;
pub mod bcu;
pub mod bir;
pub mod lexer;
pub mod lowerer;
pub mod macros;
pub mod parser;
pub mod scope;
pub mod token;

use bcu::{Bcu, BcuFile};

struct Cli {
    program: String,
    command: Command,
}

enum Command {
    Compile(CompileOptions),
}

struct CompileOptions {
    root_file: BcuFile,
    output_file_path: Option<String>,
}

impl Cli {
    fn parse() -> Result<Cli, String> {
        let mut args = std::env::args();

        let program = args.next().expect("program name should be passed");

        let Some(command) = args.next() else {
            return Err("command not provided".to_string());
        };

        let command = match command.as_str() {
            | "compile" => {
                let Some(root_file_path) = args.next() else {
                    return Err("root file path not provided".to_string());
                };

                let root_file_buffer = match std::fs::read_to_string(&root_file_path) {
                    | Ok(buffer) => buffer,

                    | Err(err) => {
                        return Err(format!("could not read file '{}': {}", root_file_path, err));
                    }
                };

                let root_file = BcuFile::new(root_file_path, root_file_buffer);

                let mut output_file_path = None;

                while let Some(option) = args.next() {
                    match option.as_str() {
                        | "--output" => {
                            let Some(provided_file_path) = args.next() else {
                                return Err("output file path not provided".to_string());
                            };

                            output_file_path = Some(provided_file_path);
                        }

                        | _ => return Err(format!("unknown option: {}", option)),
                    }
                }

                Command::Compile(CompileOptions {
                    root_file,
                    output_file_path,
                })
            }

            | _ => return Err(format!("unknown command: {}", command)),
        };

        Ok(Cli { program, command })
    }

    fn execute(self) -> ExitCode {
        match self.command {
            | Command::Compile(options) => {
                let mut bcu = Bcu::new();

                let ast = match bcu.parse(&options.root_file) {
                    | Ok(ast) => ast,

                    | Err(err) => {
                        eprintln!("error: {}", err);

                        return ExitCode::FAILURE;
                    }
                };

                match bcu.lower(&options.root_file, ast) {
                    | Ok(bir) => bir,

                    | Err(err) => {
                        eprintln!("error: {}", err);

                        return ExitCode::FAILURE;
                    }
                };

                ExitCode::SUCCESS
            }
        }
    }
}

fn main() -> ExitCode {
    match Cli::parse() {
        | Ok(cli) => cli.execute(),

        | Err(err) => {
            eprintln!("error: {}", err);

            ExitCode::FAILURE
        }
    }
}
