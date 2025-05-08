use std::{process::exit, str::FromStr};

pub mod ast;
pub mod analyzer;
pub mod bair;
pub mod bcu;
pub mod bir;
pub mod lexer;
pub mod lowerer;
pub mod macros;
pub mod parser;
pub mod scope;
pub mod token;

use bcu::{Bcu, SourceFile, Target};

struct Cli {
    command: Command,
}

enum Command {
    Compile(CompileOptions),
}

#[allow(dead_code)]
struct CompileOptions {
    root_file: SourceFile,
    output_file_path: Option<String>,
    target: Target,
}

impl Cli {
    fn parse() -> Cli {
        let mut args = std::env::args();

        args.next().expect("program name should be passed");

        let Some(command) = args.next() else {
            eprintln!("error: command not provided");
            exit(1);
        };

        let command = match command.as_str() {
            | "compile" => {
                let Some(root_file_path) = args.next() else {
                    eprintln!("error: root file path not provided");
                    exit(1);
                };

                let root_file_buffer = std::fs::read_to_string(&root_file_path).unwrap_or_else(|err| {
                    eprintln!("error: could not read file '{}': {}", root_file_path, err);
                    exit(1);
                });

                let root_file = SourceFile::new(root_file_path, root_file_buffer);
                let mut output_file_path = None;
                let mut target = Target::native();

                while let Some(option) = args.next() {
                    match option.as_str() {
                        | "--output" => {
                            let Some(provided_file_path) = args.next() else {
                                eprintln!("error: output file path not provided");
                                exit(1);
                            };

                            output_file_path = Some(provided_file_path);
                        }

                        | "--target" => {
                            let Some(provided_target_query) = args.next() else {
                                eprintln!("error: target query not provided");
                                exit(1);
                            };

                            target = Target::from_str(&provided_target_query).unwrap_or_else(|err| {
                                eprintln!("error: failed to parse target query: {}", err);
                                exit(1);
                            });
                        }

                        | _ => {
                            eprintln!("error: unknown option: {}", option);
                            exit(1);
                        }
                    }
                }

                Command::Compile(CompileOptions {
                    root_file,
                    output_file_path,
                    target,
                })
            }

            | _ => {
                eprintln!("error: unknown command: {}", command);
                exit(1);
            }
        };

        Cli { command }
    }

    fn execute(self) {
        match self.command {
            | Command::Compile(options) => {
                let mut bcu = Bcu::new(options.target);

                let ast = bcu.parse(&options.root_file).unwrap_or_else(|err| {
                    eprintln!("error: {}", err);
                    exit(1);
                });

                let bir = bcu.lower(&options.root_file, ast).unwrap_or_else(|err| {
                    eprintln!("error: {}", err);
                    exit(1);
                });

                let bair = bcu.analyze(&options.root_file, bir).unwrap_or_else(|err| {
                    eprintln!("error: {}", err);
                    exit(1);
                });

                println!("{:#?}", bair);

            }
        }
    }
}

fn main() {
    Cli::parse().execute()
}
