use unrealscriptplus::parser::{parse_file, parse_program, ParsingError, ProgramResult};
use unrealscriptplus::transform::{ScriptBuilder, ScriptFormattingOptions, ToScript};
use std::fs::File;
use std::io::Read;

fn main() {
    if let Ok(mut file) = File::open("./tests/Test.uc") {
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        match parse_program(contents.as_str()) {
            Ok(program) => {
                let mut builder = ScriptBuilder::new(ScriptFormattingOptions { });
                program.program.to_script(&mut builder);
                println!("{}", builder.to_string());
            }
            Err(error) => {
                match error {
                    ParsingError::IoError(_) => {}
                    ParsingError::EncodingError(_) => {}
                    ParsingError::PestError(error) => {
                        println!("{:?}", error);
                    }
                }
            }
        }
    }
}