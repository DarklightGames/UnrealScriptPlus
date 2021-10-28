extern crate pest;
extern crate pest_derive;
use pest::error::{ErrorVariant, LineColLocation, InputLocation};

mod ast;
mod parser;
mod writer;
mod test;

use pest_consume;

use crate::parser::{ParsingError, ParserRule};

extern crate encoding;

#[macro_use]
extern crate lazy_static;

// PYTHON STUFF
use pyo3::prelude::{pymodule};
use pyo3::create_exception;
use pyo3::types::{PyDict};
use pyo3::{Python, PyResult, exceptions, PyNativeType, ToPyObject, PyObject};
use pyo3::prelude::PyModule;

create_exception!(unrealscriptplus, ScriptParseError, pyo3::exceptions::PyException);

#[pymodule]
fn unrealscriptplus(py: Python, m: &PyModule) -> PyResult<()> {

    fn pest_error_to_py_object(py: Python, e: pest_consume::Error<ParserRule>) -> PyObject {
        let sequence = vec![
            (
                "line_col".to_object(py),
                match e.line_col {
                    LineColLocation::Pos(pos) => pos.to_object(py),
                    LineColLocation::Span(span1, span2) => (span1, span2).to_object(py)
                }
            ).to_object(py),
            (
                "location".to_object(py),
                match e.location {
                    InputLocation::Pos(pos) => pos.to_object(py),
                    InputLocation::Span(span) => span.to_object(py)
                }
            ).to_object(py),
            (
                "variant".to_object(py),
                match e.variant {
                    ErrorVariant::ParsingError { positives, negatives } => {
                        let positives: Vec<String> = positives.into_iter().map(|r| format!("{:?}", r).to_string()).collect();
                        let negatives: Vec<String> = negatives .into_iter().map(|r| format!("{:?}", r).to_string()).collect();
                        PyDict::from_sequence(py, vec![
                            ("type".to_object(py), "parsing".to_object(py)).to_object(py),
                            ("positives".to_object(py), positives.to_object(py)).to_object(py),
                            ("negatives".to_object(py), negatives.to_object(py)).to_object(py),
                        ].to_object(py)).unwrap().to_object(py)
                    },
                    ErrorVariant::CustomError { message } => {
                        PyDict::from_sequence(py, vec![
                            ("type".to_object(py), "custom".to_object(py)).to_object(py),
                            ("message".to_object(py), message.to_object(py)).to_object(py)
                        ].to_object(py)).unwrap().to_object(py)
                    },
                }
            ).to_object(py)
        ].to_object(py);
        PyDict::from_sequence(py, sequence).unwrap().to_object(py)
    }

    #[pyfn(m)]
    #[pyo3(pass_module)]
    fn parse_file(module: &PyModule, path: &str) -> PyResult<bool> {
        match crate::parser::parse_file(path) {
            Ok(_) => Ok(true),
            Err(e) => {
                Err(match e {
                    ParsingError::PestError(e) => ScriptParseError::new_err(pest_error_to_py_object(module.py(), e)),
                    ParsingError::EncodingError(e) => exceptions::PyUnicodeEncodeError::new_err(e.to_string()),
                    ParsingError::IoError(e) => exceptions::PyIOError::new_err(e.to_string())
                })
            }
        }
    }
    m.add("ScriptParseError", py.get_type::<ScriptParseError>())?;
    Ok(())
}
