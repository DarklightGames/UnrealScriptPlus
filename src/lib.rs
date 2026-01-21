pub mod ast;
mod test;
pub mod parser;
pub mod visitor;
pub mod transform;

use pest::error::{ErrorVariant, LineColLocation, InputLocation};
use crate::parser::{ParsingError, ParserRule, ProgramResult};

// PYTHON STUFF
use pyo3::prelude::*;
use pyo3::{create_exception, PyErr};
use pyo3::types::PyDict;
use pyo3::exceptions;

create_exception!(unrealscriptplus, ScriptParseError, pyo3::exceptions::PyException);

fn parsing_error_to_py_err(py: Python, error: ParsingError) -> PyErr {
    match error {
        ParsingError::PestError(e) => ScriptParseError::new_err(pest_error_to_py_object(py, e)),
        ParsingError::EncodingError(e) => exceptions::PyUnicodeEncodeError::new_err(e.to_string()),
        ParsingError::IoError(e) => exceptions::PyIOError::new_err(e.to_string())
    }
}

fn program_result_to_py_object(py: Python, result: &ProgramResult) -> Py<PyAny> {
    let errors: Vec<Py<PyAny>> = result.errors.iter().map(|e| {
        let dict = PyDict::new(py);
        dict.set_item("line", e.span.line).unwrap();
        dict.set_item("message", &e.message).unwrap();
        dict.set_item("severity", format!("{:?}", e.severity)).unwrap();
        dict.into()
    }).collect();
    
    let dict = PyDict::new(py);
    dict.set_item("errors", errors).unwrap();
    dict.into()
}

fn pest_error_to_py_object(py: Python, e: pest_consume::Error<ParserRule>) -> Py<PyAny> {
    let dict = PyDict::new(py);
    
    match e.line_col {
        LineColLocation::Pos(pos) => {
            dict.set_item("line_col", pos).unwrap();
        },
        LineColLocation::Span(span1, span2) => {
            dict.set_item("line_col", (span1, span2)).unwrap();
        }
    };
    
    match e.location {
        InputLocation::Pos(pos) => {
            dict.set_item("location", pos).unwrap();
        },
        InputLocation::Span(span) => {
            dict.set_item("location", span).unwrap();
        }
    };
    
    let variant: Py<PyAny> = match e.variant {
        ErrorVariant::ParsingError { positives, negatives } => {
            let positives: Vec<String> = positives.into_iter().map(|r| format!("{:?}", r)).collect();
            let negatives: Vec<String> = negatives.into_iter().map(|r| format!("{:?}", r)).collect();
            let variant_dict = PyDict::new(py);
            variant_dict.set_item("type", "parsing").unwrap();
            variant_dict.set_item("positives", positives).unwrap();
            variant_dict.set_item("negatives", negatives).unwrap();
            variant_dict.into()
        },
        ErrorVariant::CustomError { message } => {
            let variant_dict = PyDict::new(py);
            variant_dict.set_item("type", "custom").unwrap();
            variant_dict.set_item("message", message).unwrap();
            variant_dict.into()
        },
    };
    dict.set_item("variant", variant).unwrap();
    
    dict.into()
}

#[pyfunction]
fn parse_expression(py: Python, path: &str) -> PyResult<String> {
    match crate::parser::parse_expression(path) {
        Ok(p) => { Ok(format!("{:?}", p)) }
        Err(e) => { Err(parsing_error_to_py_err(py, e.into())) }
    }
}

#[pyfunction]
fn parse_file(py: Python, path: &str) -> PyResult<Py<PyAny>> {
    match crate::parser::parse_file(path) {
        Ok(result) => {
            Ok(program_result_to_py_object(py, &result))
        }
        Err(e) => Err(parsing_error_to_py_err(py, e))
    }
}

#[pymodule]
fn unrealscriptplus(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse_expression, m)?)?;
    m.add_function(wrap_pyfunction!(parse_file, m)?)?;
    m.add("ScriptParseError", m.py().get_type::<ScriptParseError>())?;
    Ok(())
}
