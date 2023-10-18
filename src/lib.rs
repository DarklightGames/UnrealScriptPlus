pub mod ast;
pub mod parser;
mod test;
pub mod transform;
pub mod visitor;
use crate::parser::{ParserRule, ParsingError, ProgramResult};
use pest::error::{ErrorVariant, InputLocation, LineColLocation};
use pyo3::prelude::pymodule;
use pyo3::prelude::PyModule;
use pyo3::types::PyDict;
use pyo3::{create_exception, PyErr};
use pyo3::{exceptions, PyNativeType, PyObject, PyResult, Python, ToPyObject};

create_exception!(
    unrealscriptplus,
    ScriptParseError,
    pyo3::exceptions::PyException
);

#[pymodule]
fn unrealscriptplus(py: Python, m: &PyModule) -> PyResult<()> {
    fn parsing_error_to_py_err(module: &PyModule, error: ParsingError) -> PyErr {
        match error {
            ParsingError::PestError(e) => {
                ScriptParseError::new_err(pest_error_to_py_object(module.py(), e))
            }
            ParsingError::EncodingError(e) => {
                exceptions::PyUnicodeEncodeError::new_err(e.to_string())
            }
            ParsingError::IoError(e) => exceptions::PyIOError::new_err(e.to_string()),
        }
    }

    fn program_result_to_py_object(py: Python, result: &ProgramResult) -> PyObject {
        let sequence = vec![(
            "errors".to_object(py),
            result
                .errors
                .iter()
                .map(|e| {
                    PyDict::from_sequence(
                        py,
                        vec![
                            ("line".to_object(py), e.span.line.to_object(py)).to_object(py),
                            ("message".to_object(py), e.message.to_object(py)).to_object(py),
                            (
                                "severity".to_object(py),
                                format!("{:?}", e.severity).to_string().to_object(py),
                            )
                                .to_object(py),
                        ]
                        .to_object(py),
                    )
                    .unwrap()
                    .to_object(py)
                })
                .collect::<Vec<PyObject>>()
                .to_object(py),
        )]
        .to_object(py);
        PyDict::from_sequence(py, sequence).unwrap().to_object(py)
    }

    fn pest_error_to_py_object(py: Python, e: pest_consume::Error<ParserRule>) -> PyObject {
        let sequence = vec![
            (
                "line_col".to_object(py),
                match e.line_col {
                    LineColLocation::Pos(pos) => pos.to_object(py),
                    LineColLocation::Span(span1, span2) => (span1, span2).to_object(py),
                },
            )
                .to_object(py),
            (
                "location".to_object(py),
                match e.location {
                    InputLocation::Pos(pos) => pos.to_object(py),
                    InputLocation::Span(span) => span.to_object(py),
                },
            )
                .to_object(py),
            (
                "variant".to_object(py),
                match e.variant {
                    ErrorVariant::ParsingError {
                        positives,
                        negatives,
                    } => {
                        let positives: Vec<String> = positives
                            .into_iter()
                            .map(|r| format!("{:?}", r).to_string())
                            .collect();
                        let negatives: Vec<String> = negatives
                            .into_iter()
                            .map(|r| format!("{:?}", r).to_string())
                            .collect();
                        PyDict::from_sequence(
                            py,
                            vec![
                                ("type".to_object(py), "parsing".to_object(py)).to_object(py),
                                ("positives".to_object(py), positives.to_object(py)).to_object(py),
                                ("negatives".to_object(py), negatives.to_object(py)).to_object(py),
                            ]
                            .to_object(py),
                        )
                        .unwrap()
                        .to_object(py)
                    }
                    ErrorVariant::CustomError { message } => PyDict::from_sequence(
                        py,
                        vec![
                            ("type".to_object(py), "custom".to_object(py)).to_object(py),
                            ("message".to_object(py), message.to_object(py)).to_object(py),
                        ]
                        .to_object(py),
                    )
                    .unwrap()
                    .to_object(py),
                },
            )
                .to_object(py),
        ]
        .to_object(py);
        PyDict::from_sequence(py, sequence).unwrap().to_object(py)
    }

    #[pyfn(m)]
    #[pyo3(pass_module)]
    fn parse_expression(module: &PyModule, path: &str) -> PyResult<String> {
        match crate::parser::parse_expression(path) {
            Ok(p) => Ok(format!("{:?}", p)),
            Err(e) => Err(parsing_error_to_py_err(module, e.into())),
        }
    }

    #[pyfn(m)]
    #[pyo3(pass_module)]
    fn parse_file(module: &PyModule, path: &str) -> PyResult<PyObject> {
        match crate::parser::parse_file(path) {
            Ok(result) => Ok(program_result_to_py_object(module.py(), &result)),
            Err(e) => Err(parsing_error_to_py_err(module, e)),
        }
    }
    m.add("ScriptParseError", py.get_type::<ScriptParseError>())?;
    Ok(())
}
