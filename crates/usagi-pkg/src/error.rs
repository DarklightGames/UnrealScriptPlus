use std::fmt;
use std::io;

/// A problem reading a package.
#[derive(Debug)]
pub enum Error {
    /// The file couldn't be read.
    Io(io::Error),
    /// The file doesn't start with a package tag.
    NotAPackage {
        /// The first four bytes, as a little-endian `u32`.
        tag: u32,
    },
    /// The file ended in the middle of something.
    UnexpectedEof {
        /// Where the thing that was cut short starts.
        offset: usize,
        /// What was being read.
        what: &'static str,
    },
    /// The summary has a value the file can't have, such as a negative
    /// count.
    InvalidSummary(String),
    /// An import or export has a value the engine would fail on, such as a
    /// name or object reference that's out of range.
    InvalidTable(String),
    /// An export's data has a value the engine would fail on, such as a
    /// property tag that runs past the end of the data.
    InvalidData {
        /// Where the problem is, in the export's data.
        offset: usize,
        /// What's wrong.
        reason: String,
    },
    /// The data is something this crate can't read yet.
    Unsupported(String),
    /// A `.uz2` file is malformed.
    Uz2 {
        /// Where the block with the problem starts.
        offset: usize,
        /// What's wrong with it.
        reason: String,
    },
}

/// The result of reading a package.
pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{}", e),
            Error::NotAPackage { tag } => write!(
                f,
                "not an Unreal package (the tag is {:#010X}, not {:#010X} or {:#010X})",
                tag,
                crate::PACKAGE_FILE_TAG,
                crate::TRIPWIRE_PACKAGE_FILE_TAG
            ),
            Error::UnexpectedEof { offset, what } => {
                write!(f, "the file ends in the {} at offset {}", what, offset)
            }
            Error::InvalidSummary(message) => write!(f, "invalid summary: {}", message),
            Error::InvalidTable(message) => write!(f, "invalid {}", message),
            Error::InvalidData { offset, reason } => {
                write!(f, "invalid data at offset {}: {}", offset, reason)
            }
            Error::Unsupported(message) => write!(f, "{}", message),
            Error::Uz2 { offset, reason } => {
                write!(f, "invalid .uz2 block at offset {}: {}", offset, reason)
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}
