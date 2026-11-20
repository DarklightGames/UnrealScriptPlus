//! Reading and writing native data with one function, as the engine does.
//!
//! The engine serializes an object with a single `Serialize(FArchive& Ar)`
//! that both loads and saves: `Ar << Value` reads into `Value` when loading
//! and writes it when saving. [`Serialize`] is the same: a type's
//! `serialize` mirrors the engine's code line for line, and an [`Archive`]
//! is either a [`Loader`] or a [`Saver`]. Reading and writing can't
//! disagree, since there's only one description of the layout.

use crate::reader::Reader;
use crate::{Error, Guid, ObjectRef, Package, Result};

/// The engine's `FArchive`: where values are read from or written to.
pub trait Archive {
    /// Whether values are being read (`Ar.IsLoading()`), rather than
    /// written.
    fn is_loading(&self) -> bool;

    /// The package's version (`Ar.Ver()`).
    fn version(&self) -> u16;

    /// The package's licensee version (`Ar.LicenseeVer()`).
    fn licensee_version(&self) -> u16;

    /// The offset of the next byte.
    fn pos(&self) -> usize;

    /// Reads into `bytes`, or writes them. `what` names them if the data
    /// ends first.
    fn raw(&mut self, bytes: &mut [u8], what: &'static str) -> Result<()>;

    /// Reads `len` bytes into `bytes`, or writes all of `bytes`.
    fn bytes(&mut self, bytes: &mut Vec<u8>, len: usize, what: &'static str) -> Result<()>;

    /// A compact index (`FCompactIndex`).
    fn index(&mut self, value: &mut i32, what: &'static str) -> Result<()>;

    /// The next `len` bytes, without reading them; `None` if there are fewer,
    /// or when saving.
    fn peek(&self, len: usize) -> Option<&[u8]>;

    /// Whether name `index` is `None`, which ends lists. Only asked when
    /// loading.
    fn is_none_name(&self, index: i32) -> bool;

    fn u8(&mut self, value: &mut u8, what: &'static str) -> Result<()> {
        let mut bytes = [*value];
        self.raw(&mut bytes, what)?;
        *value = bytes[0];
        Ok(())
    }

    fn u16(&mut self, value: &mut u16, what: &'static str) -> Result<()> {
        let mut bytes = value.to_le_bytes();
        self.raw(&mut bytes, what)?;
        *value = u16::from_le_bytes(bytes);
        Ok(())
    }

    fn u32(&mut self, value: &mut u32, what: &'static str) -> Result<()> {
        let mut bytes = value.to_le_bytes();
        self.raw(&mut bytes, what)?;
        *value = u32::from_le_bytes(bytes);
        Ok(())
    }

    fn i32(&mut self, value: &mut i32, what: &'static str) -> Result<()> {
        let mut bytes = value.to_le_bytes();
        self.raw(&mut bytes, what)?;
        *value = i32::from_le_bytes(bytes);
        Ok(())
    }

    fn u64(&mut self, value: &mut u64, what: &'static str) -> Result<()> {
        let mut bytes = value.to_le_bytes();
        self.raw(&mut bytes, what)?;
        *value = u64::from_le_bytes(bytes);
        Ok(())
    }

    fn f32(&mut self, value: &mut f32, what: &'static str) -> Result<()> {
        let mut bytes = value.to_le_bytes();
        self.raw(&mut bytes, what)?;
        *value = f32::from_le_bytes(bytes);
        Ok(())
    }

    /// An object reference: a compact index.
    fn object(&mut self, value: &mut ObjectRef, what: &'static str) -> Result<()>;

    /// A name (`FName`): a compact index into the name table.
    fn name(&mut self, value: &mut NameRef, what: &'static str) -> Result<()> {
        self.index(&mut value.0, what)
    }
}

impl dyn Archive + '_ {
    /// A dynamic array (`TArray`): a compact index count, then the items.
    pub fn array_of<T: Serialize>(&mut self, items: &mut Vec<T>, what: &'static str) -> Result<()> {
        let mut count = i32::try_from(items.len()).expect("fewer than 2^31 items");
        let at = self.pos();
        self.index(&mut count, what)?;
        let count = usize::try_from(count).map_err(|_| Error::InvalidData {
            offset: at,
            reason: format!("the {} has {} items", what, count),
        })?;
        if self.is_loading() {
            items.clear();
            items.resize_with(count, T::default);
        }
        for item in items {
            item.serialize(self)?;
        }
        Ok(())
    }
}

/// A type the engine serializes with `operator<<` or `Serialize`: one
/// function that reads it from a [`Loader`] or writes it to a [`Saver`].
pub trait Serialize: Default {
    /// Reads into `self` when `ar` is loading; writes `self` when it's
    /// saving.
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()>;
}

/// A name in the package's name table, by index, as it's stored.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct NameRef(pub i32);

impl Serialize for NameRef {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.name(self, "name")
    }
}

impl Serialize for ObjectRef {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.object(self, "object reference")
    }
}

impl Serialize for Guid {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.u32(&mut self.a, "GUID")?;
        ar.u32(&mut self.b, "GUID")?;
        ar.u32(&mut self.c, "GUID")?;
        ar.u32(&mut self.d, "GUID")
    }
}

/// A string (`FString`), as stored: Latin-1 bytes or UTF-16 code units,
/// including the terminating null. Kept as stored so that it's written back
/// the same; [`text`](FString::text) is its text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FString {
    Latin1(Vec<u8>),
    Utf16(Vec<u16>),
}

impl Default for FString {
    fn default() -> Self {
        FString::Latin1(Vec::new())
    }
}

impl FString {
    /// The text, up to the first null, as the engine reads it.
    pub fn text(&self) -> String {
        let text: String = match self {
            FString::Latin1(bytes) => bytes.iter().map(|&b| char::from(b)).collect(),
            FString::Utf16(units) => String::from_utf16_lossy(units),
        };
        match text.find('\0') {
            Some(end) => text[..end].to_string(),
            None => text,
        }
    }
}

impl Serialize for FString {
    /// A compact index count, including the null: positive for Latin-1,
    /// negative for UTF-16. Then the characters.
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        let (mut count, mut bytes) = match self {
            FString::Latin1(bytes) => (
                i32::try_from(bytes.len()).expect("a short string"),
                bytes.clone(),
            ),
            FString::Utf16(units) => (
                -i32::try_from(units.len()).expect("a short string"),
                units.iter().flat_map(|u| u.to_le_bytes()).collect(),
            ),
        };
        ar.index(&mut count, "string")?;
        let len = count.unsigned_abs() as usize;
        let len = if count >= 0 {
            len
        } else {
            len.saturating_mul(2)
        };
        ar.bytes(&mut bytes, len, "string")?;
        if ar.is_loading() {
            *self = if count >= 0 {
                FString::Latin1(bytes)
            } else {
                FString::Utf16(
                    bytes
                        .chunks_exact(2)
                        .map(|c| u16::from_le_bytes([c[0], c[1]]))
                        .collect(),
                )
            };
        }
        Ok(())
    }
}

/// Reads values from an export's data. Names and object references are
/// checked against the package's tables, as the engine checks them.
pub struct Loader<'a> {
    package: &'a Package,
    reader: Reader<'a>,
}

impl<'a> Loader<'a> {
    /// A loader of `data`, an export's data in `package`.
    pub fn new(package: &'a Package, data: &'a [u8]) -> Self {
        Loader {
            package,
            reader: Reader::new(data),
        }
    }

    /// Continues from `pos`.
    pub fn seek(&mut self, pos: usize) {
        self.reader.seek(pos);
    }
}

impl Archive for Loader<'_> {
    fn is_loading(&self) -> bool {
        true
    }

    fn version(&self) -> u16 {
        self.package.summary.version()
    }

    fn licensee_version(&self) -> u16 {
        self.package.summary.licensee_version()
    }

    fn pos(&self) -> usize {
        self.reader.pos()
    }

    fn raw(&mut self, bytes: &mut [u8], what: &'static str) -> Result<()> {
        bytes.copy_from_slice(self.reader.bytes(bytes.len(), what)?);
        Ok(())
    }

    fn bytes(&mut self, bytes: &mut Vec<u8>, len: usize, what: &'static str) -> Result<()> {
        *bytes = self.reader.bytes(len, what)?.to_vec();
        Ok(())
    }

    fn index(&mut self, value: &mut i32, what: &'static str) -> Result<()> {
        *value = self.reader.index(what)?;
        Ok(())
    }

    fn peek(&self, len: usize) -> Option<&[u8]> {
        self.reader.peek(len)
    }

    fn is_none_name(&self, index: i32) -> bool {
        self.package
            .name(index as usize)
            .eq_ignore_ascii_case("None")
    }

    fn object(&mut self, value: &mut ObjectRef, what: &'static str) -> Result<()> {
        let at = self.pos();
        let raw = self.reader.index(what)?;
        *value = ObjectRef::from_raw(raw);
        let in_range = match *value {
            ObjectRef::Null => true,
            ObjectRef::Export(i) => i < self.package.exports().len(),
            ObjectRef::Import(i) => i < self.package.imports().len(),
        };
        if !in_range {
            return Err(Error::InvalidData {
                offset: at,
                reason: format!("object reference {} in the {} is out of range", raw, what),
            });
        }
        Ok(())
    }

    fn name(&mut self, value: &mut NameRef, what: &'static str) -> Result<()> {
        let at = self.pos();
        value.0 = self.reader.index(what)?;
        if usize::try_from(value.0).map_or(true, |i| i >= self.package.names().len()) {
            return Err(Error::InvalidData {
                offset: at,
                reason: format!("name {} in the {} is out of range", value.0, what),
            });
        }
        Ok(())
    }
}

/// Writes values, as the engine saves them.
pub struct Saver {
    bytes: Vec<u8>,
    version: u16,
    licensee_version: u16,
}

impl Saver {
    /// A saver for a package with these versions.
    pub fn new(version: u16, licensee_version: u16) -> Self {
        Saver {
            bytes: Vec::new(),
            version,
            licensee_version,
        }
    }

    /// What has been written.
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

impl Archive for Saver {
    fn is_loading(&self) -> bool {
        false
    }

    fn version(&self) -> u16 {
        self.version
    }

    fn licensee_version(&self) -> u16 {
        self.licensee_version
    }

    fn pos(&self) -> usize {
        self.bytes.len()
    }

    fn raw(&mut self, bytes: &mut [u8], _: &'static str) -> Result<()> {
        self.bytes.extend_from_slice(bytes);
        Ok(())
    }

    fn bytes(&mut self, bytes: &mut Vec<u8>, _: usize, _: &'static str) -> Result<()> {
        self.bytes.extend_from_slice(bytes);
        Ok(())
    }

    fn index(&mut self, value: &mut i32, _: &'static str) -> Result<()> {
        self.bytes.extend(compact_index(*value));
        Ok(())
    }

    fn object(&mut self, value: &mut ObjectRef, _: &'static str) -> Result<()> {
        self.bytes.extend(compact_index(value.raw()));
        Ok(())
    }

    fn peek(&self, _: usize) -> Option<&[u8]> {
        None
    }

    fn is_none_name(&self, _: i32) -> bool {
        false
    }
}

/// A compact index as the engine writes it (`operator<<(FArchive&,
/// FCompactIndex&)` in `Core/Src/UnObj.cpp`): the sign and 6 bits, then up
/// to three bytes of 7 bits and one of 8, least significant first, each
/// with a bit saying whether another follows.
fn compact_index(value: i32) -> Vec<u8> {
    let mut v = value.unsigned_abs();
    let mut b0 = if value < 0 { 0x80 } else { 0 };
    if v < 0x40 {
        b0 |= v as u8;
    } else {
        b0 |= (v & 0x3f) as u8 | 0x40;
    }
    let mut out = vec![b0];
    if b0 & 0x40 != 0 {
        v >>= 6;
        for i in 0..4 {
            if i == 3 || v < 0x80 {
                out.push(v as u8);
                break;
            }
            out.push((v & 0x7f) as u8 | 0x80);
            v >>= 7;
        }
    }
    out
}
