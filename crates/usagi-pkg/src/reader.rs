//! A little-endian cursor over a byte slice.

use crate::{Error, Result};

pub struct Reader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Reader { bytes, pos: 0 }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn seek(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn at_end(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    /// The next `len` bytes. `what` names them in the error if the input
    /// ends first.
    pub fn bytes(&mut self, len: usize, what: &'static str) -> Result<&'a [u8]> {
        // Saturating: a length that overflows can't fit in the input either.
        let end = self.pos.saturating_add(len);
        let bytes = self.bytes.get(self.pos..end).ok_or(Error::UnexpectedEof {
            offset: self.pos,
            what,
        })?;
        self.pos = end;
        Ok(bytes)
    }

    pub fn u32(&mut self, what: &'static str) -> Result<u32> {
        let bytes = self.bytes(4, what)?;
        Ok(u32::from_le_bytes(bytes.try_into().expect("4 bytes")))
    }

    pub fn i32(&mut self, what: &'static str) -> Result<i32> {
        Ok(self.u32(what)? as i32)
    }

    pub fn u8(&mut self, what: &'static str) -> Result<u8> {
        Ok(self.bytes(1, what)?[0])
    }

    /// A compact index (`FCompactIndex`): 1 to 5 bytes. Decoded with the
    /// engine's arithmetic, which wraps for values that don't fit in an
    /// `i32`.
    pub fn index(&mut self, what: &'static str) -> Result<i32> {
        let b0 = self.u8(what)?;
        // The continuation bytes, least significant first: up to three of 7
        // bits, then one of 8.
        let mut more = [0u8; 4];
        let mut len = 0;
        if b0 & 0x40 != 0 {
            loop {
                let b = self.u8(what)?;
                more[len] = b;
                len += 1;
                if len == 4 || b & 0x80 == 0 {
                    break;
                }
            }
        }
        // The engine builds the value from the last byte back.
        let mut value: i32 = 0;
        for (i, &b) in more[..len].iter().enumerate().rev() {
            let bits = if i == 3 { b } else { b & 0x7f };
            value = value.wrapping_shl(7).wrapping_add(i32::from(bits));
        }
        value = value.wrapping_shl(6).wrapping_add(i32::from(b0 & 0x3f));
        Ok(if b0 & 0x80 != 0 {
            value.wrapping_neg()
        } else {
            value
        })
    }

    /// A string (`FString`): a compact index count, then that many Latin-1
    /// bytes, or if it's negative, that many UTF-16 code units. The count
    /// includes the null terminator. The string ends at the first null, as
    /// it does in the engine.
    pub fn string(&mut self, what: &'static str) -> Result<String> {
        let count = self.index(what)?;
        let len = count.unsigned_abs() as usize;
        let text = if count >= 0 {
            self.bytes(len, what)?
                .iter()
                .map(|&b| char::from(b))
                .collect()
        } else {
            let units: Vec<u16> = self
                .bytes(len.saturating_mul(2), what)?
                .chunks_exact(2)
                .map(|c| u16::from_le_bytes([c[0], c[1]]))
                .collect();
            String::from_utf16_lossy(&units)
        };
        Ok(until_null(text))
    }

    /// A null-terminated Latin-1 string, as names were stored before
    /// version 64.
    pub fn c_string(&mut self, what: &'static str) -> Result<String> {
        let mut text = String::new();
        loop {
            match self.u8(what)? {
                0 => return Ok(text),
                b => text.push(char::from(b)),
            }
        }
    }
}

fn until_null(mut text: String) -> String {
    if let Some(end) = text.find('\0') {
        text.truncate(end);
    }
    text
}
