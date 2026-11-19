//! Builds package and `.uz2` bytes for tests.

#![allow(dead_code)]

use std::io::Write;

use flate2::Compression;
use flate2::write::ZlibEncoder;

/// The fields of a summary, written as they are on disk. Negative values can
/// be written where the format has an `i32`.
#[derive(Clone)]
pub struct SummaryBytes {
    pub tag: u32,
    pub file_version: u32,
    pub flags: u32,
    pub names: (i32, i32),
    pub exports: (i32, i32),
    pub imports: (i32, i32),
    pub guid: [u32; 4],
    /// Written as the generation count unless `generation_count` is set.
    pub generations: Vec<(i32, i32)>,
    pub generation_count: Option<i32>,
    /// Before version 68: the heritage count and offset, in place of the
    /// GUID and generations.
    pub heritage: (i32, i32),
    /// Zeros added after the summary.
    pub padding: usize,
}

impl Default for SummaryBytes {
    fn default() -> Self {
        SummaryBytes {
            tag: 0x9E2A83C1,
            file_version: 128 | 29 << 16,
            flags: 0x21,
            names: (0, 0),
            exports: (0, 0),
            imports: (0, 0),
            guid: [0x01234567, 0x89ABCDEF, 0xDEADBEEF, 0x0000002A],
            generations: vec![(0, 0)],
            generation_count: None,
            heritage: (0, 0),
            padding: 0,
        }
    }
}

impl SummaryBytes {
    pub fn bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        let i32 = |out: &mut Vec<u8>, v: i32| out.extend_from_slice(&v.to_le_bytes());
        out.extend_from_slice(&self.tag.to_le_bytes());
        out.extend_from_slice(&self.file_version.to_le_bytes());
        out.extend_from_slice(&self.flags.to_le_bytes());
        for (count, offset) in [self.names, self.exports, self.imports] {
            i32(&mut out, count);
            i32(&mut out, offset);
        }
        if self.file_version as u16 >= 68 {
            for part in self.guid {
                out.extend_from_slice(&part.to_le_bytes());
            }
            let count = self
                .generation_count
                .unwrap_or(self.generations.len() as i32);
            i32(&mut out, count);
            for &(exports, names) in &self.generations {
                i32(&mut out, exports);
                i32(&mut out, names);
            }
        } else {
            i32(&mut out, self.heritage.0);
            i32(&mut out, self.heritage.1);
        }
        out.resize(out.len() + self.padding, 0);
        out
    }
}

/// A GUID's bytes.
pub fn guid_bytes(guid: [u32; 4]) -> Vec<u8> {
    guid.iter().flat_map(|p| p.to_le_bytes()).collect()
}

/// One `.uz2` block, with the sizes given.
pub fn uz2_block(compressed: &[u8], uncompressed_size: u32) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&(compressed.len() as u32).to_le_bytes());
    out.extend_from_slice(&uncompressed_size.to_le_bytes());
    out.extend_from_slice(compressed);
    out
}

pub fn zlib(data: &[u8]) -> Vec<u8> {
    let mut encoder = ZlibEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(data).unwrap();
    encoder.finish().unwrap()
}

/// Compresses `data` as `ucc compress` does: a block for each 32 KiB.
pub fn uz2(data: &[u8]) -> Vec<u8> {
    data.chunks(32768)
        .flat_map(|chunk| uz2_block(&zlib(chunk), chunk.len() as u32))
        .collect()
}

/// A compact index, written as the engine writes it.
pub fn index(value: i32) -> Vec<u8> {
    let mut v = value.unsigned_abs();
    let mut out = vec![(if value < 0 { 0x80 } else { 0 }) | (v & 0x3f) as u8];
    if v >= 0x40 {
        out[0] |= 0x40;
        v >>= 6;
        for i in 0..4 {
            if i == 3 {
                out.push(v as u8);
                break;
            }
            let more = v >= 0x80;
            out.push((v & 0x7f) as u8 | if more { 0x80 } else { 0 });
            if !more {
                break;
            }
            v >>= 7;
        }
    }
    out
}

/// A name table entry: a Latin-1 `FString` and flags.
pub fn name(text: &str) -> Vec<u8> {
    let mut out = index(text.len() as i32 + 1);
    out.extend(text.chars().map(|c| c as u8));
    out.push(0);
    out.extend_from_slice(&0x00070010u32.to_le_bytes());
    out
}

#[derive(Clone, Copy)]
pub struct ImportBytes {
    pub class_package: i32,
    pub class_name: i32,
    pub outer: i32,
    pub object_name: i32,
}

#[derive(Clone, Copy)]
pub struct ExportBytes {
    pub class: i32,
    pub super_struct: i32,
    pub outer: i32,
    pub object_name: i32,
    pub flags: u32,
    /// This many bytes of data are written for it.
    pub size: i32,
    /// Overrides the offset of its data.
    pub offset: Option<i32>,
}

impl Default for ExportBytes {
    fn default() -> Self {
        ExportBytes {
            class: 0,
            super_struct: 0,
            outer: 0,
            object_name: 0,
            flags: 0x00070004,
            size: 1,
            offset: None,
        }
    }
}

/// A whole package, laid out as `SavePackage` writes it: summary, names,
/// export data, imports, exports.
#[derive(Clone, Default)]
pub struct PackageBytes {
    pub summary: SummaryBytes,
    /// Encoded name table entries (see [`name`]).
    pub names: Vec<Vec<u8>>,
    pub imports: Vec<ImportBytes>,
    pub exports: Vec<ExportBytes>,
}

impl PackageBytes {
    pub fn bytes(&self) -> Vec<u8> {
        let summary_len = self.summary.bytes().len() as i32;
        let mut body = Vec::new();
        let name_offset = summary_len;
        for entry in &self.names {
            body.extend(entry);
        }
        let mut offsets = Vec::new();
        for export in &self.exports {
            offsets.push(summary_len + body.len() as i32);
            body.resize(body.len() + export.size.max(0) as usize, 0xAB);
        }
        let import_offset = summary_len + body.len() as i32;
        for import in &self.imports {
            body.extend(index(import.class_package));
            body.extend(index(import.class_name));
            body.extend_from_slice(&import.outer.to_le_bytes());
            body.extend(index(import.object_name));
        }
        let export_offset = summary_len + body.len() as i32;
        for (export, offset) in self.exports.iter().zip(offsets) {
            body.extend(index(export.class));
            body.extend(index(export.super_struct));
            body.extend_from_slice(&export.outer.to_le_bytes());
            body.extend(index(export.object_name));
            body.extend_from_slice(&export.flags.to_le_bytes());
            body.extend(index(export.size));
            if export.size != 0 {
                body.extend(index(export.offset.unwrap_or(offset)));
            }
        }
        let mut out = SummaryBytes {
            names: (self.names.len() as i32, name_offset),
            imports: (self.imports.len() as i32, import_offset),
            exports: (self.exports.len() as i32, export_offset),
            padding: 0,
            ..self.summary.clone()
        }
        .bytes();
        out.extend(body);
        out
    }
}
