//! `.uz2` files: packages compressed for redirect downloads by
//! `ucc compress`. See `FILECOPY_Compress` in the engine's
//! `Core/Inc/FFileManagerGeneric.h`.
//!
//! The file is a sequence of blocks to the end of the file, one for each
//! 32 KiB of the original, each:
//!
//! | Field | Type |
//! |---|---|
//! | compressed size | `u32` |
//! | uncompressed size | `u32` |
//! | data | a zlib stream, `compressed size` bytes |

use std::io::Read;

use flate2::read::ZlibDecoder;

use crate::reader::Reader;
use crate::{Error, Result};

/// The most a block decompresses to (`COPYBLOCKSIZE`).
pub const BLOCK_SIZE: u32 = 32768;

/// The most a compressed block can take (`MAXCOMPSIZE`).
pub const MAX_COMPRESSED_SIZE: u32 = 33096;

/// Decompresses a `.uz2` file.
///
/// Like the engine, it rejects blocks whose sizes are over the limits, and
/// blocks that inflate to more than their uncompressed size. A block that
/// inflates to less is accepted, as the engine does.
pub fn decompress(bytes: &[u8]) -> Result<Vec<u8>> {
    let mut r = Reader::new(bytes);
    let mut out = Vec::new();
    while !r.at_end() {
        let offset = r.pos();
        let error = |reason: String| Error::Uz2 { offset, reason };
        let compressed_size = r.u32("block's compressed size")?;
        let uncompressed_size = r.u32("block's uncompressed size")?;
        if compressed_size > MAX_COMPRESSED_SIZE {
            return Err(error(format!(
                "the compressed size, {}, is over {}",
                compressed_size, MAX_COMPRESSED_SIZE
            )));
        }
        if uncompressed_size > BLOCK_SIZE {
            return Err(error(format!(
                "the uncompressed size, {}, is over {}",
                uncompressed_size, BLOCK_SIZE
            )));
        }
        let data = r.bytes(compressed_size as usize, "compressed block")?;
        // Read one byte more than allowed, to tell if there's too much.
        let start = out.len();
        ZlibDecoder::new(data)
            .take(u64::from(uncompressed_size) + 1)
            .read_to_end(&mut out)
            .map_err(|e| error(format!("zlib: {}", e)))?;
        if out.len() - start > uncompressed_size as usize {
            return Err(error(format!(
                "it decompresses to more than its uncompressed size, {}",
                uncompressed_size
            )));
        }
    }
    Ok(out)
}

/// Compresses a file as `ucc compress` does: a block for each 32 KiB, each
/// a zlib stream at the default level.
pub fn compress(bytes: &[u8]) -> Vec<u8> {
    use flate2::Compression;
    use flate2::write::ZlibEncoder;
    use std::io::Write;

    let mut out = Vec::new();
    for chunk in bytes.chunks(BLOCK_SIZE as usize) {
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(chunk).expect("writing to a Vec");
        let data = encoder.finish().expect("writing to a Vec");
        out.extend((data.len() as u32).to_le_bytes());
        out.extend((chunk.len() as u32).to_le_bytes());
        out.extend(data);
    }
    out
}
