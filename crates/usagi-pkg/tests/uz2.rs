mod common;

use common::{SummaryBytes, uz2, uz2_block, zlib};
use usagi_pkg::uz2::decompress;
use usagi_pkg::{Error, Package, Summary};

fn uz2_error(bytes: &[u8]) -> (usize, String) {
    match decompress(bytes) {
        Err(Error::Uz2 { offset, reason }) => (offset, reason),
        other => panic!("expected a .uz2 error, got {:?}", other),
    }
}

#[test]
fn round_trips_several_blocks() {
    // A summary followed by 80000 bytes that aren't all the same, so the
    // blocks differ.
    let mut bytes = SummaryBytes::default().bytes();
    bytes.extend((0..80000u32).map(|i| (i * 7 % 251) as u8));
    let compressed = uz2(&bytes);
    assert_eq!(decompress(&compressed).unwrap(), bytes);

    let package = Package::from_uz2_bytes(&compressed).unwrap();
    assert_eq!(package.size, bytes.len());
    assert_eq!(package.compressed_size, Some(compressed.len()));
    assert_eq!(package.summary, Summary::read(&bytes).unwrap());
}

#[test]
fn an_empty_file_decompresses_to_nothing() {
    assert_eq!(decompress(&[]).unwrap(), Vec::<u8>::new());
}

#[test]
fn block_sizes_over_the_limits_are_errors() {
    let block = uz2_block(&vec![0; 33097], 100);
    assert_eq!(
        uz2_error(&block),
        (0, "the compressed size, 33097, is over 33096".to_string())
    );
    let mut bytes = uz2(b"first");
    let second = bytes.len();
    bytes.extend(uz2_block(&zlib(b"x"), 32769));
    let (offset, reason) = uz2_error(&bytes);
    assert_eq!(offset, second);
    assert_eq!(reason, "the uncompressed size, 32769, is over 32768");
    assert_eq!(
        Error::Uz2 { offset, reason }.to_string(),
        format!(
            "invalid .uz2 block at offset {}: the uncompressed size, 32769, is over 32768",
            second
        )
    );
}

#[test]
fn truncated_blocks_are_errors() {
    let bytes = uz2(b"some data");
    for (len, what) in [
        (2, "block's compressed size"),
        (6, "block's uncompressed size"),
        (bytes.len() - 1, "compressed block"),
    ] {
        match decompress(&bytes[..len]) {
            Err(Error::UnexpectedEof { what: found, .. }) => assert_eq!(found, what),
            other => panic!("{} bytes: {:?}", len, other),
        }
    }
}

#[test]
fn corrupt_zlib_is_an_error() {
    let (offset, reason) = uz2_error(&uz2_block(b"not zlib at all", 100));
    assert_eq!(offset, 0);
    assert!(reason.starts_with("zlib: "), "{}", reason);
}

#[test]
fn a_block_may_inflate_to_less_but_not_more() {
    // The engine accepts a block that inflates to less than it says.
    assert_eq!(decompress(&uz2_block(&zlib(b"abc"), 10)).unwrap(), b"abc");
    assert_eq!(
        uz2_error(&uz2_block(&zlib(b"abcd"), 3)),
        (
            0,
            "it decompresses to more than its uncompressed size, 3".to_string()
        )
    );
}

#[test]
fn the_extension_says_whether_a_file_is_compressed() {
    let dir = tempfile::tempdir().unwrap();
    let bytes = SummaryBytes::default().bytes();
    for name in ["Test.u.uz2", "Test.u.UZ2"] {
        let path = dir.path().join(name);
        std::fs::write(&path, uz2(&bytes)).unwrap();
        let package = Package::read(&path).unwrap();
        assert_eq!(package.size, bytes.len());
        assert!(package.compressed_size.is_some());
    }
    // An uncompressed package named .uz2 isn't read as one.
    let path = dir.path().join("Plain.uz2");
    std::fs::write(&path, &bytes).unwrap();
    assert!(matches!(Package::read(&path), Err(Error::Uz2 { .. })));
    // A .uz2 that decompresses to something else isn't a package.
    assert!(matches!(
        Package::from_uz2_bytes(&uz2(b"not a package")),
        Err(Error::NotAPackage { .. })
    ));
}
