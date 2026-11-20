mod common;

use common::{SummaryBytes, uz2};
use usagi_pkg::{Error, Package, PackageFlags, set_package_flags};

#[test]
fn sets_the_flags_of_a_package() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("Test.u");
    let before = SummaryBytes {
        padding: 100,
        ..SummaryBytes::default()
    }
    .bytes();
    std::fs::write(&path, &before).unwrap();
    set_package_flags(&path, PackageFlags(0x25)).unwrap();
    let after = std::fs::read(&path).unwrap();
    // Only the 4 flag bytes change.
    assert_eq!(after.len(), before.len());
    assert_eq!(after[8..12], 0x25u32.to_le_bytes());
    assert_eq!(after[..8], before[..8]);
    assert_eq!(after[12..], before[12..]);
    assert_eq!(
        Package::read(&path).unwrap().summary.package_flags,
        PackageFlags(0x25)
    );
}

#[test]
fn sets_the_flags_of_a_uz2() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("Test.u.UZ2");
    // More than one block.
    let mut bytes = SummaryBytes::default().bytes();
    bytes.extend((0..70000u32).map(|i| (i % 251) as u8));
    std::fs::write(&path, uz2(&bytes)).unwrap();
    set_package_flags(&path, PackageFlags(0x4)).unwrap();
    let package = Package::read(&path).unwrap();
    assert_eq!(package.summary.package_flags, PackageFlags(0x4));
    assert_eq!(package.size, bytes.len());
    let mut expected = bytes;
    expected[8..12].copy_from_slice(&4u32.to_le_bytes());
    let decompressed = usagi_pkg::uz2::decompress(&std::fs::read(&path).unwrap()).unwrap();
    assert_eq!(decompressed, expected);
}

#[test]
fn only_packages_are_changed() {
    let dir = tempfile::tempdir().unwrap();
    let text = dir.path().join("Readme.u");
    std::fs::write(&text, b"not a package at all").unwrap();
    assert!(matches!(
        set_package_flags(&text, PackageFlags(1)),
        Err(Error::NotAPackage { .. })
    ));
    assert_eq!(std::fs::read(&text).unwrap(), b"not a package at all");

    let compressed = dir.path().join("Readme.uz2");
    let original = uz2(b"not a package at all");
    std::fs::write(&compressed, &original).unwrap();
    assert!(matches!(
        set_package_flags(&compressed, PackageFlags(1)),
        Err(Error::NotAPackage { .. })
    ));
    assert_eq!(std::fs::read(&compressed).unwrap(), original);

    // Not compressed, though it's named .uz2.
    let named = dir.path().join("Plain.uz2");
    std::fs::write(&named, SummaryBytes::default().bytes()).unwrap();
    assert!(matches!(
        set_package_flags(&named, PackageFlags(1)),
        Err(Error::Uz2 { .. })
    ));

    assert!(matches!(
        set_package_flags(&dir.path().join("Missing.u"), PackageFlags(1)),
        Err(Error::Io(_))
    ));
}

#[test]
fn compress_round_trips() {
    for len in [0, 1, 32768, 32769, 100_000] {
        let bytes: Vec<u8> = (0..len).map(|i| (i * 7 % 256) as u8).collect();
        let compressed = usagi_pkg::uz2::compress(&bytes);
        assert_eq!(usagi_pkg::uz2::decompress(&compressed).unwrap(), bytes);
    }
    assert!(usagi_pkg::uz2::compress(&[]).is_empty());
}

#[test]
fn write_errors_are_reported() {
    let dir = tempfile::tempdir().unwrap();
    let bytes = SummaryBytes::default().bytes();
    for (name, contents) in [("Test.u", bytes.clone()), ("Test.u.uz2", uz2(&bytes))] {
        let path = dir.path().join(name);
        std::fs::write(&path, &contents).unwrap();
        let mut permissions = std::fs::metadata(&path).unwrap().permissions();
        permissions.set_readonly(true);
        std::fs::set_permissions(&path, permissions).unwrap();
        let result = set_package_flags(&path, PackageFlags(4));
        if std::fs::OpenOptions::new().write(true).open(&path).is_ok() {
            // Running as root, which can write read-only files.
            continue;
        }
        assert!(
            matches!(result, Err(Error::Io(_))),
            "{}: {:?}",
            name,
            result
        );
        assert_eq!(std::fs::read(&path).unwrap(), contents);
    }
}
