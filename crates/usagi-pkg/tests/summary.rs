mod common;

use common::{SummaryBytes, guid_bytes};
use usagi_pkg::{
    Error, Generation, Guid, Heritage, PACKAGE_FILE_TAG, Package, PackageFlags, Summary,
    TRIPWIRE_PACKAGE_FILE_TAG,
};

fn read(summary: &SummaryBytes) -> Result<Summary, Error> {
    Summary::read(&summary.bytes())
}

fn invalid(summary: &SummaryBytes) -> String {
    match read(summary) {
        Err(Error::InvalidSummary(message)) => message,
        other => panic!("expected an invalid summary, got {:?}", other),
    }
}

#[test]
fn reads_a_current_summary() {
    let summary = read(&SummaryBytes {
        names: (2, 64),
        imports: (1, 74),
        exports: (1, 81),
        generations: vec![(1, 1), (3, 2)],
        padding: 40,
        ..SummaryBytes::default()
    })
    .unwrap();
    assert_eq!(summary.tag, PACKAGE_FILE_TAG);
    assert_eq!(summary.file_version, 128 | 29 << 16);
    assert_eq!(summary.version(), 128);
    assert_eq!(summary.licensee_version(), 29);
    assert_eq!(summary.package_flags, PackageFlags(0x21));
    assert_eq!((summary.name_count, summary.name_offset), (2, 64));
    assert_eq!((summary.import_count, summary.import_offset), (1, 74));
    assert_eq!((summary.export_count, summary.export_offset), (1, 81));
    assert_eq!(
        summary.guid,
        Guid {
            a: 0x01234567,
            b: 0x89ABCDEF,
            c: 0xDEADBEEF,
            d: 0x2A
        }
    );
    assert_eq!(
        summary.generations,
        vec![
            Generation {
                export_count: 1,
                name_count: 1
            },
            Generation {
                export_count: 3,
                name_count: 2
            }
        ]
    );
    assert_eq!(summary.heritage, None);
}

#[test]
fn reads_red_orchestra_and_killing_floor_packages() {
    // Tripwire's engine writes a different tag; the rest is the same.
    let standard = SummaryBytes::default();
    let tripwire = SummaryBytes {
        tag: 0x9E2A83C2,
        ..standard.clone()
    };
    let summary = read(&tripwire).unwrap();
    assert_eq!(summary.tag, TRIPWIRE_PACKAGE_FILE_TAG);
    assert_eq!(
        Summary {
            tag: PACKAGE_FILE_TAG,
            ..summary
        },
        read(&standard).unwrap()
    );
}

#[test]
fn guid_is_formatted_like_the_engine() {
    let guid = Guid {
        a: 0x01234567,
        b: 0x89ABCDEF,
        c: 0xDEADBEEF,
        d: 0x2A,
    };
    assert_eq!(guid.to_string(), "0123456789ABCDEFDEADBEEF0000002A");
}

#[test]
fn package_flag_names_and_unknown_bits() {
    let flags = PackageFlags(0x0001 | 0x0020 | 0x4000 | 0x8000 | 0x10000 | 0x0100);
    assert_eq!(
        flags.names(),
        vec!["AllowDownload", "Official", "ExtraDownloadFile", "Need"]
    );
    assert_eq!(flags.unknown(), 0x10100);
    assert!(PackageFlags(0).names().is_empty());
    assert_eq!(PackageFlags(0x3F).unknown(), 0);
}

#[test]
fn version_67_reads_the_heritage_table() {
    // The summary is 44 bytes; the heritage table follows it.
    let mut bytes = SummaryBytes {
        file_version: 67,
        names: (3, 0),
        exports: (2, 0),
        heritage: (2, 44),
        ..SummaryBytes::default()
    }
    .bytes();
    bytes.extend(guid_bytes([1, 2, 3, 4]));
    bytes.extend(guid_bytes([5, 6, 7, 8]));
    let summary = Summary::read(&bytes).unwrap();
    assert_eq!(summary.version(), 67);
    assert_eq!(summary.licensee_version(), 0);
    assert_eq!(
        summary.heritage,
        Some(Heritage {
            count: 2,
            offset: 44
        })
    );
    // The last GUID wins, and the generation is made from the counts.
    assert_eq!(
        summary.guid,
        Guid {
            a: 5,
            b: 6,
            c: 7,
            d: 8
        }
    );
    assert_eq!(
        summary.generations,
        vec![Generation {
            export_count: 2,
            name_count: 3
        }]
    );
}

#[test]
fn an_empty_heritage_table_leaves_the_guid_zero() {
    let summary = read(&SummaryBytes {
        file_version: 60,
        heritage: (0, 9999),
        ..SummaryBytes::default()
    })
    .unwrap();
    assert_eq!(summary.guid, Guid::default());
}

#[test]
fn a_heritage_table_past_the_end_is_an_error() {
    let result = read(&SummaryBytes {
        file_version: 67,
        heritage: (1, 44),
        ..SummaryBytes::default()
    });
    assert!(matches!(
        result,
        Err(Error::UnexpectedEof {
            offset: 44,
            what: "heritage table"
        })
    ));
}

#[test]
fn the_tag_is_checked_first() {
    let bytes = SummaryBytes {
        tag: 0x12345678,
        ..SummaryBytes::default()
    }
    .bytes();
    let error = Summary::read(&bytes[..4]).unwrap_err();
    assert!(matches!(error, Error::NotAPackage { tag: 0x12345678 }));
    assert_eq!(
        error.to_string(),
        "not an Unreal package (the tag is 0x12345678, not 0x9E2A83C1 or 0x9E2A83C2)"
    );
}

#[test]
fn every_truncation_is_an_error() {
    for summary in [
        SummaryBytes::default(),
        SummaryBytes {
            file_version: 67,
            ..SummaryBytes::default()
        },
    ] {
        let bytes = summary.bytes();
        for len in 0..bytes.len() {
            let error = Summary::read(&bytes[..len]).unwrap_err();
            assert!(
                matches!(error, Error::UnexpectedEof { .. }),
                "{} bytes: {:?}",
                len,
                error
            );
        }
        Summary::read(&bytes).unwrap();
    }
    assert_eq!(
        Summary::read(&[0xC1, 0x83]).unwrap_err().to_string(),
        "the file ends in the tag at offset 0"
    );
}

#[test]
fn negative_counts_and_offsets_are_errors() {
    let base = SummaryBytes::default();
    let cases: [(SummaryBytes, &str); 9] = [
        (
            SummaryBytes {
                names: (-1, 0),
                ..base.clone()
            },
            "the name count is -1",
        ),
        (
            SummaryBytes {
                names: (0, -2),
                ..base.clone()
            },
            "the name offset is -2",
        ),
        (
            SummaryBytes {
                exports: (-3, 0),
                ..base.clone()
            },
            "the export count is -3",
        ),
        (
            SummaryBytes {
                exports: (0, -4),
                ..base.clone()
            },
            "the export offset is -4",
        ),
        (
            SummaryBytes {
                imports: (-5, 0),
                ..base.clone()
            },
            "the import count is -5",
        ),
        (
            SummaryBytes {
                imports: (0, -6),
                ..base.clone()
            },
            "the import offset is -6",
        ),
        (
            SummaryBytes {
                generation_count: Some(-1),
                ..base.clone()
            },
            "the generation count is -1",
        ),
        (
            SummaryBytes {
                generations: vec![(-7, 0)],
                ..base.clone()
            },
            "the generation export count is -7",
        ),
        (
            SummaryBytes {
                generations: vec![(0, -8)],
                ..base.clone()
            },
            "the generation name count is -8",
        ),
    ];
    for (summary, expected) in cases {
        assert_eq!(invalid(&summary), expected);
    }
    let old = SummaryBytes {
        file_version: 67,
        ..base
    };
    assert_eq!(
        invalid(&SummaryBytes {
            heritage: (-1, 0),
            ..old.clone()
        }),
        "the heritage count is -1"
    );
    assert_eq!(
        invalid(&SummaryBytes {
            heritage: (0, -1),
            ..old
        }),
        "the heritage offset is -1"
    );
    assert_eq!(
        Error::InvalidSummary("the name count is -1".to_string()).to_string(),
        "invalid summary: the name count is -1"
    );
}

#[test]
fn a_generation_count_past_the_end_is_an_error() {
    let result = read(&SummaryBytes {
        generation_count: Some(i32::MAX),
        ..SummaryBytes::default()
    });
    assert!(matches!(
        result,
        Err(Error::UnexpectedEof {
            what: "generation export count",
            ..
        })
    ));
}

#[test]
fn table_offsets_are_not_checked() {
    // The engine doesn't check them; reading the tables does.
    let summary = read(&SummaryBytes {
        names: (1000, i32::MAX),
        ..SummaryBytes::default()
    })
    .unwrap();
    assert_eq!(summary.name_offset, i32::MAX as u32);
}

#[test]
fn reads_package_files() {
    let dir = tempfile::tempdir().unwrap();
    let bytes = SummaryBytes {
        padding: 10,
        ..SummaryBytes::default()
    }
    .bytes();
    let path = dir.path().join("Test.u");
    std::fs::write(&path, &bytes).unwrap();
    let package = Package::read(&path).unwrap();
    assert_eq!(package.size, bytes.len());
    assert_eq!(package.compressed_size, None);
    assert_eq!(package.summary, Summary::read(&bytes).unwrap());

    let error = Package::read(&dir.path().join("Missing.u")).unwrap_err();
    assert!(matches!(error, Error::Io(_)));
    assert!(std::error::Error::source(&error).is_some());
    assert!(!error.to_string().is_empty());
    let error = Package::from_bytes(b"nope").unwrap_err();
    assert!(std::error::Error::source(&error).is_none());
}

#[cfg(feature = "serde")]
#[test]
fn serializes_to_json() {
    let summary = read(&SummaryBytes::default()).unwrap();
    let json = serde_json::to_value(&summary).unwrap();
    assert_eq!(
        json,
        serde_json::json!({
            "tag": 0x9E2A83C1u32,
            "file_version": 128 | 29 << 16,
            "package_flags": { "bits": 0x21, "names": ["AllowDownload", "Official"], "unknown": 0 },
            "name_count": 0,
            "name_offset": 0,
            "export_count": 0,
            "export_offset": 0,
            "import_count": 0,
            "import_offset": 0,
            "guid": "0123456789ABCDEFDEADBEEF0000002A",
            "generations": [{ "export_count": 0, "name_count": 0 }],
            "heritage": null,
        })
    );
    let old = read(&SummaryBytes {
        file_version: 67,
        heritage: (0, 7),
        ..SummaryBytes::default()
    })
    .unwrap();
    assert_eq!(
        serde_json::to_value(&old).unwrap()["heritage"],
        serde_json::json!({ "count": 0, "offset": 7 })
    );
}
