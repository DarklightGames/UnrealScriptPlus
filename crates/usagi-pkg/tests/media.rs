mod common;

use common::{ExportBytes, PackageBytes, SummaryBytes, index, name};
use usagi_pkg::archive::{Archive, FString, LazyArray, Loader, NameRef, Saver, Serialize};
use usagi_pkg::{
    Mipmap, ObjectRef, Package, SOUND_GROUP_SOUNDS_LICENSEE_VERSION, Sound, SoundGroup, Texture,
};

/// A package at `version` and `licensee` whose names are `None` and `WAV`,
/// with an export of each data.
fn package_at(version: u32, licensee: u32, data: &[Vec<u8>]) -> Package {
    let bytes = PackageBytes {
        summary: SummaryBytes {
            file_version: version | licensee << 16,
            ..SummaryBytes::default()
        },
        names: vec![name("None"), name("WAV")],
        imports: Vec::new(),
        exports: data
            .iter()
            .map(|data| ExportBytes {
                size: data.len() as i32,
                ..ExportBytes::default()
            })
            .collect(),
        data: data.to_vec(),
    }
    .bytes();
    let mut package = Package::from_bytes(&bytes).unwrap();
    package.name = "MyPkg".to_string();
    package
}

fn package(version: u32, licensee: u32) -> Package {
    package_at(version, licensee, &[])
}

/// Writes `value` as if at `file_offset` in the file.
fn saved<T: Serialize>(package: &Package, mut value: T, file_offset: usize) -> Vec<u8> {
    let summary = &package.summary;
    let mut saver = Saver::new(summary.version(), summary.licensee_version()).at(file_offset);
    assert_eq!(saver.file_offset(), file_offset);
    value.serialize(&mut saver).unwrap();
    saver.into_bytes()
}

/// Reads a `T` from `bytes`, as if at `file_offset` in the file, and checks
/// that it's all of them, and that it can't be read from fewer.
fn loaded<T: Serialize + std::fmt::Debug>(
    package: &Package,
    bytes: &[u8],
    file_offset: usize,
) -> T {
    for len in 0..bytes.len() {
        let mut loader = Loader::new(package, &bytes[..len]).at(file_offset);
        assert!(
            T::default().serialize(&mut loader).is_err(),
            "{} bytes",
            len
        );
    }
    let mut loader = Loader::new(package, bytes).at(file_offset);
    assert_eq!(loader.file_offset(), file_offset);
    let mut value = T::default();
    value.serialize(&mut loader).unwrap();
    assert_eq!(loader.pos(), bytes.len(), "{:?}", value);
    value
}

fn lazy(items: &[u8]) -> LazyArray<u8> {
    LazyArray {
        seek_pos: None,
        items: items.to_vec(),
    }
}

#[test]
fn a_lazy_array_starts_with_where_it_ends_in_the_file() {
    let package = package(128, 29);
    let bytes = saved(&package, lazy(&[1, 2, 3]), 0x100);
    // SeekPos (0x100 + 4 + 1 + 3), the count, the bytes.
    assert_eq!(bytes, [0x08, 0x01, 0, 0, 3, 1, 2, 3]);
    let array: LazyArray<u8> = loaded(&package, &bytes, 0x100);
    assert_eq!(array.seek_pos, Some(0x108));
    assert_eq!(array.items, [1, 2, 3]);
    // Read elsewhere in the file, it ends before its SeekPos says: the
    // engine carries on from SeekPos.
    let mut padded = bytes.clone();
    padded.extend([0xAA, 0xBB]);
    let array: LazyArray<u8> = loaded(&package, &padded, 0x100 - 2);
    assert_eq!(array.items, [1, 2, 3]);
    // Or after it.
    let mut loader = Loader::new(&package, &bytes).at(0x101);
    let error = lazy(&[]).serialize(&mut loader).unwrap_err();
    assert_eq!(
        error.to_string(),
        "invalid data at offset 0: the lazy array ends at 0x109, after its SeekPos, 0x108"
    );
}

#[test]
fn a_lazy_array_before_version_62_is_only_the_array() {
    let package = package(61, 0);
    let bytes = saved(&package, lazy(&[7, 8]), 0x100);
    assert_eq!(bytes, [2, 7, 8]);
    let array: LazyArray<u8> = loaded(&package, &bytes, 0x100);
    assert_eq!(array, lazy(&[7, 8]));
    // A negative count.
    let mut loader = Loader::new(&package, &[0x81]);
    let error = lazy(&[]).serialize(&mut loader).unwrap_err();
    assert_eq!(
        error.to_string(),
        "invalid data at offset 0: the lazy array has -1 items"
    );
}

fn mip(pixels: &[u8], size: i32, bits: u8) -> Mipmap {
    Mipmap {
        data: lazy(pixels),
        u_size: size,
        v_size: size,
        u_bits: bits,
        v_bits: bits,
    }
}

#[test]
fn textures_are_their_mipmaps() {
    let package = package(128, 29);
    let texture = Texture {
        mips: vec![mip(&[1, 2, 3, 4], 2, 1), mip(&[5], 1, 0)],
    };
    let bytes = saved(&package, texture.clone(), 0x40);
    let mut expected = vec![2];
    expected.extend(0x4Ai32.to_le_bytes());
    expected.extend([4, 1, 2, 3, 4]);
    expected.extend([2, 0, 0, 0, 2, 0, 0, 0, 1, 1]);
    expected.extend(0x5Ai32.to_le_bytes());
    expected.extend([1, 5]);
    expected.extend([1, 0, 0, 0, 1, 0, 0, 0, 0, 0]);
    assert_eq!(bytes, expected);
    let read: Texture = loaded(&package, &bytes, 0x40);
    assert_eq!(read.mips.len(), 2);
    assert_eq!(read.mips[0].data.seek_pos, Some(0x4A));
    assert_eq!(read.mips[1].u_size, 1);
    assert_eq!(read.mips[1].data.items, [5]);
}

#[test]
fn a_texture_is_read_where_its_data_is_in_the_file() {
    // The export's data, a tagged-properties `None` then a texture, written
    // for where the export is in the file.
    let texture = || Texture {
        mips: vec![mip(&[9, 9], 1, 0)],
    };
    let data = |package: &Package, offset: usize| {
        let mut data = index(0);
        data.extend(saved(package, texture(), offset + 1));
        data
    };
    let placeholder = package_at(128, 29, &[data(&package(128, 29), 0)]);
    let offset = placeholder.exports()[0].serial_offset.unwrap() as usize;
    let package = package_at(128, 29, &[data(&placeholder, offset)]);
    let (read, end) = package.texture(0, 1).unwrap();
    assert_eq!(end, package.export_data(0).len());
    assert_eq!(read.mips[0].data.items, [9, 9]);
    // Read as if the data were at the start of the file, its SeekPos is
    // past the end.
    let mut loader = Loader::new(&package, package.export_data(0));
    loader.seek(1);
    assert!(Texture::default().serialize(&mut loader).is_err());
    // Packages before version 84 can have two arrays of mipmaps.
    let old = package_at(83, 0, &[data(&placeholder, offset)]);
    assert_eq!(
        old.texture(0, 1).unwrap_err().to_string(),
        "invalid data at offset 1: textures before version 84 aren't read"
    );
}

#[test]
fn sounds_are_their_file_type_likelihood_and_data() {
    let package = package(128, 29);
    let sound = Sound {
        file_type: NameRef(1),
        likelihood: Some(0.5),
        data: lazy(&[1, 2]),
    };
    let bytes = saved(&package, sound.clone(), 0);
    let mut expected = vec![1];
    expected.extend(0.5f32.to_le_bytes());
    expected.extend(12i32.to_le_bytes());
    expected.extend([2, 1, 2]);
    assert_eq!(bytes, expected);
    let read: Sound = loaded(&package, &bytes, 0);
    assert_eq!((read.file_type, read.likelihood), (NameRef(1), Some(0.5)));
    assert_eq!(read.data.items, [1, 2]);
    // No likelihood before licensee version 2.
    let package = package_at(128, 1, &[]);
    let sound = Sound {
        likelihood: None,
        ..sound
    };
    let bytes = saved(&package, sound, 0);
    assert_eq!(bytes, [1, 8, 0, 0, 0, 2, 1, 2]);
    let read: Sound = loaded(&package, &bytes, 0);
    assert_eq!(read.likelihood, None);
}

#[test]
fn sound_groups_are_their_sounds_or_their_package() {
    // From licensee version 27, its sounds.
    let package = package_at(128, 29, &[vec![0]]);
    let group = SoundGroup {
        package: None,
        sounds: Some(vec![ObjectRef::Null, ObjectRef::Export(0)]),
    };
    let bytes = saved(&package, group.clone(), 0);
    assert_eq!(bytes, [2, 0, 1]);
    assert_eq!(loaded::<SoundGroup>(&package, &bytes, 0), group);
    // An object out of range.
    let mut loader = Loader::new(&package, &[1, 2]);
    assert!(SoundGroup::default().serialize(&mut loader).is_err());
    // Before, the package they're in.
    let package = package_at(128, SOUND_GROUP_SOUNDS_LICENSEE_VERSION as u32 - 1, &[]);
    let group = SoundGroup {
        package: Some(FString::Latin1(b"Pkg\0".to_vec())),
        sounds: None,
    };
    let bytes = saved(&package, group.clone(), 0);
    assert_eq!(bytes, [4, b'P', b'k', b'g', 0]);
    assert_eq!(loaded::<SoundGroup>(&package, &bytes, 0), group);
}
