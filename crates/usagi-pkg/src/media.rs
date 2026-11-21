//! Textures' and sounds' native data (`UTexture::Serialize` in
//! `Engine/Src/UnTex.cpp`, `USound::Serialize` in `Engine/Src/UnAudio.cpp`).

use crate::archive::{Archive, FString, LazyArray, NameRef, Serialize};
use crate::{Error, ObjectRef, Package, Result};

/// The first version in which every texture has one array of mipmaps.
/// Before it, a texture with `bHasComp` has two (`OLDbHasComp`).
pub const TEXTURE_MIPS_VERSION: u16 = 84;

/// A mipmap (`FMipmap`, `Engine/Inc/UnTex.h`).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Mipmap {
    /// The pixels, in the texture's `Format`.
    pub data: LazyArray<u8>,
    pub u_size: i32,
    pub v_size: i32,
    /// The base-2 logarithm of `u_size`.
    pub u_bits: u8,
    /// The base-2 logarithm of `v_size`.
    pub v_bits: u8,
}

impl Serialize for Mipmap {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.data.serialize(ar)?;
        ar.i32(&mut self.u_size, "mipmap")?;
        ar.i32(&mut self.v_size, "mipmap")?;
        ar.u8(&mut self.u_bits, "mipmap")?;
        ar.u8(&mut self.v_bits, "mipmap")
    }
}

/// A texture's native data, after its tagged properties: its mipmaps,
/// largest first (`SerializeMips`). From version
/// [`TEXTURE_MIPS_VERSION`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Texture {
    pub mips: Vec<Mipmap>,
}

impl Serialize for Texture {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.array_of(&mut self.mips, "texture's mipmaps")
    }
}

impl Package {
    /// Export `export`'s data from `offset` (where its tagged properties
    /// end) as a texture's. Returns it and where it ends: a subclass of
    /// `Texture` may have more after it.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if the package is older than
    /// [`TEXTURE_MIPS_VERSION`], or the data is malformed.
    ///
    /// # Panics
    ///
    /// If `export` is out of range.
    pub fn texture(&self, export: usize, offset: usize) -> Result<(Texture, usize)> {
        if self.summary.version() < TEXTURE_MIPS_VERSION {
            return Err(Error::InvalidData {
                offset,
                reason: format!(
                    "textures before version {} aren't read",
                    TEXTURE_MIPS_VERSION
                ),
            });
        }
        self.read_native(export, offset)
    }
}

/// A sound's native data, after its tagged properties.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Sound {
    /// The format of `data`, such as `WAV`.
    pub file_type: NameRef,
    /// From licensee version 2.
    pub likelihood: Option<f32>,
    /// The sound file.
    pub data: LazyArray<u8>,
}

impl Serialize for Sound {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.name(&mut self.file_type, "sound")?;
        if ar.licensee_version() >= 2 {
            ar.f32(self.likelihood.get_or_insert(0.0), "sound")?;
        }
        self.data.serialize(ar)
    }
}

/// The licensee version from which a sound group stores its sounds, rather
/// than the package they're in.
pub const SOUND_GROUP_SOUNDS_LICENSEE_VERSION: u16 = 0x1B;

/// A sound group's native data (`USoundGroup::Serialize`), after its tagged
/// properties. It doesn't start with a sound's: the engine skips
/// `USound::Serialize`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SoundGroup {
    /// Before licensee version [`SOUND_GROUP_SOUNDS_LICENSEE_VERSION`]: the
    /// package its sounds are in.
    pub package: Option<FString>,
    /// From licensee version [`SOUND_GROUP_SOUNDS_LICENSEE_VERSION`]: its
    /// sounds.
    pub sounds: Option<Vec<ObjectRef>>,
}

impl Serialize for SoundGroup {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        if ar.licensee_version() < SOUND_GROUP_SOUNDS_LICENSEE_VERSION {
            self.package
                .get_or_insert_with(FString::default)
                .serialize(ar)
        } else {
            ar.array_of(
                self.sounds.get_or_insert_with(Vec::new),
                "sound group's sounds",
            )
        }
    }
}
