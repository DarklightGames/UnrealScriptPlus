//! Package flags and object flags.

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

/// Defines a flags newtype over `u32` with the names of its known bits.
macro_rules! flags {
    ($(#[$meta:meta])* $name:ident, $prefix:literal, [$(($flag:literal, $bit:literal)),* $(,)?]) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
        pub struct $name(pub u32);

        impl $name {
            /// Each flag's name and bit.
            pub const ALL: &'static [(&'static str, u32)] = &[$(($flag, $bit)),*];

            #[doc = concat!("The names of the flags that are set, without the `", $prefix, "` prefix.")]
            pub fn names(self) -> Vec<&'static str> {
                Self::ALL
                    .iter()
                    .filter(|(_, bit)| self.0 & bit != 0)
                    .map(|(name, _)| *name)
                    .collect()
            }

            /// The bits that are set but aren't a known flag.
            pub fn unknown(self) -> u32 {
                Self::ALL.iter().fold(self.0, |bits, (_, bit)| bits & !bit)
            }
        }

        /// Serialized as `{ "bits": .., "names": [..], "unknown": .. }`.
        #[cfg(feature = "serde")]
        impl Serialize for $name {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                Fields {
                    bits: self.0,
                    names: self.names(),
                    unknown: self.unknown(),
                }
                .serialize(serializer)
            }
        }
    };
}

#[cfg(feature = "serde")]
#[derive(Serialize)]
struct Fields {
    bits: u32,
    names: Vec<&'static str>,
    unknown: u32,
}

flags!(
    /// The package flags (`EPackageFlags`).
    PackageFlags,
    "PKG_",
    [
        ("AllowDownload", 0x0001),
        ("ClientOptional", 0x0002),
        ("ServerSideOnly", 0x0004),
        ("BrokenLinks", 0x0008),
        ("Unsecure", 0x0010),
        ("Official", 0x0020),
        ("ExtraDownloadFile", 0x4000),
        ("Need", 0x8000),
    ]
);

flags!(
    /// An export's object flags (`EObjectFlags`). Only the flags in the
    /// engine's `RF_Load` mask are saved, so only those are named.
    ObjectFlags,
    "RF_",
    [
        ("Transactional", 0x00000001),
        ("Public", 0x00000004),
        ("SourceModified", 0x00000020),
        ("Final", 0x00000080),
        ("PerObjectLocalized", 0x00000100),
        ("Protected", 0x00000800),
        ("LoadForClient", 0x00010000),
        ("LoadForServer", 0x00020000),
        ("LoadForEdit", 0x00040000),
        ("Standalone", 0x00080000),
        ("NotForClient", 0x00100000),
        ("NotForServer", 0x00200000),
        ("NotForEdit", 0x00400000),
        ("HasStack", 0x02000000),
        ("Native", 0x04000000),
    ]
);
