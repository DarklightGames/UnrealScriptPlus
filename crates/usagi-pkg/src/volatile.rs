//! Properties the engine saves but never uses as saved: it works them out
//! again, at run time or in the editor, before it reads them. Their saved
//! value is whatever it was when the package was saved, so it changes from
//! one save to the next even when nothing else does. [`diff`](crate::diff)
//! marks their changes as volatile.
//!
//! To add one, add a row to the table below: the class that declares the
//! property (its subclasses' objects have it too), the property, and why,
//! with where in the engine's source the value is worked out. Only add a
//! property with that evidence: one that merely looks unused may still be
//! read.

/// A property whose saved value the engine never uses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VolatileProperty {
    /// The class that declares it, `Package.Class`.
    pub class: &'static str,
    pub name: &'static str,
    /// Why its saved value doesn't matter.
    pub reason: &'static str,
}

macro_rules! volatile_properties {
    ($($package:ident . $class:ident . $name:ident => $reason:literal,)*) => {
        /// Every property whose saved value the engine never uses.
        pub const VOLATILE_PROPERTIES: &[VolatileProperty] = &[
            $(VolatileProperty {
                class: concat!(stringify!($package), ".", stringify!($class)),
                name: stringify!($name),
                reason: $reason,
            },)*
        ];
    };
}

volatile_properties! {
    // A texture modifier's matrix is set by its `GetMatrix`
    // (`Engine/Src/UnMaterial.cpp`) every time it's used: from the time, for
    // those that move, and from its other properties.
    Engine.TexOscillator.M => "set from the time by GetMatrix whenever it's used",
    Engine.TexPanner.M => "set from the time by GetMatrix whenever it's used",
    Engine.VariableTexPanner.M => "set from the time by GetMatrix whenever it's used",
    Engine.TexRotator.M => "set from the time by GetMatrix whenever it's used",
    Engine.TexScaler.M => "set from its scale and offset by GetMatrix whenever it's used",
}
