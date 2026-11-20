//! UnrealScript bytecode: the `Script` of a class, state or function, as
//! `UStruct::Serialize` and `UStruct::SerializeExpr` (`Core/Src/UnClass.cpp`)
//! store it.
//!
//! The stored size is the size of the code *in memory*, where object
//! references and names take 4 bytes; in the file they're compact indices,
//! of 1 to 5 bytes. So the code can't be skipped by its size: every
//! expression has to be read, as the engine does, to find where it ends.
//! Jump offsets are offsets in memory too.
//!
//! [`Expr`] is the code as `SerializeExpr` walks it, not as UnrealScript
//! means it: `EX_BoolVariable`, for example, has no operand here, since
//! `SerializeExpr` reads the variable after it as an expression of its own.

use crate::archive::{Archive, NameRef, Saver, Serialize};
use crate::{Error, ObjectRef, Result};

/// The tokens (`EExprToken`, `Core/Inc/UnStack.h`).
mod token {
    pub const LOCAL_VARIABLE: u8 = 0x00;
    pub const INSTANCE_VARIABLE: u8 = 0x01;
    pub const DEFAULT_VARIABLE: u8 = 0x02;
    pub const RETURN: u8 = 0x04;
    pub const SWITCH: u8 = 0x05;
    pub const JUMP: u8 = 0x06;
    pub const JUMP_IF_NOT: u8 = 0x07;
    pub const STOP: u8 = 0x08;
    pub const ASSERT: u8 = 0x09;
    pub const CASE: u8 = 0x0A;
    pub const NOTHING: u8 = 0x0B;
    pub const LABEL_TABLE: u8 = 0x0C;
    pub const GOTO_LABEL: u8 = 0x0D;
    pub const EAT_STRING: u8 = 0x0E;
    pub const LET: u8 = 0x0F;
    pub const DYN_ARRAY_ELEMENT: u8 = 0x10;
    pub const NEW: u8 = 0x11;
    pub const CLASS_CONTEXT: u8 = 0x12;
    pub const META_CAST: u8 = 0x13;
    pub const LET_BOOL: u8 = 0x14;
    pub const END_FUNCTION_PARMS: u8 = 0x16;
    pub const SELF: u8 = 0x17;
    pub const SKIP: u8 = 0x18;
    pub const CONTEXT: u8 = 0x19;
    pub const ARRAY_ELEMENT: u8 = 0x1A;
    pub const VIRTUAL_FUNCTION: u8 = 0x1B;
    pub const FINAL_FUNCTION: u8 = 0x1C;
    pub const INT_CONST: u8 = 0x1D;
    pub const FLOAT_CONST: u8 = 0x1E;
    pub const STRING_CONST: u8 = 0x1F;
    pub const OBJECT_CONST: u8 = 0x20;
    pub const NAME_CONST: u8 = 0x21;
    pub const ROTATION_CONST: u8 = 0x22;
    pub const VECTOR_CONST: u8 = 0x23;
    pub const BYTE_CONST: u8 = 0x24;
    pub const INT_ZERO: u8 = 0x25;
    pub const INT_ONE: u8 = 0x26;
    pub const TRUE: u8 = 0x27;
    pub const FALSE: u8 = 0x28;
    pub const NATIVE_PARM: u8 = 0x29;
    pub const NO_OBJECT: u8 = 0x2A;
    pub const INT_CONST_BYTE: u8 = 0x2C;
    pub const BOOL_VARIABLE: u8 = 0x2D;
    pub const DYNAMIC_CAST: u8 = 0x2E;
    pub const ITERATOR: u8 = 0x2F;
    pub const ITERATOR_POP: u8 = 0x30;
    pub const ITERATOR_NEXT: u8 = 0x31;
    pub const STRUCT_CMP_EQ: u8 = 0x32;
    pub const STRUCT_CMP_NE: u8 = 0x33;
    pub const UNICODE_STRING_CONST: u8 = 0x34;
    pub const STRUCT_MEMBER: u8 = 0x36;
    pub const DYN_ARRAY_LENGTH: u8 = 0x37;
    pub const GLOBAL_FUNCTION: u8 = 0x38;
    pub const PRIMITIVE_CAST: u8 = 0x39;
    pub const DYN_ARRAY_INSERT: u8 = 0x40;
    pub const DYN_ARRAY_REMOVE: u8 = 0x41;
    pub const DEBUG_INFO: u8 = 0x42;
    pub const DELEGATE_FUNCTION: u8 = 0x43;
    pub const DELEGATE_PROPERTY: u8 = 0x44;
    pub const LET_DELEGATE: u8 = 0x45;
    pub const END_OF_SCRIPT: u8 = 0x47;
    pub const EXTENDED_NATIVE: u8 = 0x60;
    pub const FIRST_NATIVE: u8 = 0x70;
}

/// The version of `EX_DebugInfo` that a function call takes as its own
/// (`HANDLE_OPTIONAL_DEBUG_INFO`).
const DEBUG_INFO_VERSION: i32 = 100;

/// A struct's bytecode: its size in memory, and its expressions.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Script {
    /// The size of the code in memory, as stored. When saving, it's worked
    /// out from the expressions.
    pub size: i32,
    pub exprs: Vec<Expr>,
}

impl Serialize for Script {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        if !ar.is_loading() {
            // The size in memory, from writing the code somewhere else.
            let mut scratch = Saver::new(ar.version(), ar.licensee_version());
            let mut code = Code {
                offset: 0,
                size: usize::MAX,
            };
            for expr in &mut self.exprs {
                expr.serialize_in(&mut scratch, &mut code)
                    .expect("saving can't fail");
            }
            self.size = i32::try_from(code.offset).expect("code smaller than 2 GiB");
        }
        let at = ar.pos();
        ar.i32(&mut self.size, "script size")?;
        let size = usize::try_from(self.size).map_err(|_| Error::InvalidData {
            offset: at,
            reason: format!("the script's size is {}", self.size),
        })?;
        let mut code = Code { offset: 0, size };
        if ar.is_loading() {
            self.exprs.clear();
        }
        // Loading: until the code's size is reached. Saving: every
        // expression, whose size is the code's.
        let mut i = 0;
        while if ar.is_loading() {
            code.offset < size
        } else {
            i < self.exprs.len()
        } {
            if ar.is_loading() {
                self.exprs.push(Expr::default());
            }
            self.exprs[i].serialize_in(ar, &mut code)?;
            i += 1;
        }
        if code.offset != size {
            return Err(Error::InvalidData {
                offset: ar.pos(),
                reason: format!(
                    "the script is {} bytes in memory, not {}",
                    code.offset, size
                ),
            });
        }
        Ok(())
    }
}

/// Where `SerializeExpr` has got to in the code in memory (`iCode`).
struct Code {
    offset: usize,
    /// The code's size in memory.
    size: usize,
}

/// An entry of a state's label table (`FLabelEntry`).
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Label {
    pub name: NameRef,
    /// Where the label is, in the code in memory.
    pub offset: i32,
}

/// A function call's arguments, and the debug information that may follow
/// it.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Call {
    pub args: Vec<Expr>,
    /// An `EX_DebugInfo` of version 100 right after the call, which
    /// `SerializeExpr` reads as part of it.
    pub debug_info: Option<Box<Expr>>,
}

/// An expression, as `SerializeExpr` reads it. The names are the tokens'
/// (`EX_LocalVariable` is [`Expr::LocalVariable`]); a native function call
/// is [`Expr::Native`].
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    LocalVariable(ObjectRef),
    InstanceVariable(ObjectRef),
    DefaultVariable(ObjectRef),
    Return(Box<Expr>),
    Switch {
        size: u8,
        expr: Box<Expr>,
    },
    Jump(u16),
    JumpIfNot {
        offset: u16,
        condition: Box<Expr>,
    },
    Stop,
    Assert {
        line: u16,
        condition: Box<Expr>,
    },
    /// A `case`, with its offset of the next one; `default:` (offset
    /// `0xFFFF`) has no value.
    Case {
        offset: u16,
        value: Option<Box<Expr>>,
    },
    Nothing,
    /// Ends with the entry named `None`, which is included.
    LabelTable(Vec<Label>),
    GotoLabel(Box<Expr>),
    EatString(Box<Expr>),
    Let {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetBool {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    LetDelegate {
        variable: Box<Expr>,
        value: Box<Expr>,
    },
    DynArrayElement {
        index: Box<Expr>,
        array: Box<Expr>,
    },
    New {
        outer: Box<Expr>,
        name: Box<Expr>,
        flags: Box<Expr>,
        class: Box<Expr>,
    },
    ClassContext(Context),
    MetaCast {
        class: ObjectRef,
        expr: Box<Expr>,
    },
    EndFunctionParms,
    SelfObject,
    Skip {
        size: u16,
        expr: Box<Expr>,
    },
    Context(Context),
    ArrayElement {
        index: Box<Expr>,
        array: Box<Expr>,
    },
    VirtualFunction {
        name: NameRef,
        call: Call,
    },
    FinalFunction {
        function: ObjectRef,
        call: Call,
    },
    IntConst(i32),
    FloatConst(f32),
    /// Latin-1, without the terminating null.
    StringConst(Vec<u8>),
    ObjectConst(ObjectRef),
    NameConst(NameRef),
    RotationConst {
        pitch: i32,
        yaw: i32,
        roll: i32,
    },
    VectorConst {
        x: f32,
        y: f32,
        z: f32,
    },
    ByteConst(u8),
    IntZero,
    IntOne,
    True,
    False,
    NativeParm(ObjectRef),
    NoObject,
    IntConstByte(u8),
    BoolVariable,
    DynamicCast {
        class: ObjectRef,
        expr: Box<Expr>,
    },
    Iterator {
        expr: Box<Expr>,
        offset: u16,
    },
    IteratorPop,
    IteratorNext,
    StructCmpEq {
        structure: ObjectRef,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    StructCmpNe {
        structure: ObjectRef,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// UTF-16, without the terminating null.
    UnicodeStringConst(Vec<u16>),
    StructMember {
        property: ObjectRef,
        expr: Box<Expr>,
    },
    DynArrayLength(Box<Expr>),
    GlobalFunction {
        name: NameRef,
        call: Call,
    },
    PrimitiveCast {
        cast: u8,
        expr: Box<Expr>,
    },
    DynArrayInsert {
        array: Box<Expr>,
        index: Box<Expr>,
        count: Box<Expr>,
    },
    DynArrayRemove {
        array: Box<Expr>,
        index: Box<Expr>,
        count: Box<Expr>,
    },
    DebugInfo {
        version: i32,
        line: i32,
        pos: i32,
        opcode: u8,
    },
    DelegateFunction {
        property: ObjectRef,
        name: NameRef,
    },
    DelegateProperty(NameRef),
    EndOfScript,
    /// A call to a native final function, by its number: 112 to 255 in one
    /// byte, otherwise in two (`EX_ExtendedNative`).
    Native {
        index: u16,
        call: Call,
    },
}

impl Default for Expr {
    fn default() -> Self {
        Expr::Nothing
    }
}

/// An object context (`EX_Context`, `EX_ClassContext`): `object.expr`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Context {
    pub object: Box<Expr>,
    /// Where to skip to if the object is `None`.
    pub skip: u16,
    /// The size of the result to zero if it's skipped.
    pub size: u8,
    pub expr: Box<Expr>,
}

impl Expr {
    /// The token that starts the expression. For a native call, its first
    /// byte.
    fn token(&self) -> u8 {
        use token::*;
        match self {
            Expr::LocalVariable(_) => LOCAL_VARIABLE,
            Expr::InstanceVariable(_) => INSTANCE_VARIABLE,
            Expr::DefaultVariable(_) => DEFAULT_VARIABLE,
            Expr::Return(_) => RETURN,
            Expr::Switch { .. } => SWITCH,
            Expr::Jump(_) => JUMP,
            Expr::JumpIfNot { .. } => JUMP_IF_NOT,
            Expr::Stop => STOP,
            Expr::Assert { .. } => ASSERT,
            Expr::Case { .. } => CASE,
            Expr::Nothing => NOTHING,
            Expr::LabelTable(_) => LABEL_TABLE,
            Expr::GotoLabel(_) => GOTO_LABEL,
            Expr::EatString(_) => EAT_STRING,
            Expr::Let { .. } => LET,
            Expr::LetBool { .. } => LET_BOOL,
            Expr::LetDelegate { .. } => LET_DELEGATE,
            Expr::DynArrayElement { .. } => DYN_ARRAY_ELEMENT,
            Expr::New { .. } => NEW,
            Expr::ClassContext(_) => CLASS_CONTEXT,
            Expr::MetaCast { .. } => META_CAST,
            Expr::EndFunctionParms => END_FUNCTION_PARMS,
            Expr::SelfObject => SELF,
            Expr::Skip { .. } => SKIP,
            Expr::Context(_) => CONTEXT,
            Expr::ArrayElement { .. } => ARRAY_ELEMENT,
            Expr::VirtualFunction { .. } => VIRTUAL_FUNCTION,
            Expr::FinalFunction { .. } => FINAL_FUNCTION,
            Expr::IntConst(_) => INT_CONST,
            Expr::FloatConst(_) => FLOAT_CONST,
            Expr::StringConst(_) => STRING_CONST,
            Expr::ObjectConst(_) => OBJECT_CONST,
            Expr::NameConst(_) => NAME_CONST,
            Expr::RotationConst { .. } => ROTATION_CONST,
            Expr::VectorConst { .. } => VECTOR_CONST,
            Expr::ByteConst(_) => BYTE_CONST,
            Expr::IntZero => INT_ZERO,
            Expr::IntOne => INT_ONE,
            Expr::True => TRUE,
            Expr::False => FALSE,
            Expr::NativeParm(_) => NATIVE_PARM,
            Expr::NoObject => NO_OBJECT,
            Expr::IntConstByte(_) => INT_CONST_BYTE,
            Expr::BoolVariable => BOOL_VARIABLE,
            Expr::DynamicCast { .. } => DYNAMIC_CAST,
            Expr::Iterator { .. } => ITERATOR,
            Expr::IteratorPop => ITERATOR_POP,
            Expr::IteratorNext => ITERATOR_NEXT,
            Expr::StructCmpEq { .. } => STRUCT_CMP_EQ,
            Expr::StructCmpNe { .. } => STRUCT_CMP_NE,
            Expr::UnicodeStringConst(_) => UNICODE_STRING_CONST,
            Expr::StructMember { .. } => STRUCT_MEMBER,
            Expr::DynArrayLength(_) => DYN_ARRAY_LENGTH,
            Expr::GlobalFunction { .. } => GLOBAL_FUNCTION,
            Expr::PrimitiveCast { .. } => PRIMITIVE_CAST,
            Expr::DynArrayInsert { .. } => DYN_ARRAY_INSERT,
            Expr::DynArrayRemove { .. } => DYN_ARRAY_REMOVE,
            Expr::DebugInfo { .. } => DEBUG_INFO,
            Expr::DelegateFunction { .. } => DELEGATE_FUNCTION,
            Expr::DelegateProperty(_) => DELEGATE_PROPERTY,
            Expr::EndOfScript => END_OF_SCRIPT,
            Expr::Native { index, .. } => match u8::try_from(*index) {
                Ok(index) if index >= FIRST_NATIVE => index,
                _ => EXTENDED_NATIVE + (index >> 8) as u8,
            },
        }
    }

    /// An expression of the kind `token` starts, with empty operands; `None`
    /// for a token the engine rejects (`Bad expr token`).
    fn empty(token: u8) -> Option<Expr> {
        use token::*;
        let e = Box::<Expr>::default;
        Some(match token {
            LOCAL_VARIABLE => Expr::LocalVariable(ObjectRef::Null),
            INSTANCE_VARIABLE => Expr::InstanceVariable(ObjectRef::Null),
            DEFAULT_VARIABLE => Expr::DefaultVariable(ObjectRef::Null),
            RETURN => Expr::Return(e()),
            SWITCH => Expr::Switch { size: 0, expr: e() },
            JUMP => Expr::Jump(0),
            JUMP_IF_NOT => Expr::JumpIfNot {
                offset: 0,
                condition: e(),
            },
            STOP => Expr::Stop,
            ASSERT => Expr::Assert {
                line: 0,
                condition: e(),
            },
            CASE => Expr::Case {
                offset: 0,
                value: None,
            },
            NOTHING => Expr::Nothing,
            LABEL_TABLE => Expr::LabelTable(Vec::new()),
            GOTO_LABEL => Expr::GotoLabel(e()),
            EAT_STRING => Expr::EatString(e()),
            LET => Expr::Let {
                variable: e(),
                value: e(),
            },
            LET_BOOL => Expr::LetBool {
                variable: e(),
                value: e(),
            },
            LET_DELEGATE => Expr::LetDelegate {
                variable: e(),
                value: e(),
            },
            DYN_ARRAY_ELEMENT => Expr::DynArrayElement {
                index: e(),
                array: e(),
            },
            NEW => Expr::New {
                outer: e(),
                name: e(),
                flags: e(),
                class: e(),
            },
            CLASS_CONTEXT => Expr::ClassContext(Context::default()),
            META_CAST => Expr::MetaCast {
                class: ObjectRef::Null,
                expr: e(),
            },
            END_FUNCTION_PARMS => Expr::EndFunctionParms,
            SELF => Expr::SelfObject,
            SKIP => Expr::Skip { size: 0, expr: e() },
            CONTEXT => Expr::Context(Context::default()),
            ARRAY_ELEMENT => Expr::ArrayElement {
                index: e(),
                array: e(),
            },
            VIRTUAL_FUNCTION => Expr::VirtualFunction {
                name: NameRef::default(),
                call: Call::default(),
            },
            FINAL_FUNCTION => Expr::FinalFunction {
                function: ObjectRef::Null,
                call: Call::default(),
            },
            INT_CONST => Expr::IntConst(0),
            FLOAT_CONST => Expr::FloatConst(0.0),
            STRING_CONST => Expr::StringConst(Vec::new()),
            OBJECT_CONST => Expr::ObjectConst(ObjectRef::Null),
            NAME_CONST => Expr::NameConst(NameRef::default()),
            ROTATION_CONST => Expr::RotationConst {
                pitch: 0,
                yaw: 0,
                roll: 0,
            },
            VECTOR_CONST => Expr::VectorConst {
                x: 0.0,
                y: 0.0,
                z: 0.0,
            },
            BYTE_CONST => Expr::ByteConst(0),
            INT_ZERO => Expr::IntZero,
            INT_ONE => Expr::IntOne,
            TRUE => Expr::True,
            FALSE => Expr::False,
            NATIVE_PARM => Expr::NativeParm(ObjectRef::Null),
            NO_OBJECT => Expr::NoObject,
            INT_CONST_BYTE => Expr::IntConstByte(0),
            BOOL_VARIABLE => Expr::BoolVariable,
            DYNAMIC_CAST => Expr::DynamicCast {
                class: ObjectRef::Null,
                expr: e(),
            },
            ITERATOR => Expr::Iterator {
                expr: e(),
                offset: 0,
            },
            ITERATOR_POP => Expr::IteratorPop,
            ITERATOR_NEXT => Expr::IteratorNext,
            STRUCT_CMP_EQ => Expr::StructCmpEq {
                structure: ObjectRef::Null,
                left: e(),
                right: e(),
            },
            STRUCT_CMP_NE => Expr::StructCmpNe {
                structure: ObjectRef::Null,
                left: e(),
                right: e(),
            },
            UNICODE_STRING_CONST => Expr::UnicodeStringConst(Vec::new()),
            STRUCT_MEMBER => Expr::StructMember {
                property: ObjectRef::Null,
                expr: e(),
            },
            DYN_ARRAY_LENGTH => Expr::DynArrayLength(e()),
            GLOBAL_FUNCTION => Expr::GlobalFunction {
                name: NameRef::default(),
                call: Call::default(),
            },
            PRIMITIVE_CAST => Expr::PrimitiveCast { cast: 0, expr: e() },
            DYN_ARRAY_INSERT => Expr::DynArrayInsert {
                array: e(),
                index: e(),
                count: e(),
            },
            DYN_ARRAY_REMOVE => Expr::DynArrayRemove {
                array: e(),
                index: e(),
                count: e(),
            },
            DEBUG_INFO => Expr::DebugInfo {
                version: 0,
                line: 0,
                pos: 0,
                opcode: 0,
            },
            DELEGATE_FUNCTION => Expr::DelegateFunction {
                property: ObjectRef::Null,
                name: NameRef::default(),
            },
            DELEGATE_PROPERTY => Expr::DelegateProperty(NameRef::default()),
            END_OF_SCRIPT => Expr::EndOfScript,
            t if t >= EXTENDED_NATIVE => Expr::Native {
                index: u16::from(t),
                call: Call::default(),
            },
            _ => return None,
        })
    }

    /// `SerializeExpr`: reads or writes the expression, moving
    /// `code.offset` on by its size in memory.
    fn serialize_in(&mut self, ar: &mut dyn Archive, code: &mut Code) -> Result<()> {
        let at = ar.pos();
        let mut token = if ar.is_loading() { 0 } else { self.token() };
        ar.u8(&mut token, "token")?;
        code.offset += 1;
        if ar.is_loading() {
            *self = Expr::empty(token).ok_or_else(|| Error::InvalidData {
                offset: at,
                reason: format!("bad token {:#04X} in the script", token),
            })?;
        }
        // The engine's XFER: a value of `memory` bytes in memory.
        macro_rules! xfer {
            ($method:ident, $value:expr, $memory:expr) => {{
                ar.$method($value, "script")?;
                code.offset += $memory;
            }};
        }
        match self {
            Expr::Native { index, call } => {
                if token < token::FIRST_NATIVE {
                    // EX_ExtendedNative: the high bits in the token, then a
                    // byte.
                    let mut low = *index as u8;
                    xfer!(u8, &mut low, 1);
                    *index = (u16::from(token - token::EXTENDED_NATIVE) << 8) | u16::from(low);
                }
                call.serialize_in(ar, code)?;
            }
            Expr::PrimitiveCast { cast, expr } => {
                xfer!(u8, cast, 1);
                expr.serialize_in(ar, code)?;
            }
            Expr::Let { variable, value }
            | Expr::LetBool { variable, value }
            | Expr::LetDelegate { variable, value } => {
                variable.serialize_in(ar, code)?;
                value.serialize_in(ar, code)?;
            }
            Expr::Jump(offset) => xfer!(u16, offset, 2),
            Expr::LocalVariable(property)
            | Expr::InstanceVariable(property)
            | Expr::DefaultVariable(property)
            | Expr::NativeParm(property)
            | Expr::ObjectConst(property) => xfer!(object, property, 4),
            Expr::DebugInfo {
                version,
                line,
                pos,
                opcode,
            } => {
                xfer!(i32, version, 4);
                xfer!(i32, line, 4);
                xfer!(i32, pos, 4);
                xfer!(u8, opcode, 1);
            }
            Expr::BoolVariable
            | Expr::Nothing
            | Expr::EndOfScript
            | Expr::EndFunctionParms
            | Expr::IntZero
            | Expr::IntOne
            | Expr::True
            | Expr::False
            | Expr::NoObject
            | Expr::SelfObject
            | Expr::IteratorPop
            | Expr::Stop
            | Expr::IteratorNext => {}
            Expr::EatString(expr)
            | Expr::Return(expr)
            | Expr::GotoLabel(expr)
            | Expr::DynArrayLength(expr) => expr.serialize_in(ar, code)?,
            Expr::FinalFunction { function, call } => {
                xfer!(object, function, 4);
                call.serialize_in(ar, code)?;
            }
            Expr::VirtualFunction { name, call } | Expr::GlobalFunction { name, call } => {
                xfer!(name, name, 4);
                call.serialize_in(ar, code)?;
            }
            Expr::DelegateFunction { property, name } => {
                xfer!(object, property, 4);
                xfer!(name, name, 4);
            }
            Expr::ClassContext(context) | Expr::Context(context) => {
                context.object.serialize_in(ar, code)?;
                xfer!(u16, &mut context.skip, 2);
                xfer!(u8, &mut context.size, 1);
                context.expr.serialize_in(ar, code)?;
            }
            Expr::ArrayElement { index, array } | Expr::DynArrayElement { index, array } => {
                index.serialize_in(ar, code)?;
                array.serialize_in(ar, code)?;
            }
            Expr::DynArrayInsert {
                array,
                index,
                count,
            }
            | Expr::DynArrayRemove {
                array,
                index,
                count,
            } => {
                array.serialize_in(ar, code)?;
                index.serialize_in(ar, code)?;
                count.serialize_in(ar, code)?;
            }
            Expr::New {
                outer,
                name,
                flags,
                class,
            } => {
                outer.serialize_in(ar, code)?;
                name.serialize_in(ar, code)?;
                flags.serialize_in(ar, code)?;
                class.serialize_in(ar, code)?;
            }
            Expr::IntConst(value) => xfer!(i32, value, 4),
            Expr::FloatConst(value) => xfer!(f32, value, 4),
            Expr::StringConst(text) => {
                if ar.is_loading() {
                    loop {
                        let mut c = 0;
                        xfer!(u8, &mut c, 1);
                        if c == 0 {
                            break;
                        }
                        text.push(c);
                    }
                } else {
                    for c in text.iter_mut().chain(&mut [0]) {
                        xfer!(u8, c, 1);
                    }
                }
            }
            Expr::UnicodeStringConst(text) => {
                if ar.is_loading() {
                    loop {
                        let mut c = 0;
                        xfer!(u16, &mut c, 2);
                        if c == 0 {
                            break;
                        }
                        text.push(c);
                    }
                } else {
                    for c in text.iter_mut().chain(&mut [0]) {
                        xfer!(u16, c, 2);
                    }
                }
            }
            Expr::NameConst(name) | Expr::DelegateProperty(name) => xfer!(name, name, 4),
            Expr::RotationConst { pitch, yaw, roll } => {
                xfer!(i32, pitch, 4);
                xfer!(i32, yaw, 4);
                xfer!(i32, roll, 4);
            }
            Expr::VectorConst { x, y, z } => {
                xfer!(f32, x, 4);
                xfer!(f32, y, 4);
                xfer!(f32, z, 4);
            }
            Expr::ByteConst(value) | Expr::IntConstByte(value) => xfer!(u8, value, 1),
            Expr::MetaCast { class, expr } | Expr::DynamicCast { class, expr } => {
                xfer!(object, class, 4);
                expr.serialize_in(ar, code)?;
            }
            Expr::JumpIfNot { offset, condition } => {
                xfer!(u16, offset, 2);
                condition.serialize_in(ar, code)?;
            }
            Expr::Iterator { expr, offset } => {
                expr.serialize_in(ar, code)?;
                xfer!(u16, offset, 2);
            }
            Expr::Switch { size, expr } => {
                xfer!(u8, size, 1);
                expr.serialize_in(ar, code)?;
            }
            Expr::Assert { line, condition } => {
                xfer!(u16, line, 2);
                condition.serialize_in(ar, code)?;
            }
            Expr::Case { offset, value } => {
                xfer!(u16, offset, 2);
                if *offset != 0xFFFF {
                    value
                        .get_or_insert_with(Box::default)
                        .serialize_in(ar, code)?;
                }
            }
            Expr::LabelTable(labels) => {
                if code.offset % 4 != 0 {
                    return Err(Error::InvalidData {
                        offset: ar.pos(),
                        reason: "a label table that isn't aligned to 4 bytes in memory".to_string(),
                    });
                }
                if ar.is_loading() {
                    loop {
                        let mut label = Label::default();
                        xfer!(name, &mut label.name, 4);
                        xfer!(i32, &mut label.offset, 4);
                        labels.push(label);
                        if ar.is_none_name(label.name.0) {
                            break;
                        }
                    }
                } else {
                    for label in labels {
                        xfer!(name, &mut label.name, 4);
                        xfer!(i32, &mut label.offset, 4);
                    }
                }
            }
            Expr::Skip { size, expr } => {
                xfer!(u16, size, 2);
                expr.serialize_in(ar, code)?;
            }
            Expr::StructCmpEq {
                structure,
                left,
                right,
            }
            | Expr::StructCmpNe {
                structure,
                left,
                right,
            } => {
                xfer!(object, structure, 4);
                left.serialize_in(ar, code)?;
                right.serialize_in(ar, code)?;
            }
            Expr::StructMember { property, expr } => {
                xfer!(object, property, 4);
                expr.serialize_in(ar, code)?;
            }
        }
        Ok(())
    }
}

impl Call {
    /// The arguments up to `EX_EndFunctionParms`, then the debug
    /// information that may follow (`HANDLE_OPTIONAL_DEBUG_INFO`).
    fn serialize_in(&mut self, ar: &mut dyn Archive, code: &mut Code) -> Result<()> {
        if ar.is_loading() {
            self.args.clear();
        }
        let mut i = 0;
        loop {
            // Saving: the arguments, then the end of the parameters.
            let mut arg = match self.args.get(i) {
                Some(arg) if !ar.is_loading() => arg.clone(),
                _ => Expr::EndFunctionParms,
            };
            arg.serialize_in(ar, code)?;
            if arg == Expr::EndFunctionParms {
                break;
            }
            if ar.is_loading() {
                self.args.push(arg);
            }
            i += 1;
        }
        let follows = if ar.is_loading() {
            code.offset < code.size
                && ar.peek(5).is_some_and(|next| {
                    next[0] == token::DEBUG_INFO
                        && i32::from_le_bytes(next[1..5].try_into().expect("4 bytes"))
                            == DEBUG_INFO_VERSION
                })
        } else {
            self.debug_info.is_some()
        };
        if follows {
            self.debug_info
                .get_or_insert_with(Box::default)
                .serialize_in(ar, code)?;
        }
        Ok(())
    }
}
