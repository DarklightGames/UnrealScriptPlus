//! Object references and patterns, written as in UnrealScript:
//! `Texture'MyPkg.Walls.Brick'`, or without the class, `MyPkg.Walls.Brick`.

use std::fmt;
use std::str::FromStr;

use crate::{ObjectRef, Package};

/// An object reference, or a glob matching several: a path with its
/// package, and optionally a class, as in `Texture'MyPkg.Walls.Brick'` or
/// `MyPkg.Walls.Brick`. Names are matched ignoring case, as the engine does.
///
/// In the path, `*` matches any characters within a name (not `.`), `**`
/// matches any characters, including `.`, and `?` matches one character
/// within a name. So `MyPkg.UI.*` matches the objects directly in the group
/// `UI`, and `MyPkg.UI.**` those in it and its groups.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectPattern {
    class: Option<String>,
    path: String,
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    /// A character, in lowercase.
    Char(char),
    /// `?`
    One,
    /// `*`
    Name,
    /// `**`
    Any,
}

impl ObjectPattern {
    /// The class, if one is given.
    pub fn class(&self) -> Option<&str> {
        self.class.as_deref()
    }

    /// The path, as written.
    pub fn path(&self) -> &str {
        &self.path
    }

    /// Whether the path has a wildcard, so it can match several objects.
    pub fn is_glob(&self) -> bool {
        self.tokens.iter().any(|t| !matches!(t, Token::Char(_)))
    }

    /// Whether `path` (`MyPkg.Walls.Brick`) matches the pattern's path.
    pub fn matches_path(&self, path: &str) -> bool {
        let text: Vec<char> = path.chars().flat_map(char::to_lowercase).collect();
        let tokens = &self.tokens;
        // matched[i][j]: tokens[i..] match text[j..].
        let mut matched = vec![vec![false; text.len() + 1]; tokens.len() + 1];
        matched[tokens.len()][text.len()] = true;
        for i in (0..tokens.len()).rev() {
            for j in (0..=text.len()).rev() {
                let next = text.get(j).copied();
                let in_name = next.is_some_and(|c| c != '.');
                matched[i][j] = match tokens[i] {
                    Token::Char(c) => next == Some(c) && matched[i + 1][j + 1],
                    Token::One => in_name && matched[i + 1][j + 1],
                    Token::Name => matched[i + 1][j] || (in_name && matched[i][j + 1]),
                    Token::Any => matched[i + 1][j] || (next.is_some() && matched[i][j + 1]),
                };
            }
        }
        matched[0][0]
    }

    /// Whether `class` is the pattern's class, ignoring case; true if the
    /// pattern has no class.
    pub fn matches_class(&self, class: &str) -> bool {
        self.class
            .as_deref()
            .is_none_or(|c| c.eq_ignore_ascii_case(class))
    }
}

impl FromStr for ObjectPattern {
    type Err = String;

    fn from_str(text: &str) -> Result<ObjectPattern, String> {
        let invalid = |reason: &str| format!("invalid object reference {:?}: {}", text, reason);
        let (class, path) = match text.split_once('\'') {
            None => (None, text),
            Some((class, rest)) => {
                let path = rest
                    .strip_suffix('\'')
                    .ok_or_else(|| invalid("it doesn't end with a '"))?;
                if class.is_empty() {
                    return Err(invalid("the class is missing before the '"));
                }
                if class.contains('.') {
                    return Err(invalid("the class is a name, without its package"));
                }
                (Some(class.to_string()), path)
            }
        };
        if path.contains('\'') {
            return Err(invalid("there's a ' in the path"));
        }
        if path.split('.').any(str::is_empty) {
            return Err(invalid("a name in the path is empty"));
        }
        let mut tokens = Vec::new();
        let mut chars = path.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '?' => tokens.push(Token::One),
                '*' if chars.peek() == Some(&'*') => {
                    while chars.next_if_eq(&'*').is_some() {}
                    tokens.push(Token::Any);
                }
                '*' => tokens.push(Token::Name),
                c => tokens.extend(c.to_lowercase().map(Token::Char)),
            }
        }
        Ok(ObjectPattern {
            class,
            path: path.to_string(),
            tokens,
        })
    }
}

impl fmt::Display for ObjectPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.class {
            Some(class) => write!(f, "{}'{}'", class, self.path),
            None => write!(f, "{}", self.path),
        }
    }
}

impl Package {
    /// The exports whose [`full_path`](Package::full_path) and class match
    /// `pattern`, in table order.
    pub fn find_exports(&self, pattern: &ObjectPattern) -> Vec<usize> {
        (0..self.exports().len())
            .filter(|&i| self.matches(pattern, ObjectRef::Export(i)))
            .collect()
    }

    /// The imports whose [`full_path`](Package::full_path) and class match
    /// `pattern`, in table order.
    pub fn find_imports(&self, pattern: &ObjectPattern) -> Vec<usize> {
        (0..self.imports().len())
            .filter(|&i| self.matches(pattern, ObjectRef::Import(i)))
            .collect()
    }

    /// Whether an export or import matches `pattern`.
    fn matches(&self, pattern: &ObjectPattern, object: ObjectRef) -> bool {
        pattern.matches_class(self.object_class_name(object).expect("not null"))
            && pattern.matches_path(&self.full_path(object).expect("not null"))
    }
}
