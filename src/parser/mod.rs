lalrpop_mod!(pub cheshire, "/parser/cheshire.rs");

use crate::{
    ast::{AstModule, AstProgram},
    lexer::Lexer,
    util::{FileId, FileRegistry, PError, PResult, Span},
};
use lalrpop_util::ParseError;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

pub fn parse_program(files: Vec<FileId>) -> PResult<AstProgram> {
    let mut modules = Vec::new();

    for f in files {
        modules.push(parse_module(f)?);
    }

    Ok(AstProgram { modules })
}

fn parse_module(file: FileId) -> PResult<AstModule> {
    let contents = FileRegistry::open(file)?;
    let name = FileRegistry::name(file);
    let lex = Lexer::new(file, &contents);

    cheshire::ModuleParser::new()
        .parse(file, &name, lex)
        .map_err(|e: ParseError<_, _, _>| match e {
            ParseError::InvalidToken { location } => PError::new_at(
                Span::new(file, location, location + 1),
                format!("Invalid token"),
            ),
            ParseError::UnrecognizedEOF { location, expected } => PError::new_at(
                Span::new(file, location, location + 1),
                format!(
                    "Unexpected token `<EOF>`. Expected `{}`.",
                    expected.join("`, `")
                ),
            ),
            ParseError::UnrecognizedToken {
                token: (l, t, h),
                expected,
            } => PError::new_at(
                Span::new(file, l, h),
                format!(
                    "Unexpected token `{}`. Expected `{}`.",
                    t,
                    expected.join("`, `")
                ),
            ),
            ParseError::ExtraToken { token: (l, t, h) } =>
                PError::new_at(Span::new(file, l, h), format!("Extra token `{}`.", t,)),
            ParseError::User { error } => error,
        })
}

pub fn append<T>(mut v: Vec<T>, t: T) -> Vec<T> {
    v.push(t);
    v
}

pub fn append_maybe<T>(mut v: Vec<T>, t: Option<T>) -> Vec<T> {
    match t {
        Some(t) => v.push(t),
        None => {},
    }

    v
}

pub fn combine<T>(mut v: Vec<T>, t: Vec<T>) -> Vec<T> {
    v.extend(t);

    v
}

pub fn dedup_set<K: Display + Eq + Ord, L, T>(
    where_at: &str,
    vec: Vec<K>,
) -> Result<BTreeSet<K>, ParseError<L, T, PError>> {
    let mut set = BTreeSet::new();

    for k in vec {
        if set.contains(&k) {
            return Err(ParseError::User {
                error: PError::new(format!("Duplicated entry `{}` in {}", k, where_at)),
            });
        }

        set.insert(k);
    }

    Ok(set)
}

pub fn dedup_keys<K: Display + Eq + Hash, V, L, T>(
    where_at: &str,
    vec: Vec<(K, V)>,
) -> Result<HashMap<K, V>, ParseError<L, T, PError>> {
    let mut map = HashMap::new();

    for (k, v) in vec {
        if map.contains_key(&k) {
            return Err(ParseError::User {
                error: PError::new(format!("Duplicated entry `{}` in {}", k, where_at)),
            });
        }

        map.insert(k, v);
    }

    Ok(map)
}

pub fn check_dedup_keys<'a, K: Clone + Display + Eq + Hash + 'a, L, T>(
    where_at: &str,
    vec: impl IntoIterator<Item = &'a K>,
) -> Result<(), ParseError<L, T, PError>> {
    let mut keys = HashSet::new();

    for k in vec {
        if keys.contains(k) {
            return Err(ParseError::User {
                error: PError::new(format!("Duplicated entry `{}` in {}", k, where_at)),
            });
        }

        keys.insert(k.clone());
    }

    Ok(())
}
