use crate::{
    ast::{AstNamedVariable, VariableId},
    util::PError,
};
use lalrpop_util::ParseError;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

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

pub fn amend_variables(
    scope: Vec<AstNamedVariable>,
    map: &mut Option<HashMap<VariableId, AstNamedVariable>>,
) {
    let map = map.get_or_insert_with(|| HashMap::new());

    for s in scope {
        map.insert(s.id, s);
    }
}
