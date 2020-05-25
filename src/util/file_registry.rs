use crate::util::result::PResult;

use super::PError;
use crate::util::Span;
use std::{
    collections::{HashMap, HashSet},
    ffi::{OsStr, OsString},
    fs,
    io::Write,
    path::{Path, PathBuf},
    sync::{Arc, RwLock, Weak},
};
use tempfile::NamedTempFile;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub usize);

pub struct FileRegistry {
    file_ids: HashMap<PathBuf, FileId>,
    paths: HashMap<FileId, PathBuf>,
    mod_paths: HashMap<FileId, Vec<String>>,

    files: HashMap<FileId, Weak<str>>,
    temporary_files: Vec<NamedTempFile>,
}

lazy_static! {
    static ref FILE_ID_COUNTER: RwLock<usize> = RwLock::new(1);
    static ref FILE_REGISTRY: RwLock<FileRegistry> = RwLock::new(FileRegistry {
        file_ids: HashMap::new(),
        paths: HashMap::new(),
        files: HashMap::new(),
        mod_paths: HashMap::new(),
        temporary_files: vec![]
    });
    static ref EXTENSIONS: HashSet<OsString> = {
        let mut x = HashSet::new();
        x.insert(OsString::from("C"));
        x.insert(OsString::from("cs"));
        x.insert(OsString::from("ch"));
        x.insert(OsString::from("cheshire"));
        x
    };
}

impl FileRegistry {
    pub fn seek_path(path: &Path) -> PResult<Vec<FileId>> {
        let files;
        let mod_name = Self::verify_mod_name(path.file_stem().unwrap())?;

        if path.is_file() {
            files = vec![(path.into(), vec![mod_name])];
        } else if path.is_dir() && path.ends_with("src") {
            files = Self::seek_directory(path, &mut vec![])?;
        } else if path.is_dir() {
            files = Self::seek_directory(path, &mut vec![mod_name])?;
        } else {
            return perror!("Unknown file type: {:?}", path);
        }

        Ok(files
            .into_iter()
            .map(|(f, m)| Self::seek_module(f, m))
            .collect())
    }

    fn verify_mod_name(name: &OsStr) -> PResult<String> {
        let mut s = String::new();

        for b in name.to_str().unwrap().bytes() {
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => s.push(b as char),
                b'0'..=b'9' if !s.is_empty() => s.push(b as char),
                _ => {
                    return perror!("Bad file name: {:?}", name);
                },
            }
        }

        Ok(s)
    }

    fn seek_directory(path: &Path, mod_path: &Vec<String>) -> PResult<Vec<(PathBuf, Vec<String>)>> {
        let mut children = vec![];

        for child in fs::read_dir(path).unwrap() {
            let child = child.unwrap();
            let ty = child.file_type().unwrap();
            let path = child.path();
            let mut mod_path = mod_path.clone();
            mod_path.push(Self::verify_mod_name(path.file_stem().unwrap())?);

            if ty.is_dir() {
                // TODO: disallow keywords

                children.extend(Self::seek_directory(&path, &mod_path)?);
            } else if ty.is_file() {
                if path.extension().is_none() || !EXTENSIONS.contains(path.extension().unwrap()) {
                    continue;
                }

                // Pop the name "mod(.cheshire)"...
                if mod_path.last().unwrap() == "mod" {
                    if mod_path.len() == 1 {
                        return perror!(
                            "Illegal file! Can't have a file with name `src/mod.ch`, since \
                             according to the rules of module translation, that leads to an empty \
                             module path!"
                        );
                    }

                    mod_path.pop();
                }

                children.push((path, mod_path));
            }
        }

        info!(
            "Seeked directory {:?} with {:?}, got {:?}",
            path, mod_path, children
        );

        Ok(children)
    }

    fn seek_module(path: PathBuf, module_path: Vec<String>) -> FileId {
        let mut reg = FILE_REGISTRY.write().unwrap();

        if reg.file_ids.contains_key(&path) {
            reg.file_ids[&path]
        } else {
            let mut id_ref = FILE_ID_COUNTER.write().unwrap();
            *id_ref += 1;

            let new_id = FileId(id_ref.clone());
            reg.file_ids.insert(path.clone(), new_id);
            reg.paths.insert(new_id, path);
            reg.mod_paths.insert(new_id, module_path);

            new_id
        }
    }

    pub fn open(file_id: FileId) -> PResult<Arc<str>> {
        let mut reg = FILE_REGISTRY.write().unwrap();

        if reg.files.contains_key(&file_id) {
            if let Some(file) = reg.files[&file_id].upgrade() {
                return Ok(file);
            }
        }

        let path = &reg.paths[&file_id];

        if let Ok(file_contents) = std::fs::read_to_string(path) {
            let file_contents: Arc<str> = file_contents.into();
            let weak = Arc::downgrade(&file_contents);
            reg.files.insert(file_id, weak);

            Ok(file_contents)
        } else {
            perror!("Error reading file content")
        }
    }

    pub fn path(file_id: FileId) -> PathBuf {
        let reg = FILE_REGISTRY.write().unwrap();
        reg.paths[&file_id].clone()
    }

    pub fn mod_path(file_id: FileId) -> Vec<String> {
        let reg = FILE_REGISTRY.write().unwrap();
        reg.mod_paths[&file_id].clone()
    }

    /// Gets the mod_path of the folder containing the given file.
    ///
    /// For `mod.ch` files, this is the same as the file's own
    /// mod_path.
    pub fn parent_mod_path(file_id: FileId) -> Vec<String> {
        let mut mod_path = Self::mod_path(file_id);

        // mod_path of `x/mod.ch` is already `x`, so we only need
        // to pop the last part of the path off if it's not a mod file.
        if Self::path(file_id).file_stem().unwrap() != "mod" {
            mod_path.pop();
        }

        mod_path
    }

    pub fn name(file_id: FileId) -> String {
        let reg = FILE_REGISTRY.write().unwrap();
        reg.mod_paths[&file_id].last().unwrap().clone()
    }

    pub fn register_temporary(from: &str, file: NamedTempFile) -> FileId {
        let id = FileRegistry::seek_module(file.path().into(), vec![from.to_string()]);

        let mut reg = FILE_REGISTRY.write().unwrap();
        reg.temporary_files.push(file);

        id
    }

    pub fn register_formatted_string(
        from: &str,
        contents: std::fmt::Arguments<'_>,
    ) -> PResult<FileId> {
        let mut file = NamedTempFile::new()
            .map_err(|e| PError::new(format!("Error creating temporary file: {}", e)))?;
        file.write_fmt(contents)
            .map_err(|e| PError::new(format!("Error writing to temporary file: {}", e)))?;

        Ok(Self::register_temporary(from, file))
    }

    pub fn location_info(span: Span) -> PResult<(String, usize, usize, usize, usize, String)> {
        let file = Self::open(span.file)?;
        let (row, col) = Self::get_row_col(&file, span.start);
        let (end_row, end_col) = Self::get_row_col(&file, span.end);
        // TODO: this is technically unsafe...
        let name = Self::path(span.file).to_str().unwrap().to_owned();
        let line = Self::get_line_from_pos(&file, span.start);

        Ok((name, row + 1, col + 1, end_row + 1, end_col + 1, line))
    }

    fn get_row_col(file_contents: &str, pos: usize) -> (usize, usize) {
        let mut row = 0;
        let mut col = 0;

        for c in file_contents[0..pos].chars() {
            if c == '\n' {
                row += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        (row, col)
    }

    fn get_line_from_pos(file_contents: &str, pos: usize) -> String {
        if pos > file_contents.len() {
            return "<EOF>".to_string();
        }

        let (line, _) = Self::get_row_col(file_contents, pos);
        file_contents
            .lines()
            .nth(line)
            .unwrap_or_else(|| "<EOF>")
            .to_string()
    }
}
