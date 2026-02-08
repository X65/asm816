use std::path::{Path, PathBuf};

use rustc_hash::FxHashMap;

pub type BytePos = usize;
pub type Span = std::ops::Range<BytePos>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub file: FileId,
    pub span: Span,
    pub value: T,
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
}

#[derive(Clone, Debug)]
pub struct SourceManager {
    files: Vec<SourceFile>,
    path_to_id: FxHashMap<PathBuf, FileId>,
    include_dirs: Vec<PathBuf>,
}

impl SourceManager {
    pub fn new(include_dirs: Vec<PathBuf>) -> Self {
        Self {
            files: Vec::new(),
            path_to_id: FxHashMap::default(),
            include_dirs,
        }
    }

    pub fn include_dirs(&self) -> &[PathBuf] {
        &self.include_dirs
    }

    pub fn load_path(&mut self, path: &Path) -> Result<FileId, std::io::Error> {
        let normalized = normalize_path(path);
        if let Some(id) = self.path_to_id.get(&normalized).copied() {
            return Ok(id);
        }

        let text = std::fs::read_to_string(path)?;
        let text = normalize_newlines(&text);
        Ok(self.insert_source(normalized, text))
    }

    pub fn add_virtual_file(
        &mut self,
        path: impl Into<PathBuf>,
        text: impl Into<String>,
    ) -> FileId {
        let path = path.into();
        let text = normalize_newlines(&text.into());
        self.insert_source(path, text)
    }

    pub fn file(&self, id: FileId) -> &SourceFile {
        &self.files[id.0 as usize]
    }

    pub fn files(&self) -> &[SourceFile] {
        &self.files
    }

    pub fn files_iter(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
        self.files
            .iter()
            .enumerate()
            .map(|(idx, file)| (FileId(idx as u32), file))
    }

    fn insert_source(&mut self, path: PathBuf, text: String) -> FileId {
        let id = FileId(self.files.len() as u32);
        self.path_to_id.insert(path.clone(), id);
        self.files.push(SourceFile { path, text });
        id
    }
}

fn normalize_path(path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(path)
    }
}

fn normalize_newlines(input: &str) -> String {
    input.replace("\r\n", "\n").replace('\r', "\n")
}
