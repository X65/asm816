use std::{fs, path::Path};

use asm816_core::{
    AssembleOptions, CpuMode, assemble_path, diag::has_errors,
    module_system::build_program_from_entry, source::SourceManager,
};
use tempfile::tempdir;

#[test]
fn imports_pub_constant_by_module_path() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::vals\n.org $8000\nLDA #libs::vals::ONE\n",
    );
    write_file(temp.path().join("libs/vals.asm"), "pub ONE = 1\nTWO = 2\n");

    let out_path = temp.path().join("out.bin");
    let result = assemble_path(
        &temp.path().join("main.asm"),
        &out_path,
        &AssembleOptions {
            cpu_mode: CpuMode::default(),
            include_dirs: Vec::new(),
        },
    );
    assert!(result.is_ok());

    let bytes = fs::read(out_path).expect("read output bytes");
    assert_eq!(bytes, vec![0xA9, 0x01]);
}

#[test]
fn imports_pub_constant_by_alias() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::vals as v\n.org $8000\nLDA #v::ONE\n",
    );
    write_file(temp.path().join("libs/vals.asm"), "pub ONE = 1\n");

    let out_path = temp.path().join("out.bin");
    let result = assemble_path(
        &temp.path().join("main.asm"),
        &out_path,
        &AssembleOptions {
            cpu_mode: CpuMode::default(),
            include_dirs: Vec::new(),
        },
    );
    assert!(result.is_ok());

    let bytes = fs::read(out_path).expect("read output bytes");
    assert_eq!(bytes, vec![0xA9, 0x01]);
}

#[test]
fn rejects_non_exported_cross_module_symbol() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::vals\n.org $8000\nLDA #libs::vals::TWO\n",
    );
    write_file(temp.path().join("libs/vals.asm"), "pub ONE = 1\nTWO = 2\n");

    let mut source_manager = SourceManager::new(Vec::new());
    let build = build_program_from_entry(&mut source_manager, &temp.path().join("main.asm"));
    assert!(has_errors(&build.diags), "diagnostics: {:#?}", build.diags);
}

#[test]
fn rejects_missing_import_for_qualified_lookup() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        ".org $8000\nLDA #libs::vals::ONE\n",
    );
    write_file(temp.path().join("libs/vals.asm"), "pub ONE = 1\n");

    let mut source_manager = SourceManager::new(Vec::new());
    let build = build_program_from_entry(&mut source_manager, &temp.path().join("main.asm"));
    assert!(has_errors(&build.diags), "diagnostics: {:#?}", build.diags);
}

#[test]
fn rejects_module_header_mismatch() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::vals\n.org $8000\nLDA #libs::vals::ONE\n",
    );
    write_file(
        temp.path().join("libs/vals.asm"),
        "module libs::wrong\npub ONE = 1\n",
    );

    let mut source_manager = SourceManager::new(Vec::new());
    let build = build_program_from_entry(&mut source_manager, &temp.path().join("main.asm"));
    assert!(has_errors(&build.diags), "diagnostics: {:#?}", build.diags);
}

fn write_file(path: impl AsRef<Path>, text: &str) {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent directories");
    }
    fs::write(path, text).expect("write test file");
}
