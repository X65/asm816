use std::{fs, path::Path};

use asm816_core::{
    AssembleOptions, CheckOptions, CpuMode, assemble_path, check_path, diag::has_errors,
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

#[test]
fn supports_use_for_unqualified_symbol_import() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "use libs::vals::{ONE as ITEM}\n.org $8000\nLDA #ITEM\n",
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
fn supports_inner_module_import() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "mod util {\n    pub ONE = 1\n}\nimport main::util\n.org $8000\nLDA #main::util::ONE\n",
    );

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
fn rejects_inner_module_conflict_with_file_module() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "mod util {\n    pub ONE = 1\n}\n.org $8000\nNOP\n",
    );
    write_file(temp.path().join("main/util.asm"), "pub ONE = 2\n");

    let mut source_manager = SourceManager::new(Vec::new());
    let build = build_program_from_entry(&mut source_manager, &temp.path().join("main.asm"));
    assert!(has_errors(&build.diags), "diagnostics: {:#?}", build.diags);
}

#[test]
fn pub_macro_expands_in_caller_scope() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::mac\n.org $8000\nhelper:\nRTS\nlibs::mac::call_helper\n",
    );
    write_file(
        temp.path().join("libs/mac.asm"),
        "pub macro call_helper {\nJSR helper\n}\n",
    );

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
    assert_eq!(bytes, vec![0x60, 0x20, 0x00, 0x80]);
}

#[test]
fn pub_macro_missing_caller_dependency_errors() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::mac\n.org $8000\nlibs::mac::call_helper\n",
    );
    write_file(
        temp.path().join("libs/mac.asm"),
        "pub macro call_helper {\nJSR helper\n}\n",
    );

    let result = check_path(
        &temp.path().join("main.asm"),
        &CheckOptions {
            cpu_mode: CpuMode::default(),
            include_dirs: Vec::new(),
        },
    );
    assert!(result.is_err());
}

#[test]
fn macro_qualified_reference_requires_import_in_caller() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::mac\n.org $8000\nlibs::mac::emit_one\n",
    );
    write_file(
        temp.path().join("libs/mac.asm"),
        "pub macro emit_one {\nLDA #deps::vals::ONE\n}\n",
    );
    write_file(temp.path().join("deps/vals.asm"), "pub ONE = 1\n");

    let mut source_manager = SourceManager::new(Vec::new());
    let build = build_program_from_entry(&mut source_manager, &temp.path().join("main.asm"));
    assert!(has_errors(&build.diags), "diagnostics: {:#?}", build.diags);
}

#[test]
fn macro_qualified_reference_succeeds_with_import() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::mac\nimport deps::vals\n.org $8000\nlibs::mac::emit_one\n",
    );
    write_file(
        temp.path().join("libs/mac.asm"),
        "pub macro emit_one {\nLDA #deps::vals::ONE\n}\n",
    );
    write_file(temp.path().join("deps/vals.asm"), "pub ONE = 1\n");

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
fn macro_local_labels_are_gensymed_per_expansion() {
    let temp = tempdir().expect("create tempdir");
    write_file(
        temp.path().join("main.asm"),
        "import libs::mac\n.org $8000\nlibs::mac::spin\nlibs::mac::spin\n",
    );
    write_file(
        temp.path().join("libs/mac.asm"),
        "pub macro spin {\n%%loop:\nBNE %%loop\n}\n",
    );

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
    assert_eq!(bytes, vec![0xD0, 0xFE, 0xD0, 0xFE]);
}

fn write_file(path: impl AsRef<Path>, text: &str) {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent directories");
    }
    fs::write(path, text).expect("write test file");
}
