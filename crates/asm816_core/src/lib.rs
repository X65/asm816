use std::path::{Path, PathBuf};

pub mod asm;
pub mod diag;
pub mod encode;
pub mod expr;
pub mod fmt;
pub mod ir;
pub mod lex;
pub mod module_system;
pub mod parse;
pub mod source;

use asm::{pass1, pass2};
use diag::{Diag, Severity, has_errors, render_diags};
use fmt::format_program;
use lex::lex_file;
use module_system::build_program_from_entry;
use parse::parse_tokens;
use source::SourceManager;

pub use asm::CpuMode;
pub use asm::{Fixup, FixupKind, LineLayout, Pass1Result, SymKind, SymTab, SymValue, Width};
pub use diag::{DiagLabel, Severity as DiagSeverity};

#[derive(Clone, Debug)]
pub struct AssembleOptions {
    pub cpu_mode: CpuMode,
    pub include_dirs: Vec<PathBuf>,
}

#[derive(Clone, Debug)]
pub struct CheckOptions {
    pub cpu_mode: CpuMode,
    pub include_dirs: Vec<PathBuf>,
}

#[derive(Clone, Debug, Default)]
pub struct FmtOptions {}

#[derive(Clone, Debug, Default)]
pub struct PipelineWarnings {
    pub diags: Vec<Diag>,
}

pub fn assemble_path(
    input: &Path,
    out: &Path,
    opts: &AssembleOptions,
) -> Result<PipelineWarnings, Vec<Diag>> {
    let artifacts = compile_path(input, opts.cpu_mode, opts.include_dirs.clone(), true);
    let _ = render_diags(&artifacts.source_manager, &artifacts.diags);

    if has_errors(&artifacts.diags) {
        return Err(artifacts.diags);
    }

    if let Err(err) = std::fs::write(out, &artifacts.bytes) {
        let file = artifacts.entry_file;
        let span = 0..0;
        let io_diag = Diag::error(file, span, format!("failed to write output file: {err}"));
        let mut diags = artifacts.diags;
        diags.push(io_diag);
        let _ = render_diags(&artifacts.source_manager, &diags);
        return Err(diags);
    }

    Ok(PipelineWarnings {
        diags: only_warnings(&artifacts.diags),
    })
}

pub fn check_path(input: &Path, opts: &CheckOptions) -> Result<PipelineWarnings, Vec<Diag>> {
    let artifacts = compile_path(input, opts.cpu_mode, opts.include_dirs.clone(), true);
    let _ = render_diags(&artifacts.source_manager, &artifacts.diags);

    if has_errors(&artifacts.diags) {
        Err(artifacts.diags)
    } else {
        Ok(PipelineWarnings {
            diags: only_warnings(&artifacts.diags),
        })
    }
}

pub fn format_path(input: &Path, _opts: &FmtOptions) -> Result<String, Vec<Diag>> {
    let mut source_manager = SourceManager::new(Vec::new());
    let entry_file = match source_manager.load_path(input) {
        Ok(file) => file,
        Err(err) => {
            let file = source_manager.add_virtual_file(input.to_path_buf(), String::new());
            let diags = vec![Diag::error(
                file,
                0..0,
                format!("failed to read input file: {err}"),
            )];
            let _ = render_diags(&source_manager, &diags);
            return Err(diags);
        }
    };

    let (tokens, mut diags) = lex_file(&source_manager, entry_file);
    let (program, parse_diags) = parse_tokens(&tokens);
    diags.extend(parse_diags);

    let _ = render_diags(&source_manager, &diags);
    if has_errors(&diags) {
        return Err(diags);
    }

    Ok(format_program(&program))
}

pub fn compile_source_text(text: &str, cpu_mode: CpuMode) -> (Vec<u8>, Vec<Diag>) {
    let mut source_manager = SourceManager::new(Vec::new());
    let file = source_manager.add_virtual_file("inline.s", text.to_string());
    let (tokens, mut diags) = lex_file(&source_manager, file);
    let (program, parse_diags) = parse_tokens(&tokens);
    diags.extend(parse_diags);

    let pass1 = pass1(&program, cpu_mode);
    diags.extend(pass1.diags.clone());

    if !has_errors(&diags) {
        let (bytes, pass2_diags) = pass2(&program, &pass1);
        diags.extend(pass2_diags);
        return (bytes, diags);
    }

    (Vec::new(), diags)
}

struct CompileArtifacts {
    source_manager: SourceManager,
    entry_file: source::FileId,
    bytes: Vec<u8>,
    diags: Vec<Diag>,
}

fn compile_path(
    input: &Path,
    cpu_mode: CpuMode,
    include_dirs: Vec<PathBuf>,
    run_pass2: bool,
) -> CompileArtifacts {
    let mut source_manager = SourceManager::new(include_dirs);
    let module_build = build_program_from_entry(&mut source_manager, input);
    let entry_file = module_build.entry_file;
    let program = module_build.program;
    let mut diags = module_build.diags;

    let pass1 = pass1(&program, cpu_mode);
    diags.extend(pass1.diags.clone());

    let mut bytes = Vec::new();
    if run_pass2 && !has_errors(&diags) {
        let (encoded, pass2_diags) = pass2(&program, &pass1);
        bytes = encoded;
        diags.extend(pass2_diags);
    }

    CompileArtifacts {
        source_manager,
        entry_file,
        bytes,
        diags,
    }
}

fn only_warnings(diags: &[Diag]) -> Vec<Diag> {
    diags
        .iter()
        .filter(|diag| diag.severity != Severity::Error)
        .cloned()
        .collect()
}
