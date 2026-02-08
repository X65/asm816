use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
};

use asm816_core::{
    CpuMode,
    asm::{pass1, pass2},
    diag::{Diag, has_errors, render_diags_to_string},
    lex::lex_file,
    parse::parse_tokens,
    source::SourceManager,
};

#[test]
fn golden_fixtures_match_expectations() {
    let update_snapshots = std::env::var_os("ASM816_UPDATE_GOLDEN").is_some();
    for case in discover_cases() {
        let case_name = &case.name;
        let input_path = &case.input_path;
        let expected_path = case.expected_bin_path.as_ref();
        let expect_error_path = case.expected_err_path.as_ref();

        let input = fs::read_to_string(&input_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", input_path.display()));
        let expected = expected_path.map(|path| {
            fs::read(path).unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()))
        });
        let expected_error_snapshot = expect_error_path.map(|path| {
            fs::read_to_string(path)
                .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()))
        });
        let expect_error = expected_error_snapshot.is_some();

        let inline_path = input_path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        let (actual, diags, source_manager) =
            compile_source_text_with_manager(&inline_path, &input, CpuMode::default());
        assert_eq!(
            has_errors(&diags),
            expect_error,
            "diagnostic expectation mismatch for `{case_name}`\ninput: {}\nexpect_error: {expect_error}\ndiags: {diags:#?}",
            input_path.display(),
        );
        if let Some(expected_error_snapshot) = expected_error_snapshot {
            let actual_error_snapshot = render_diags_to_string(&source_manager, &diags)
                .unwrap_or_else(|err| {
                    panic!("failed to render diagnostics snapshot for `{case_name}`: {err}")
                });
            assert_or_update_error_snapshot(
                case_name,
                expect_error_path.expect("error snapshot path"),
                &expected_error_snapshot,
                &actual_error_snapshot,
                update_snapshots,
            );
        }

        match expected {
            Some(expected) => {
                if actual != expected {
                    panic!(
                        "{}",
                        format_mismatch_report(
                            case_name,
                            &input_path,
                            expected_path.expect("expected bytes path"),
                            &expected,
                            &actual
                        )
                    );
                }
            }
            None => {}
        }
    }
}

#[derive(Clone, Debug)]
struct GoldenCase {
    name: String,
    input_path: PathBuf,
    expected_bin_path: Option<PathBuf>,
    expected_err_path: Option<PathBuf>,
}

fn discover_cases() -> Vec<GoldenCase> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut stems = fs::read_dir(&root)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", root.display()))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter_map(|path| {
            if path.is_dir() {
                panic!(
                    "golden fixtures must be flat files; found directory {}",
                    path.display()
                );
            }
            if path.file_name().and_then(|name| name.to_str()) == Some("README.md") {
                return None;
            }

            let ext = path.extension().and_then(|ext| ext.to_str());
            match ext {
                Some("s") => Some(stem_name(&path)),
                Some("bin") | Some("err") => None,
                Some(other) => panic!(
                    "unsupported file extension `{other}` in golden fixtures: {}",
                    path.display()
                ),
                None => panic!("unsupported golden fixture file: {}", path.display()),
            }
        })
        .collect::<Vec<_>>();
    stems.sort();
    stems.dedup();
    assert!(
        !stems.is_empty(),
        "no golden fixture source files (*.s) found under {}",
        root.display()
    );

    let source_stems = stems.iter().cloned().collect::<BTreeSet<_>>();
    for ext in ["bin", "err"] {
        for path in fs::read_dir(&root)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", root.display()))
            .filter_map(|entry| entry.ok().map(|entry| entry.path()))
            .filter(|path| path.extension().and_then(|item| item.to_str()) == Some(ext))
        {
            let stem = stem_name(&path);
            assert!(
                source_stems.contains(&stem),
                "found {} without matching {}: {}",
                path.display(),
                format!("{stem}.s"),
                root.display()
            );
        }
    }

    stems
        .into_iter()
        .map(|name| {
            let input_path = root.join(format!("{name}.s"));
            let expected_bin_path = root.join(format!("{name}.bin"));
            let expected_err_path = root.join(format!("{name}.err"));
            let has_bin = expected_bin_path.exists();
            let has_err = expected_err_path.exists();
            assert!(
                !(has_bin && has_err),
                "fixture `{name}` has both {} and {}; this is not allowed",
                expected_bin_path.display(),
                expected_err_path.display()
            );
            assert!(
                has_bin || has_err,
                "fixture `{name}` must provide either {} or {}",
                expected_bin_path.display(),
                expected_err_path.display()
            );

            GoldenCase {
                name,
                input_path,
                expected_bin_path: has_bin.then_some(expected_bin_path),
                expected_err_path: has_err.then_some(expected_err_path),
            }
        })
        .collect()
}

fn stem_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or_else(|| panic!("invalid fixture filename: {}", path.display()))
        .to_string()
}

fn compile_source_text_with_manager(
    source_path: &str,
    text: &str,
    cpu_mode: CpuMode,
) -> (Vec<u8>, Vec<Diag>, SourceManager) {
    let mut source_manager = SourceManager::new(Vec::new());
    let file = source_manager.add_virtual_file(source_path, text.to_string());

    let (tokens, mut diags) = lex_file(&source_manager, file);
    let (program, parse_diags) = parse_tokens(&tokens);
    diags.extend(parse_diags);

    let pass1 = pass1(&program, cpu_mode);
    diags.extend(pass1.diags.clone());

    let mut bytes = Vec::new();
    if !has_errors(&diags) {
        let (encoded, pass2_diags) = pass2(&program, &pass1);
        bytes = encoded;
        diags.extend(pass2_diags);
    }

    (bytes, diags, source_manager)
}

fn assert_or_update_error_snapshot(
    case_name: &str,
    expect_error_path: &Path,
    expected_snapshot: &str,
    actual_snapshot: &str,
    update_snapshots: bool,
) {
    let expected_normalized = normalize_newlines(expected_snapshot);
    let actual_normalized = normalize_newlines(actual_snapshot);
    if expected_normalized == actual_normalized {
        return;
    }

    if update_snapshots {
        fs::write(expect_error_path, &actual_normalized).unwrap_or_else(|err| {
            panic!(
                "failed to update {} for `{case_name}`: {err}",
                expect_error_path.display()
            )
        });
        return;
    }

    let diff_at = first_diff(expected_normalized.as_bytes(), actual_normalized.as_bytes())
        .unwrap_or(expected_normalized.len().min(actual_normalized.len()));
    panic!(
        "error snapshot mismatch for `{case_name}`\nsnapshot: {}\nfirst_diff: {diff_at}\nexpected_len: {}\nactual_len: {}\nset ASM816_UPDATE_GOLDEN=1 to refresh snapshots",
        expect_error_path.display(),
        expected_normalized.len(),
        actual_normalized.len(),
    );
}

fn format_mismatch_report(
    case_name: &str,
    input_path: &Path,
    expected_path: &Path,
    expected: &[u8],
    actual: &[u8],
) -> String {
    let offset = first_diff(expected, actual).unwrap_or(expected.len().min(actual.len()));
    let expected_ctx = hex_context(expected, offset, 16);
    let actual_ctx = hex_context(actual, offset, 16);

    format!(
        "golden mismatch for `{case_name}`\ninput: {}\nexpected: {}\nfirst_diff: {offset}\nexpected_len: {}\nactual_len: {}\nexpected_ctx: {expected_ctx}\nactual_ctx:   {actual_ctx}",
        input_path.display(),
        expected_path.display(),
        expected.len(),
        actual.len(),
    )
}

fn first_diff(expected: &[u8], actual: &[u8]) -> Option<usize> {
    let shared = expected.len().min(actual.len());
    for idx in 0..shared {
        if expected[idx] != actual[idx] {
            return Some(idx);
        }
    }
    if expected.len() != actual.len() {
        Some(shared)
    } else {
        None
    }
}

fn hex_context(bytes: &[u8], center: usize, radius: usize) -> String {
    let start = center.saturating_sub(radius);
    let end = bytes.len().min(center.saturating_add(radius));
    bytes[start..end]
        .iter()
        .enumerate()
        .map(|(i, b)| format!("{:04X}:{:02X}", start + i, b))
        .collect::<Vec<_>>()
        .join(" ")
}

fn normalize_newlines(text: &str) -> String {
    text.replace("\r\n", "\n").replace('\r', "\n")
}
