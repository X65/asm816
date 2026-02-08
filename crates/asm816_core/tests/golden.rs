use std::{fs, path::PathBuf};

use asm816_core::{CpuMode, compile_source_text, diag::has_errors};

#[test]
fn golden_fixtures_match_expected_bytes() {
    for case_dir in discover_case_dirs() {
        let case_name = case_dir
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        let input_path = case_dir.join("input.s");
        let expected_path = case_dir.join("expected.bin");

        let input = fs::read_to_string(&input_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", input_path.display()));
        let expected = fs::read(&expected_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", expected_path.display()));

        let (actual, diags) = compile_source_text(&input, CpuMode::default());
        assert!(
            !has_errors(&diags),
            "assembly diagnostics for `{case_name}`: {diags:#?}"
        );

        if actual != expected {
            panic!(
                "{}",
                format_mismatch_report(case_name, &input_path, &expected_path, &expected, &actual)
            );
        }
    }
}

fn discover_case_dirs() -> Vec<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut dirs = fs::read_dir(&root)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", root.display()))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    dirs.sort();
    dirs
}

fn format_mismatch_report(
    case_name: &str,
    input_path: &PathBuf,
    expected_path: &PathBuf,
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
