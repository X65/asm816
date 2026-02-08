use std::{
    fs,
    path::{Path, PathBuf},
};

use asm816_core::fmt::format_source;

#[test]
fn formatter_golden_fixtures_match_expectations() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fmt");
    let cases = discover_cases(&root);
    assert!(
        cases.len() >= 10,
        "expected at least 10 formatter cases under {} but found {}",
        root.display(),
        cases.len()
    );

    for case in cases {
        let input = fs::read_to_string(&case.input_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", case.input_path.display()));
        let expected = fs::read_to_string(&case.expected_path).unwrap_or_else(|err| {
            panic!("failed to read {}: {err}", case.expected_path.display())
        });

        let actual = format_source(&input, &case.input_path.display().to_string()).unwrap_or_else(
            |diags| panic!("format_source failed for `{}`: {diags:#?}", case.name),
        );
        let expected = normalize_newlines(&expected);

        assert_eq!(
            actual, expected,
            "formatter mismatch for `{}`\ninput: {}\nexpected: {}",
            case.name,
            case.input_path.display(),
            case.expected_path.display()
        );
    }
}

#[derive(Clone, Debug)]
struct FmtCase {
    name: String,
    input_path: PathBuf,
    expected_path: PathBuf,
}

fn discover_cases(root: &Path) -> Vec<FmtCase> {
    let mut case_dirs = fs::read_dir(root)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", root.display()))
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    case_dirs.sort();

    let mut cases = Vec::with_capacity(case_dirs.len());
    for case_dir in case_dirs {
        let name = case_dir
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_else(|| panic!("invalid formatter case path: {}", case_dir.display()))
            .to_string();
        let input_path = case_dir.join("input.s");
        let expected_path = case_dir.join("expected.s");

        assert!(
            input_path.is_file(),
            "formatter case `{name}` is missing {}",
            input_path.display()
        );
        assert!(
            expected_path.is_file(),
            "formatter case `{name}` is missing {}",
            expected_path.display()
        );

        cases.push(FmtCase {
            name,
            input_path,
            expected_path,
        });
    }

    cases
}

fn normalize_newlines(input: &str) -> String {
    input.replace("\r\n", "\n").replace('\r', "\n")
}
