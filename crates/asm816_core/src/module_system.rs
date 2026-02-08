use std::{
    fs,
    path::{Path, PathBuf},
};

use indexmap::IndexMap;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    diag::{Diag, DiagLabel},
    expr::Expr,
    ir::{DirArg, Item, Operand, Program},
    lex::{Token, TokenKind, lex_file},
    parse::parse_tokens,
    source::{FileId, SourceManager, Span},
};

const MAX_MACRO_EXPANSION_DEPTH: usize = 32;

#[derive(Clone, Debug)]
pub struct ModuleBuild {
    pub entry_file: FileId,
    pub program: Program,
    pub diags: Vec<Diag>,
}

#[derive(Clone, Debug)]
struct ImportDecl {
    module_path: String,
    alias: Option<String>,
    file: FileId,
    span: Span,
}

#[derive(Clone, Debug)]
struct UseDecl {
    module_path: String,
    items: Vec<UseItem>,
    file: FileId,
    span: Span,
}

#[derive(Clone, Debug)]
struct UseItem {
    source: String,
    alias: String,
    span: Span,
}

#[derive(Clone, Debug)]
struct MacroDef {
    module_path: String,
    name: String,
    is_pub: bool,
    body_tokens: Vec<Token>,
    required_prefixes: FxHashSet<String>,
    file: FileId,
    span: Span,
}

#[derive(Clone, Debug)]
struct UseBinding {
    module_path: String,
    symbol: String,
    file: FileId,
    span: Span,
}

#[derive(Clone, Debug)]
struct ModuleUnit {
    file: FileId,
    file_path: PathBuf,
    module_path: String,
    imports: Vec<ImportDecl>,
    uses: Vec<UseDecl>,
    imported_paths: FxHashSet<String>,
    aliases: FxHashMap<String, String>,
    use_symbol_bindings: FxHashMap<String, UseBinding>,
    use_macro_bindings: FxHashMap<String, UseBinding>,
    exports: FxHashSet<String>,
    macro_exports: FxHashSet<String>,
    macros: FxHashMap<String, MacroDef>,
    filtered_tokens: Vec<Token>,
}

#[derive(Clone, Debug)]
struct Discovery {
    by_module: FxHashMap<String, Vec<PathBuf>>,
}

#[derive(Clone, Debug)]
struct MacroInvocation {
    def: MacroDef,
    call: Token,
}

pub fn build_program_from_entry(source_manager: &mut SourceManager, input: &Path) -> ModuleBuild {
    let entry_file = match source_manager.load_path(input) {
        Ok(file) => file,
        Err(err) => {
            let file = source_manager.add_virtual_file(input.to_path_buf(), String::new());
            return ModuleBuild {
                entry_file: file,
                program: Program::default(),
                diags: vec![Diag::error(
                    file,
                    0..0,
                    format!("failed to read input file: {err}"),
                )],
            };
        }
    };

    let entry_path = source_manager.file(entry_file).path.clone();
    let roots = module_roots(&entry_path, source_manager.include_dirs());
    let discovery = discover_file_modules(&roots);

    let mut diags = collect_discovery_duplicate_diags(&discovery);
    let mut units = IndexMap::<String, ModuleUnit>::new();

    let Some(entry_module_path) =
        canonical_module_path(&entry_path, &roots, entry_file, &mut diags)
    else {
        return ModuleBuild {
            entry_file,
            program: Program::default(),
            diags,
        };
    };

    load_module_file(
        source_manager,
        entry_file,
        Some(&entry_module_path),
        &roots,
        &discovery,
        &mut units,
        &mut diags,
    );

    let exports_by_module = units
        .iter()
        .map(|(path, unit)| (path.clone(), unit.exports.clone()))
        .collect::<FxHashMap<_, _>>();
    let macro_exports_by_module = units
        .iter()
        .map(|(path, unit)| (path.clone(), unit.macro_exports.clone()))
        .collect::<FxHashMap<_, _>>();
    let macros_by_module = units
        .iter()
        .map(|(path, unit)| (path.clone(), unit.macros.clone()))
        .collect::<FxHashMap<_, _>>();

    resolve_module_bindings(
        &mut units,
        &exports_by_module,
        &macro_exports_by_module,
        &mut diags,
    );

    let compile_order = compile_order_from_entry(&entry_module_path, &units);

    let mut program = Program::default();
    for module_path in compile_order {
        let Some(unit) = units.get(&module_path) else {
            continue;
        };

        let expanded_tokens = expand_macros_for_unit(unit, &macros_by_module, &mut diags);
        let (mut module_program, parse_diags) = parse_tokens(&expanded_tokens);
        diags.extend(parse_diags);

        rewrite_module_symbols(unit, &mut module_program, &exports_by_module, &mut diags);

        program.items.extend(module_program.items);
    }

    ModuleBuild {
        entry_file,
        program,
        diags,
    }
}

fn collect_discovery_duplicate_diags(discovery: &Discovery) -> Vec<Diag> {
    let mut diags = Vec::new();
    for (module_path, files) in &discovery.by_module {
        if files.len() < 2 {
            continue;
        }

        let mut diag = Diag::error(
            FileId(0),
            0..0,
            format!(
                "ambiguous module definition `{module_path}` across roots: {}",
                files
                    .iter()
                    .map(|p| p.display().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        )
        .with_help("remove duplicates or use a single module root set");

        for file in files.iter().skip(1) {
            diag = diag.with_label(DiagLabel {
                file: FileId(0),
                span: 0..0,
                message: format!("also found at `{}`", file.display()),
            });
        }

        diags.push(diag);
    }

    diags
}

fn discover_file_modules(roots: &[PathBuf]) -> Discovery {
    let mut by_module: FxHashMap<String, Vec<PathBuf>> = FxHashMap::default();
    for root in roots {
        if !root.is_dir() {
            continue;
        }
        let mut stack = vec![root.clone()];
        while let Some(dir) = stack.pop() {
            let Ok(entries) = fs::read_dir(&dir) else {
                continue;
            };

            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                    continue;
                }
                if path.extension().and_then(|ext| ext.to_str()) != Some("asm") {
                    continue;
                }

                if let Ok(relative) = path.strip_prefix(root) {
                    if let Some(module_path) = module_path_from_relative_path(relative) {
                        by_module.entry(module_path).or_default().push(path.clone());
                    }
                }
            }
        }
    }

    for files in by_module.values_mut() {
        files.sort();
        files.dedup();
    }

    Discovery { by_module }
}

fn load_module_by_path(
    source_manager: &mut SourceManager,
    module_path: &str,
    roots: &[PathBuf],
    discovery: &Discovery,
    units: &mut IndexMap<String, ModuleUnit>,
    diags: &mut Vec<Diag>,
    caller: Option<(FileId, Span)>,
) {
    if units.contains_key(module_path) {
        return;
    }

    let Some(path) = resolve_module_source_path(module_path, discovery, caller.clone(), diags)
    else {
        if let Some(ancestor) = nearest_existing_file_module(module_path, discovery) {
            load_module_by_path(
                source_manager,
                &ancestor,
                roots,
                discovery,
                units,
                diags,
                caller.clone(),
            );
            if units.contains_key(module_path) {
                return;
            }
        }

        if !units.contains_key(module_path) {
            let (file, span) = caller.unwrap_or((FileId(0), 0..0));
            diags.push(
                Diag::error(file, span, format!("module not found: `{module_path}`")).with_help(
                    format!(
                        "expected file `{}`",
                        module_path_to_relative_file(module_path)
                    ),
                ),
            );
        }
        return;
    };

    match source_manager.load_path(&path) {
        Ok(file_id) => {
            load_module_file(
                source_manager,
                file_id,
                Some(module_path),
                roots,
                discovery,
                units,
                diags,
            );
            if !units.contains_key(module_path) {
                let (file, span) = caller.unwrap_or((FileId(0), 0..0));
                diags.push(
                    Diag::error(
                        file,
                        span,
                        format!(
                            "module `{module_path}` was not defined in `{}`",
                            path.display()
                        ),
                    )
                    .with_help("define an inner `mod` block matching the requested module path"),
                );
            }
        }
        Err(err) => {
            let (file, span) = caller.unwrap_or((FileId(0), 0..0));
            diags.push(Diag::error(
                file,
                span,
                format!("failed to load `{}`: {err}", path.display()),
            ));
        }
    }
}

fn resolve_module_source_path(
    module_path: &str,
    discovery: &Discovery,
    caller: Option<(FileId, Span)>,
    diags: &mut Vec<Diag>,
) -> Option<PathBuf> {
    let files = discovery.by_module.get(module_path)?;
    if files.len() == 1 {
        return files.first().cloned();
    }

    let (file, span) = caller.unwrap_or((FileId(0), 0..0));
    diags.push(
        Diag::error(
            file,
            span,
            format!(
                "ambiguous module import `{module_path}`; candidates: {}",
                files
                    .iter()
                    .map(|p| p.display().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        )
        .with_help("remove duplicate module files across roots"),
    );
    None
}

fn nearest_existing_file_module(module_path: &str, discovery: &Discovery) -> Option<String> {
    let segments = module_path.split("::").collect::<Vec<_>>();
    if segments.len() < 2 {
        return None;
    }

    for end in (1..segments.len()).rev() {
        let candidate = segments[..end].join("::");
        if discovery.by_module.contains_key(&candidate) {
            return Some(candidate);
        }
    }

    None
}

fn load_module_file(
    source_manager: &mut SourceManager,
    file: FileId,
    expected_path: Option<&str>,
    roots: &[PathBuf],
    discovery: &Discovery,
    units: &mut IndexMap<String, ModuleUnit>,
    diags: &mut Vec<Diag>,
) {
    let file_path = source_manager.file(file).path.clone();
    let Some(module_path) = canonical_module_path(&file_path, roots, file, diags) else {
        return;
    };

    if let Some(expected) = expected_path {
        if expected != module_path {
            diags.push(Diag::error(
                file,
                0..0,
                format!("module file resolves to `{module_path}` but was imported as `{expected}`"),
            ));
        }
    }

    if let Some(existing) = units.get(&module_path) {
        if existing.file_path != file_path {
            diags.push(
                Diag::error(
                    file,
                    0..0,
                    format!("duplicate module definition `{module_path}`"),
                )
                .with_label(DiagLabel {
                    file: existing.file,
                    span: 0..0,
                    message: "previous definition here".to_string(),
                }),
            );
        }
        return;
    }

    let (tokens, lex_diags) = lex_file(source_manager, file);
    diags.extend(lex_diags);

    let analyzed = analyze_module_region(
        file,
        &file_path,
        &module_path,
        &tokens,
        true,
        &discovery.by_module,
        diags,
    );

    let mut inserted = Vec::new();
    for unit in analyzed {
        if let Some(existing) = units.get(&unit.module_path) {
            diags.push(
                Diag::error(
                    unit.file,
                    0..0,
                    format!("duplicate module definition `{}`", unit.module_path),
                )
                .with_label(DiagLabel {
                    file: existing.file,
                    span: 0..0,
                    message: "previous definition here".to_string(),
                }),
            );
            continue;
        }

        inserted.push(unit.module_path.clone());
        units.insert(unit.module_path.clone(), unit);
    }

    for module in inserted {
        let imports = units
            .get(&module)
            .map(|u| u.imports.clone())
            .unwrap_or_default();
        let uses = units
            .get(&module)
            .map(|u| u.uses.clone())
            .unwrap_or_default();

        for import in imports {
            load_module_by_path(
                source_manager,
                &import.module_path,
                roots,
                discovery,
                units,
                diags,
                Some((import.file, import.span.clone())),
            );
        }

        for use_decl in uses {
            load_module_by_path(
                source_manager,
                &use_decl.module_path,
                roots,
                discovery,
                units,
                diags,
                Some((use_decl.file, use_decl.span.clone())),
            );
        }
    }
}

fn analyze_module_region(
    file: FileId,
    file_path: &Path,
    module_path: &str,
    tokens: &[Token],
    is_file_root: bool,
    file_modules: &FxHashMap<String, Vec<PathBuf>>,
    diags: &mut Vec<Diag>,
) -> Vec<ModuleUnit> {
    let mut imports = Vec::new();
    let mut uses = Vec::new();
    let mut exports = FxHashSet::default();
    let mut macro_exports = FxHashSet::default();
    let mut macros = FxHashMap::default();
    let mut filtered_tokens = Vec::new();
    let mut nested_units = Vec::new();

    let mut saw_non_empty_line = false;
    let mut idx = 0usize;

    while idx < tokens.len() {
        let line_start = idx;
        while idx < tokens.len() && tokens[idx].kind != TokenKind::Newline {
            idx += 1;
        }
        let line_end = idx;

        let newline = if idx < tokens.len() {
            let token = tokens[idx].clone();
            idx += 1;
            Some(token)
        } else {
            None
        };

        let line_tokens = &tokens[line_start..line_end];
        if line_tokens.is_empty() {
            if let Some(newline) = newline {
                filtered_tokens.push(newline);
            }
            continue;
        }

        if token_is_ident(&line_tokens[0], "module") {
            if !is_file_root {
                diags.push(Diag::error(
                    file,
                    line_tokens[0].span.clone(),
                    "`module` header is only allowed at file module root",
                ));
            } else if saw_non_empty_line {
                diags.push(Diag::error(
                    file,
                    line_tokens[0].span.clone(),
                    "`module` header must appear before any other non-empty line",
                ));
            }

            match parse_module_path_tokens(file, &line_tokens[1..]) {
                Ok((header, span)) => {
                    if header != module_path {
                        diags.push(Diag::error(
                            file,
                            span,
                            format!(
                                "module header `{header}` does not match canonical path `{module_path}`"
                            ),
                        ));
                    }
                }
                Err(diag) => diags.push(diag),
            }

            saw_non_empty_line = true;
            if let Some(newline) = newline {
                filtered_tokens.push(newline);
            }
            continue;
        }

        if let Some((is_pub, name, brace_rel)) = parse_macro_decl_line(file, line_tokens, diags) {
            let open_brace = line_start + brace_rel;
            let Some(close_brace) = find_matching_brace(tokens, open_brace) else {
                diags.push(Diag::error(
                    file,
                    line_tokens[brace_rel].span.clone(),
                    "unclosed macro body; missing `}`",
                ));
                continue;
            };

            let body_tokens = tokens[open_brace + 1..close_brace].to_vec();
            let span = line_tokens[0].span.start..tokens[close_brace].span.end;
            let def = MacroDef {
                module_path: module_path.to_string(),
                name: name.clone(),
                is_pub,
                required_prefixes: extract_qualified_prefixes(&body_tokens),
                body_tokens,
                file,
                span: span.clone(),
            };

            if macros.insert(name.clone(), def).is_some() {
                diags.push(Diag::error(
                    file,
                    span,
                    format!("macro `{name}` is already defined in `{module_path}`"),
                ));
            }

            if is_pub {
                macro_exports.insert(name);
            }

            saw_non_empty_line = true;
            idx = close_brace + 1;
            if idx < tokens.len() && tokens[idx].kind == TokenKind::Newline {
                filtered_tokens.push(tokens[idx].clone());
                idx += 1;
            }
            continue;
        }

        if let Some((name, brace_rel)) = parse_mod_decl_line(file, line_tokens, diags) {
            if !is_valid_module_segment(&name) {
                diags.push(Diag::error(
                    file,
                    line_tokens[1].span.clone(),
                    format!("invalid module segment `{name}`; expected lowercase snake_case"),
                ));
                continue;
            }

            let inner_path = format!("{module_path}::{name}");
            if let Some(files) = file_modules.get(&inner_path) {
                if files.iter().any(|path| path != file_path) {
                    diags.push(
                        Diag::error(
                            file,
                            line_tokens[0].span.start..line_tokens[line_tokens.len() - 1].span.end,
                            format!(
                                "duplicate module definition `{inner_path}` (inner module conflicts with file module)"
                            ),
                        )
                        .with_help(format!(
                            "remove either `mod {name} {{ ... }}` or `{}`",
                            module_path_to_relative_file(&inner_path)
                        )),
                    );
                }
            }

            let open_brace = line_start + brace_rel;
            let Some(close_brace) = find_matching_brace(tokens, open_brace) else {
                diags.push(Diag::error(
                    file,
                    line_tokens[brace_rel].span.clone(),
                    "unclosed inner module body; missing `}`",
                ));
                continue;
            };

            let inner_tokens = tokens[open_brace + 1..close_brace].to_vec();
            let mut children = analyze_module_region(
                file,
                file_path,
                &inner_path,
                &inner_tokens,
                false,
                file_modules,
                diags,
            );
            nested_units.append(&mut children);

            saw_non_empty_line = true;
            idx = close_brace + 1;
            if idx < tokens.len() && tokens[idx].kind == TokenKind::Newline {
                filtered_tokens.push(tokens[idx].clone());
                idx += 1;
            }
            continue;
        }

        if token_is_ident(&line_tokens[0], "import") {
            match parse_import_line(file, line_tokens) {
                Ok(import) => imports.push(import),
                Err(diag) => diags.push(diag),
            }
            saw_non_empty_line = true;
            if let Some(newline) = newline {
                filtered_tokens.push(newline);
            }
            continue;
        }

        if token_is_ident(&line_tokens[0], "use") {
            match parse_use_line(file, line_tokens) {
                Ok(use_decl) => uses.push(use_decl),
                Err(diag) => diags.push(diag),
            }
            saw_non_empty_line = true;
            if let Some(newline) = newline {
                filtered_tokens.push(newline);
            }
            continue;
        }

        saw_non_empty_line = true;

        if token_is_ident(&line_tokens[0], "pub") {
            if line_tokens.len() < 2 {
                diags.push(Diag::error(
                    file,
                    line_tokens[0].span.clone(),
                    "`pub` must be followed by a declaration",
                ));
                if let Some(newline) = newline {
                    filtered_tokens.push(newline);
                }
                continue;
            }

            if token_is_ident(&line_tokens[1], "macro") {
                // handled above by parse_macro_decl_line
                if let Some(newline) = newline {
                    filtered_tokens.push(newline);
                }
                continue;
            }

            if let Some(name) = parse_pub_export_name(&line_tokens[1..]) {
                exports.insert(name.to_string());
                filtered_tokens.extend(line_tokens[1..].iter().cloned());
            } else {
                diags.push(
                    Diag::error(
                        file,
                        line_tokens[0].span.start..line_tokens[line_tokens.len() - 1].span.end,
                        "`pub` currently supports labels, assignments, and macros",
                    )
                    .with_help("use `pub NAME:`, `pub NAME = expr`, or `pub macro NAME { ... }`"),
                );
            }

            if let Some(newline) = newline {
                filtered_tokens.push(newline);
            }
            continue;
        }

        filtered_tokens.extend(line_tokens.iter().cloned());
        if let Some(newline) = newline {
            filtered_tokens.push(newline);
        }
    }

    let mut out = Vec::with_capacity(1 + nested_units.len());
    out.push(ModuleUnit {
        file,
        file_path: file_path.to_path_buf(),
        module_path: module_path.to_string(),
        imports,
        uses,
        imported_paths: FxHashSet::default(),
        aliases: FxHashMap::default(),
        use_symbol_bindings: FxHashMap::default(),
        use_macro_bindings: FxHashMap::default(),
        exports,
        macro_exports,
        macros,
        filtered_tokens,
    });
    out.extend(nested_units);
    out
}

fn parse_macro_decl_line(
    file: FileId,
    line: &[Token],
    diags: &mut Vec<Diag>,
) -> Option<(bool, String, usize)> {
    if line.is_empty() {
        return None;
    }

    let (is_pub, start) = if token_is_ident(&line[0], "pub") {
        (true, 1usize)
    } else {
        (false, 0usize)
    };

    if start >= line.len() || !token_is_ident(&line[start], "macro") {
        return None;
    }

    if start + 2 >= line.len() {
        diags.push(Diag::error(
            file,
            line[start].span.clone(),
            "expected `macro NAME {`",
        ));
        return None;
    }

    if line[start + 1].kind != TokenKind::Ident {
        diags.push(Diag::error(
            file,
            line[start + 1].span.clone(),
            "expected macro name after `macro`",
        ));
        return None;
    }

    let name = line[start + 1].lexeme.clone();
    if line[start + 2].kind != TokenKind::LBrace {
        diags.push(Diag::error(
            file,
            line[start + 2].span.clone(),
            "expected `{` to start macro body",
        ));
        return None;
    }

    if start + 3 < line.len() {
        diags.push(Diag::error(
            file,
            line[start + 3].span.clone(),
            "unexpected tokens after macro opening brace",
        ));
    }

    Some((is_pub, name, start + 2))
}

fn parse_mod_decl_line(
    file: FileId,
    line: &[Token],
    diags: &mut Vec<Diag>,
) -> Option<(String, usize)> {
    if line.len() < 3 || !token_is_ident(&line[0], "mod") {
        return None;
    }

    if line[1].kind != TokenKind::Ident {
        diags.push(Diag::error(
            file,
            line[1].span.clone(),
            "expected module name after `mod`",
        ));
        return None;
    }

    if line[2].kind != TokenKind::LBrace {
        diags.push(Diag::error(
            file,
            line[2].span.clone(),
            "expected `{` to start inner module body",
        ));
        return None;
    }

    if line.len() > 3 {
        diags.push(Diag::error(
            file,
            line[3].span.clone(),
            "unexpected tokens after inner module opening brace",
        ));
    }

    Some((line[1].lexeme.clone(), 2))
}

fn find_matching_brace(tokens: &[Token], open_brace: usize) -> Option<usize> {
    if tokens.get(open_brace)?.kind != TokenKind::LBrace {
        return None;
    }

    let mut depth = 1usize;
    let mut idx = open_brace + 1;
    while idx < tokens.len() {
        match tokens[idx].kind {
            TokenKind::LBrace => depth += 1,
            TokenKind::RBrace => {
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
        idx += 1;
    }

    None
}

fn parse_import_line(file: FileId, line_tokens: &[Token]) -> Result<ImportDecl, Diag> {
    debug_assert!(!line_tokens.is_empty());
    let mut idx = 1usize;
    let (module_path, consumed, path_span) = parse_module_path_at(file, line_tokens, idx)?;
    idx = consumed;

    let alias = if idx < line_tokens.len() {
        if !token_is_ident(&line_tokens[idx], "as") {
            return Err(Diag::error(
                file,
                line_tokens[idx].span.clone(),
                "expected `as` after import module path",
            ));
        }
        idx += 1;
        if idx >= line_tokens.len() || line_tokens[idx].kind != TokenKind::Ident {
            return Err(Diag::error(
                file,
                line_tokens[line_tokens.len() - 1].span.clone(),
                "expected alias identifier after `as`",
            ));
        }
        let alias = line_tokens[idx].lexeme.clone();
        if !is_valid_module_segment(&alias) {
            return Err(Diag::error(
                file,
                line_tokens[idx].span.clone(),
                format!("invalid alias `{alias}`; expected lowercase snake_case"),
            ));
        }
        idx += 1;
        Some(alias)
    } else {
        None
    };

    if idx != line_tokens.len() {
        return Err(Diag::error(
            file,
            line_tokens[idx].span.clone(),
            "unexpected tokens at end of import declaration",
        ));
    }

    Ok(ImportDecl {
        module_path,
        alias,
        file,
        span: path_span,
    })
}

fn parse_use_line(file: FileId, line_tokens: &[Token]) -> Result<UseDecl, Diag> {
    if line_tokens.len() < 6 {
        return Err(Diag::error(
            file,
            line_tokens[0].span.clone(),
            "expected `use module::path::{name, ...}`",
        ));
    }

    let mut split = None;
    for idx in 1..line_tokens.len().saturating_sub(1) {
        if line_tokens[idx].kind == TokenKind::DColon
            && line_tokens[idx + 1].kind == TokenKind::LBrace
        {
            split = Some(idx);
            break;
        }
    }

    let Some(split_idx) = split else {
        return Err(Diag::error(
            file,
            line_tokens[0].span.clone(),
            "expected `::{...}` in use declaration",
        ));
    };

    let (module_path, consumed, _span) = parse_module_path_at(file, &line_tokens[..split_idx], 1)?;
    if consumed != split_idx {
        return Err(Diag::error(
            file,
            line_tokens[consumed].span.clone(),
            "unexpected tokens in `use` module path",
        ));
    }

    if line_tokens[line_tokens.len() - 1].kind != TokenKind::RBrace {
        return Err(Diag::error(
            file,
            line_tokens[line_tokens.len() - 1].span.clone(),
            "expected `}` to close use list",
        ));
    }

    let mut items = Vec::new();
    let mut idx = split_idx + 2;
    while idx < line_tokens.len() - 1 {
        if line_tokens[idx].kind != TokenKind::Ident {
            return Err(Diag::error(
                file,
                line_tokens[idx].span.clone(),
                "expected imported symbol name",
            ));
        }

        let source = line_tokens[idx].lexeme.clone();
        let mut alias = source.clone();
        let mut end_span = line_tokens[idx].span.clone();
        idx += 1;

        if idx < line_tokens.len() - 1 && token_is_ident(&line_tokens[idx], "as") {
            idx += 1;
            if idx >= line_tokens.len() - 1 || line_tokens[idx].kind != TokenKind::Ident {
                return Err(Diag::error(
                    file,
                    line_tokens[idx.saturating_sub(1)].span.clone(),
                    "expected alias name after `as`",
                ));
            }
            alias = line_tokens[idx].lexeme.clone();
            end_span = line_tokens[idx].span.clone();
            idx += 1;
        }

        items.push(UseItem {
            source,
            alias,
            span: end_span,
        });

        if idx < line_tokens.len() - 1 {
            if line_tokens[idx].kind != TokenKind::Comma {
                return Err(Diag::error(
                    file,
                    line_tokens[idx].span.clone(),
                    "expected `,` between use imports",
                ));
            }
            idx += 1;
        }
    }

    if items.is_empty() {
        return Err(Diag::error(
            file,
            line_tokens[split_idx].span.clone(),
            "`use` list cannot be empty",
        ));
    }

    Ok(UseDecl {
        module_path,
        items,
        file,
        span: line_tokens[0].span.start..line_tokens[line_tokens.len() - 1].span.end,
    })
}

fn parse_module_path_tokens(file: FileId, tokens: &[Token]) -> Result<(String, Span), Diag> {
    let (path, consumed, span) = parse_module_path_at(file, tokens, 0)?;
    if consumed != tokens.len() {
        return Err(Diag::error(
            file,
            tokens[consumed].span.clone(),
            "unexpected tokens after module path",
        ));
    }
    Ok((path, span))
}

fn parse_module_path_at(
    file: FileId,
    tokens: &[Token],
    start: usize,
) -> Result<(String, usize, Span), Diag> {
    if start >= tokens.len() || tokens[start].kind != TokenKind::Ident {
        let span = tokens
            .get(start)
            .map(|token| token.span.clone())
            .unwrap_or(0..0);
        return Err(Diag::error(file, span, "expected module path"));
    }

    let mut segments = vec![tokens[start].lexeme.clone()];
    let mut idx = start + 1;
    while idx + 1 < tokens.len() && tokens[idx].kind == TokenKind::DColon {
        if tokens[idx + 1].kind != TokenKind::Ident {
            return Err(Diag::error(
                file,
                tokens[idx + 1].span.clone(),
                "expected module path segment after `::`",
            ));
        }
        segments.push(tokens[idx + 1].lexeme.clone());
        idx += 2;
    }

    for (seg_idx, segment) in segments.iter().enumerate() {
        if !is_valid_module_segment(segment) {
            let span = tokens[start + seg_idx * 2].span.clone();
            return Err(Diag::error(
                file,
                span,
                format!("invalid module segment `{segment}`; expected lowercase snake_case"),
            ));
        }
    }

    let span = tokens[start].span.start..tokens[idx - 1].span.end;
    Ok((segments.join("::"), idx, span))
}

fn parse_pub_export_name(tokens: &[Token]) -> Option<&str> {
    if tokens.len() < 2 || tokens[0].kind != TokenKind::Ident {
        return None;
    }

    if tokens[1].kind == TokenKind::Colon || tokens[1].kind == TokenKind::Eq {
        Some(tokens[0].lexeme.as_str())
    } else {
        None
    }
}

fn token_is_ident(token: &Token, keyword: &str) -> bool {
    token.kind == TokenKind::Ident && token.lexeme.eq_ignore_ascii_case(keyword)
}

fn canonical_module_path(
    file_path: &Path,
    roots: &[PathBuf],
    file: FileId,
    diags: &mut Vec<Diag>,
) -> Option<String> {
    let relative = roots
        .iter()
        .find_map(|root| file_path.strip_prefix(root).ok())
        .map(Path::to_path_buf);

    let Some(relative) = relative else {
        diags.push(Diag::error(
            file,
            0..0,
            format!(
                "cannot derive module path for `{}` from configured module roots",
                file_path.display()
            ),
        ));
        return None;
    };

    let Some(module_path) = module_path_from_relative_path(&relative) else {
        diags.push(Diag::error(
            file,
            0..0,
            format!(
                "module files must use `.asm` extension: `{}`",
                file_path.display()
            ),
        ));
        return None;
    };

    for segment in module_path.split("::") {
        if !is_valid_module_segment(segment) {
            diags.push(Diag::error(
                file,
                0..0,
                format!("invalid module segment `{segment}`; expected lowercase snake_case"),
            ));
            return None;
        }
    }

    Some(module_path)
}

fn module_path_from_relative_path(relative: &Path) -> Option<String> {
    if relative.extension().and_then(|ext| ext.to_str()) != Some("asm") {
        return None;
    }

    let mut segments = Vec::new();
    for component in relative.iter() {
        let raw = component.to_str()?;
        segments.push(raw.to_string());
    }

    let stem = Path::new(segments.last()?)
        .file_stem()?
        .to_str()?
        .to_string();
    let last = segments.len().saturating_sub(1);
    segments[last] = stem;

    Some(segments.join("::"))
}

fn module_path_to_relative_file(module_path: &str) -> String {
    format!("{}.asm", module_path.replace("::", "/"))
}

fn module_roots(entry_path: &Path, include_dirs: &[PathBuf]) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(parent) = entry_path.parent() {
        roots.push(normalize_path(parent));
    }

    for include_dir in include_dirs {
        roots.push(normalize_path(include_dir));
    }

    roots.sort();
    roots.dedup();
    roots
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

fn is_valid_module_segment(segment: &str) -> bool {
    let mut chars = segment.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !first.is_ascii_lowercase() {
        return false;
    }

    chars.all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_')
}

fn resolve_module_bindings(
    units: &mut IndexMap<String, ModuleUnit>,
    exports_by_module: &FxHashMap<String, FxHashSet<String>>,
    macro_exports_by_module: &FxHashMap<String, FxHashSet<String>>,
    diags: &mut Vec<Diag>,
) {
    let module_names = units.keys().cloned().collect::<FxHashSet<_>>();

    for unit in units.values_mut() {
        let mut imported_paths = FxHashSet::default();
        let mut aliases = FxHashMap::default();

        for import in &unit.imports {
            imported_paths.insert(import.module_path.clone());

            if !module_names.contains(&import.module_path) {
                diags.push(
                    Diag::error(
                        import.file,
                        import.span.clone(),
                        format!("module not found: `{}`", import.module_path),
                    )
                    .with_help(format!(
                        "expected file `{}`",
                        module_path_to_relative_file(&import.module_path)
                    )),
                );
            }

            if let Some(alias) = &import.alias {
                if let Some(previous) = aliases.insert(alias.clone(), import.module_path.clone()) {
                    diags.push(
                        Diag::error(
                            import.file,
                            import.span.clone(),
                            format!("import alias `{alias}` is already used for `{previous}`"),
                        )
                        .with_help("choose a unique alias in this module"),
                    );
                }
            }
        }

        unit.imported_paths = imported_paths;
        unit.aliases = aliases;

        let mut symbol_bindings = FxHashMap::default();
        let mut macro_bindings = FxHashMap::default();

        for use_decl in &unit.uses {
            let target = &use_decl.module_path;
            let Some(target_exports) = exports_by_module.get(target) else {
                diags.push(
                    Diag::error(
                        use_decl.file,
                        use_decl.span.clone(),
                        format!("module not found in `use`: `{target}`"),
                    )
                    .with_help(format!(
                        "add an `import {target}` or ensure `{}` exists",
                        module_path_to_relative_file(target)
                    )),
                );
                continue;
            };

            let target_macro_exports = macro_exports_by_module
                .get(target)
                .cloned()
                .unwrap_or_default();

            for item in &use_decl.items {
                let alias = item.alias.clone();
                let is_symbol_export = target_exports.contains(&item.source);
                let is_macro_export = target_macro_exports.contains(&item.source);

                if !is_symbol_export && !is_macro_export {
                    diags.push(
                        Diag::error(
                            use_decl.file,
                            item.span.clone(),
                            format!("`{}` is not exported by module `{target}`", item.source),
                        )
                        .with_help("mark the item as `pub` in the exporting module"),
                    );
                    continue;
                }

                let binding = UseBinding {
                    module_path: target.clone(),
                    symbol: item.source.clone(),
                    file: use_decl.file,
                    span: item.span.clone(),
                };

                if is_symbol_export {
                    if symbol_bindings
                        .insert(alias.clone(), binding.clone())
                        .is_some()
                    {
                        diags.push(Diag::error(
                            use_decl.file,
                            item.span.clone(),
                            format!("duplicate `use` binding for `{alias}`"),
                        ));
                    }
                }

                if is_macro_export {
                    if macro_bindings.insert(alias.clone(), binding).is_some() {
                        diags.push(Diag::error(
                            use_decl.file,
                            item.span.clone(),
                            format!("duplicate `use` macro binding for `{alias}`"),
                        ));
                    }
                }
            }
        }

        unit.use_symbol_bindings = symbol_bindings;
        unit.use_macro_bindings = macro_bindings;
    }
}

fn compile_order_from_entry(
    entry_module_path: &str,
    units: &IndexMap<String, ModuleUnit>,
) -> Vec<String> {
    let mut order = Vec::new();
    let mut visited = FxHashSet::default();
    let mut visiting = FxHashSet::default();

    visit_module_for_order(
        entry_module_path,
        units,
        &mut visited,
        &mut visiting,
        &mut order,
    );

    order
}

fn visit_module_for_order(
    module_path: &str,
    units: &IndexMap<String, ModuleUnit>,
    visited: &mut FxHashSet<String>,
    visiting: &mut FxHashSet<String>,
    order: &mut Vec<String>,
) {
    if visited.contains(module_path) {
        return;
    }

    if !visiting.insert(module_path.to_string()) {
        return;
    }

    if let Some(unit) = units.get(module_path) {
        for import in &unit.imports {
            if units.contains_key(&import.module_path) {
                visit_module_for_order(&import.module_path, units, visited, visiting, order);
            }
        }
        for use_decl in &unit.uses {
            if units.contains_key(&use_decl.module_path) {
                visit_module_for_order(&use_decl.module_path, units, visited, visiting, order);
            }
        }
    }

    visiting.remove(module_path);
    visited.insert(module_path.to_string());
    order.push(module_path.to_string());
}

fn expand_macros_for_unit(
    unit: &ModuleUnit,
    macros_by_module: &FxHashMap<String, FxHashMap<String, MacroDef>>,
    diags: &mut Vec<Diag>,
) -> Vec<Token> {
    let mut gensym_counter = 0u64;
    expand_macro_stream(
        unit,
        &unit.filtered_tokens,
        macros_by_module,
        0,
        &mut gensym_counter,
        diags,
    )
}

fn expand_macro_stream(
    unit: &ModuleUnit,
    tokens: &[Token],
    macros_by_module: &FxHashMap<String, FxHashMap<String, MacroDef>>,
    depth: usize,
    gensym_counter: &mut u64,
    diags: &mut Vec<Diag>,
) -> Vec<Token> {
    if depth > MAX_MACRO_EXPANSION_DEPTH {
        if let Some(first) = tokens.first() {
            diags.push(
                Diag::error(
                    first.file,
                    first.span.clone(),
                    "macro expansion exceeded recursion limit",
                )
                .with_help("check for recursive macro calls"),
            );
        }
        return tokens.to_vec();
    }

    let mut out = Vec::new();
    let mut idx = 0usize;
    while idx < tokens.len() {
        let line_start = idx;
        while idx < tokens.len() && tokens[idx].kind != TokenKind::Newline {
            idx += 1;
        }
        let line_end = idx;
        let line = &tokens[line_start..line_end];

        let newline = if idx < tokens.len() {
            let token = tokens[idx].clone();
            idx += 1;
            Some(token)
        } else {
            None
        };

        if line.is_empty() {
            if let Some(newline) = newline {
                out.push(newline);
            }
            continue;
        }

        if let Some(invocation) = resolve_macro_invocation(unit, line, macros_by_module, diags) {
            let mut missing_dependency = false;
            for required_prefix in &invocation.def.required_prefixes {
                if caller_has_module_prefix(unit, required_prefix) {
                    continue;
                }

                missing_dependency = true;
                diags.push(
                    Diag::error(
                        invocation.call.file,
                        invocation.call.span.clone(),
                        format!(
                            "missing import required by macro expansion: import {required_prefix}"
                        ),
                    )
                    .with_label(DiagLabel {
                        file: invocation.def.file,
                        span: invocation.def.span.clone(),
                        message: format!("macro `{}` is defined here", invocation.def.name),
                    })
                    .with_help(format!("add `import {required_prefix}` in this module")),
                );
            }

            if !missing_dependency {
                let instantiated =
                    instantiate_macro_tokens(&invocation.def, &invocation.call, gensym_counter);
                let expanded = expand_macro_stream(
                    unit,
                    &instantiated,
                    macros_by_module,
                    depth + 1,
                    gensym_counter,
                    diags,
                );
                out.extend(expanded);
            }
        } else {
            out.extend(rewrite_macro_local_labels_outside_macro(line, diags));
        }

        if let Some(newline) = newline {
            out.push(newline);
        }
    }

    out
}

fn resolve_macro_invocation(
    unit: &ModuleUnit,
    line: &[Token],
    macros_by_module: &FxHashMap<String, FxHashMap<String, MacroDef>>,
    diags: &mut Vec<Diag>,
) -> Option<MacroInvocation> {
    let parts = parse_path_like_line(line)?;
    if parts.is_empty() {
        return None;
    }

    let call = line[0].clone();

    if parts.len() == 1 {
        let name = &parts[0];
        if let Some(def) = unit.macros.get(name) {
            return Some(MacroInvocation {
                def: def.clone(),
                call,
            });
        }

        if let Some(binding) = unit.use_macro_bindings.get(name) {
            if let Some(def) = macros_by_module
                .get(&binding.module_path)
                .and_then(|defs| defs.get(&binding.symbol))
            {
                return Some(MacroInvocation {
                    def: def.clone(),
                    call,
                });
            }
        }

        return None;
    }

    let prefix = parts[..parts.len() - 1].join("::");
    let name = parts[parts.len() - 1].clone();

    let target_module = if prefix == unit.module_path {
        Some(prefix.clone())
    } else if let Some(alias_target) = unit.aliases.get(&prefix) {
        Some(alias_target.clone())
    } else if unit.imported_paths.contains(&prefix) {
        Some(prefix.clone())
    } else {
        None
    };

    let Some(target_module) = target_module else {
        if macros_by_module
            .get(&prefix)
            .and_then(|defs| defs.get(&name))
            .is_some()
        {
            diags.push(
                Diag::error(
                    line[0].file,
                    line[0].span.clone(),
                    format!("missing import required by macro expansion: import {prefix}"),
                )
                .with_help(format!("add `import {prefix}`")),
            );
        }
        return None;
    };

    let Some(def) = macros_by_module
        .get(&target_module)
        .and_then(|defs| defs.get(&name))
        .cloned()
    else {
        return None;
    };

    if target_module != unit.module_path && !def.is_pub {
        diags.push(
            Diag::error(
                line[0].file,
                line[0].span.clone(),
                format!("macro `{name}` is not exported by module `{target_module}`"),
            )
            .with_help("mark the macro as `pub` in the defining module"),
        );
        return None;
    }

    Some(MacroInvocation { def, call })
}

fn parse_path_like_line(line: &[Token]) -> Option<Vec<String>> {
    if line.is_empty() {
        return None;
    }

    if line[0].kind != TokenKind::Ident {
        return None;
    }

    let mut parts = vec![line[0].lexeme.clone()];
    let mut idx = 1usize;
    while idx < line.len() {
        if idx + 1 >= line.len() {
            return None;
        }
        if line[idx].kind != TokenKind::DColon || line[idx + 1].kind != TokenKind::Ident {
            return None;
        }
        parts.push(line[idx + 1].lexeme.clone());
        idx += 2;
    }

    Some(parts)
}

fn caller_has_module_prefix(unit: &ModuleUnit, prefix: &str) -> bool {
    prefix == unit.module_path
        || unit.imported_paths.contains(prefix)
        || unit.aliases.contains_key(prefix)
}

fn instantiate_macro_tokens(def: &MacroDef, call: &Token, gensym_counter: &mut u64) -> Vec<Token> {
    let expansion_id = *gensym_counter;
    *gensym_counter = gensym_counter.saturating_add(1);

    let mut labels = FxHashMap::<String, String>::default();
    let mut out = Vec::with_capacity(def.body_tokens.len());

    for token in &def.body_tokens {
        if token.kind == TokenKind::MacroLocalLabel {
            let label = token.lexeme.trim_start_matches("%%").to_string();
            let replacement = labels
                .entry(label.clone())
                .or_insert_with(|| {
                    format!(
                        "__ML__{}__{}__{}__{}",
                        def.module_path.replace("::", "__"),
                        def.name,
                        expansion_id,
                        label
                    )
                })
                .clone();

            out.push(Token {
                kind: TokenKind::Ident,
                file: call.file,
                span: call.span.clone(),
                lexeme: replacement,
            });
            continue;
        }

        let mut cloned = token.clone();
        cloned.file = call.file;
        cloned.span = call.span.clone();
        out.push(cloned);
    }

    out
}

fn rewrite_macro_local_labels_outside_macro(line: &[Token], diags: &mut Vec<Diag>) -> Vec<Token> {
    let mut out = Vec::with_capacity(line.len());
    for token in line {
        if token.kind == TokenKind::MacroLocalLabel {
            diags.push(
                Diag::error(
                    token.file,
                    token.span.clone(),
                    "`%%label` can only be used inside a macro body",
                )
                .with_help("define a `macro { ... }` and use `%%label` inside it"),
            );

            out.push(Token {
                kind: TokenKind::Ident,
                file: token.file,
                span: token.span.clone(),
                lexeme: token.lexeme.trim_start_matches("%%").to_string(),
            });
        } else {
            out.push(token.clone());
        }
    }
    out
}

fn extract_qualified_prefixes(tokens: &[Token]) -> FxHashSet<String> {
    let mut prefixes = FxHashSet::default();
    let mut idx = 0usize;

    while idx < tokens.len() {
        if tokens[idx].kind != TokenKind::Ident {
            idx += 1;
            continue;
        }

        let mut parts = vec![tokens[idx].lexeme.clone()];
        let mut j = idx + 1;
        while j + 1 < tokens.len()
            && tokens[j].kind == TokenKind::DColon
            && tokens[j + 1].kind == TokenKind::Ident
        {
            parts.push(tokens[j + 1].lexeme.clone());
            j += 2;
        }

        if parts.len() >= 2 {
            let prefix = parts[..parts.len() - 1].join("::");
            prefixes.insert(prefix);
        }

        idx = j.max(idx + 1);
    }

    prefixes
}

fn rewrite_module_symbols(
    unit: &ModuleUnit,
    program: &mut Program,
    exports_by_module: &FxHashMap<String, FxHashSet<String>>,
    diags: &mut Vec<Diag>,
) {
    let local_defs = collect_local_definitions(program);
    for export in &unit.exports {
        if !local_defs.contains(export) {
            diags.push(Diag::error(
                unit.file,
                0..0,
                format!(
                    "module `{}` exports `{export}` but it is not defined",
                    unit.module_path
                ),
            ));
        }
    }

    for item in &mut program.items {
        match item {
            Item::Label(label) => {
                label.name.value = mangle_symbol(&unit.module_path, &label.name.value);
            }
            Item::Assign(assign) => {
                assign.name.value = mangle_symbol(&unit.module_path, &assign.name.value);
                rewrite_expr(
                    &mut assign.expr.value,
                    unit,
                    &local_defs,
                    exports_by_module,
                    assign.expr.file,
                    assign.expr.span.clone(),
                    diags,
                );
            }
            Item::Directive(directive) => {
                for arg in &mut directive.args {
                    match &mut arg.value {
                        DirArg::Expr(expr) => rewrite_expr(
                            expr,
                            unit,
                            &local_defs,
                            exports_by_module,
                            arg.file,
                            arg.span.clone(),
                            diags,
                        ),
                        DirArg::Ident(ident) => {
                            *ident = rewrite_symbol_name(
                                ident,
                                unit,
                                &local_defs,
                                exports_by_module,
                                arg.file,
                                arg.span.clone(),
                                diags,
                            );
                        }
                        DirArg::String(_) => {}
                    }
                }
            }
            Item::Instruction(instruction) => {
                if let Some(operand) = &mut instruction.operand {
                    match &mut operand.value {
                        Operand::Imm(expr)
                        | Operand::Expr(expr)
                        | Operand::ExprX(expr)
                        | Operand::ExprY(expr)
                        | Operand::Ind(expr)
                        | Operand::IndX(expr)
                        | Operand::IndY(expr) => rewrite_expr(
                            expr,
                            unit,
                            &local_defs,
                            exports_by_module,
                            operand.file,
                            operand.span.clone(),
                            diags,
                        ),
                        Operand::Acc => {}
                    }
                }
            }
            Item::EmptyLine(_) => {}
        }
    }
}

fn collect_local_definitions(program: &Program) -> FxHashSet<String> {
    let mut defs = FxHashSet::default();
    for item in &program.items {
        match item {
            Item::Label(label) => {
                defs.insert(label.name.value.clone());
            }
            Item::Assign(assign) => {
                defs.insert(assign.name.value.clone());
            }
            _ => {}
        }
    }
    defs
}

fn rewrite_expr(
    expr: &mut Expr,
    unit: &ModuleUnit,
    local_defs: &FxHashSet<String>,
    exports_by_module: &FxHashMap<String, FxHashSet<String>>,
    file: FileId,
    span: Span,
    diags: &mut Vec<Diag>,
) {
    match expr {
        Expr::Symbol(name) => {
            *name =
                rewrite_symbol_name(name, unit, local_defs, exports_by_module, file, span, diags);
        }
        Expr::Unary { rhs, .. } => {
            rewrite_expr(rhs, unit, local_defs, exports_by_module, file, span, diags)
        }
        Expr::Binary { lhs, rhs, .. } => {
            rewrite_expr(
                lhs,
                unit,
                local_defs,
                exports_by_module,
                file,
                span.clone(),
                diags,
            );
            rewrite_expr(rhs, unit, local_defs, exports_by_module, file, span, diags);
        }
        Expr::LowByte(inner) | Expr::HighByte(inner) => rewrite_expr(
            inner,
            unit,
            local_defs,
            exports_by_module,
            file,
            span,
            diags,
        ),
        Expr::Number(_) | Expr::CurrentPc => {}
    }
}

fn rewrite_symbol_name(
    symbol: &str,
    unit: &ModuleUnit,
    local_defs: &FxHashSet<String>,
    exports_by_module: &FxHashMap<String, FxHashSet<String>>,
    file: FileId,
    span: Span,
    diags: &mut Vec<Diag>,
) -> String {
    if let Some((prefix, name)) = split_qualified_symbol(symbol) {
        let resolved_module = if prefix == unit.module_path {
            Some(unit.module_path.clone())
        } else if let Some(path) = unit.aliases.get(&prefix) {
            Some(path.clone())
        } else if unit.imported_paths.contains(&prefix) {
            Some(prefix.clone())
        } else {
            None
        };

        let Some(target_module) = resolved_module else {
            diags.push(
                Diag::error(
                    file,
                    span,
                    format!("missing import required by qualified lookup: `{prefix}`"),
                )
                .with_help(format!("add `import {prefix}`")),
            );
            return symbol.to_string();
        };

        let Some(exports) = exports_by_module.get(&target_module) else {
            diags.push(Diag::error(
                file,
                0..0,
                format!("imported module `{target_module}` is not available"),
            ));
            return symbol.to_string();
        };

        if !exports.contains(&name) {
            diags.push(
                Diag::error(
                    file,
                    span,
                    format!("`{name}` is not exported by module `{target_module}`"),
                )
                .with_help("mark the declaration as `pub` in the exporting module"),
            );
            return symbol.to_string();
        }

        return mangle_symbol(&target_module, &name);
    }

    if local_defs.contains(symbol) {
        return mangle_symbol(&unit.module_path, symbol);
    }

    if let Some(binding) = unit.use_symbol_bindings.get(symbol) {
        if let Some(exports) = exports_by_module.get(&binding.module_path) {
            if exports.contains(&binding.symbol) {
                return mangle_symbol(&binding.module_path, &binding.symbol);
            }
        }

        diags.push(
            Diag::error(
                binding.file,
                binding.span.clone(),
                format!(
                    "`{}` is not exported by module `{}`",
                    binding.symbol, binding.module_path
                ),
            )
            .with_help("mark the declaration as `pub` in the exporting module"),
        );
    }

    symbol.to_string()
}

fn split_qualified_symbol(symbol: &str) -> Option<(String, String)> {
    if !symbol.contains("::") {
        return None;
    }

    let segments = symbol.split("::").collect::<Vec<_>>();
    if segments.len() < 2 {
        return None;
    }

    let name = segments[segments.len() - 1].to_string();
    let prefix = segments[..segments.len() - 1].join("::");
    Some((prefix, name))
}

fn mangle_symbol(module_path: &str, symbol: &str) -> String {
    format!("__M__{}__{}", module_path.replace("::", "__"), symbol)
}
