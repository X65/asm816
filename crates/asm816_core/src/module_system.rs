use std::path::{Path, PathBuf};

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
    span: Span,
    file: FileId,
}

#[derive(Clone, Debug)]
struct ModuleUnit {
    file: FileId,
    file_path: PathBuf,
    module_path: String,
    imports: Vec<ImportDecl>,
    imported_paths: FxHashSet<String>,
    aliases: FxHashMap<String, String>,
    exports: FxHashSet<String>,
    filtered_tokens: Vec<Token>,
}

#[derive(Clone, Debug, Default)]
struct ModuleAnalysis {
    module_header: Option<(String, Span)>,
    imports: Vec<ImportDecl>,
    exports: FxHashSet<String>,
    filtered_tokens: Vec<Token>,
    diags: Vec<Diag>,
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

    let mut units = IndexMap::<String, ModuleUnit>::new();
    let mut diags = Vec::new();

    let entry_module_path = match load_module_recursive(
        source_manager,
        entry_file,
        &roots,
        None,
        &mut units,
        &mut diags,
    ) {
        Some(path) => path,
        None => {
            return ModuleBuild {
                entry_file,
                program: Program::default(),
                diags,
            };
        }
    };

    let exports_by_module = units
        .iter()
        .map(|(path, unit)| (path.clone(), unit.exports.clone()))
        .collect::<FxHashMap<_, _>>();

    let mut order = Vec::new();
    let mut visited = FxHashSet::default();
    let mut visiting = FxHashSet::default();
    visit_module(
        &entry_module_path,
        &units,
        &mut visited,
        &mut visiting,
        &mut order,
    );

    let mut program = Program::default();
    for module_path in order {
        let Some(unit) = units.get(&module_path) else {
            continue;
        };

        let (mut module_program, parse_diags) = parse_tokens(&unit.filtered_tokens);
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

fn load_module_recursive(
    source_manager: &mut SourceManager,
    file: FileId,
    roots: &[PathBuf],
    expected_path: Option<&str>,
    units: &mut IndexMap<String, ModuleUnit>,
    diags: &mut Vec<Diag>,
) -> Option<String> {
    let file_path = source_manager.file(file).path.clone();
    let module_path = canonical_module_path(&file_path, roots, file, diags)?;

    if let Some(expected_path) = expected_path {
        if module_path != expected_path {
            diags.push(Diag::error(
                file,
                0..0,
                format!(
                    "module file resolves to `{module_path}` but was imported as `{expected_path}`"
                ),
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
                    message: "first definition is here".to_string(),
                }),
            );
        }
        return Some(module_path);
    }

    let (tokens, lex_diags) = lex_file(source_manager, file);
    diags.extend(lex_diags);

    let analysis = analyze_module_tokens(file, &tokens);
    diags.extend(analysis.diags);

    if let Some((header_path, header_span)) = &analysis.module_header {
        if header_path != &module_path {
            diags.push(Diag::error(
                file,
                header_span.clone(),
                format!(
                    "module header `{header_path}` does not match canonical path `{module_path}`"
                ),
            ));
        }
    }

    let mut imported_paths = FxHashSet::default();
    let mut aliases = FxHashMap::default();
    for import in &analysis.imports {
        imported_paths.insert(import.module_path.clone());
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

    let unit = ModuleUnit {
        file,
        file_path: file_path.clone(),
        module_path: module_path.clone(),
        imports: analysis.imports.clone(),
        imported_paths,
        aliases,
        exports: analysis.exports,
        filtered_tokens: analysis.filtered_tokens,
    };
    units.insert(module_path.clone(), unit);

    for import in analysis.imports {
        match resolve_import_path(&import.module_path, roots) {
            Ok(resolved_path) => match source_manager.load_path(&resolved_path) {
                Ok(import_file) => {
                    let _ = load_module_recursive(
                        source_manager,
                        import_file,
                        roots,
                        Some(&import.module_path),
                        units,
                        diags,
                    );
                }
                Err(err) => diags.push(Diag::error(
                    import.file,
                    import.span.clone(),
                    format!(
                        "failed to load imported module `{}` from `{}`: {err}",
                        import.module_path,
                        resolved_path.display()
                    ),
                )),
            },
            Err(message) => {
                diags.push(
                    Diag::error(import.file, import.span.clone(), message).with_help(format!(
                        "expected file `{}`",
                        module_path_to_relative_file(&import.module_path)
                    )),
                );
            }
        }
    }

    Some(module_path)
}

fn analyze_module_tokens(file: FileId, tokens: &[Token]) -> ModuleAnalysis {
    let mut analysis = ModuleAnalysis::default();
    analysis.filtered_tokens.reserve(tokens.len());

    let mut saw_non_empty_line = false;
    let mut idx = 0usize;
    while idx < tokens.len() {
        let line_start = idx;
        while idx < tokens.len() && tokens[idx].kind != TokenKind::Newline {
            idx += 1;
        }
        let line_tokens = &tokens[line_start..idx];
        let newline = if idx < tokens.len() {
            let newline = tokens[idx].clone();
            idx += 1;
            Some(newline)
        } else {
            None
        };

        if line_tokens.is_empty() {
            if let Some(newline) = newline {
                analysis.filtered_tokens.push(newline);
            }
            continue;
        }

        if token_is_ident(&line_tokens[0], "module") {
            if saw_non_empty_line {
                analysis.diags.push(Diag::error(
                    file,
                    line_tokens[0].span.clone(),
                    "`module` header must appear before any other non-empty line",
                ));
            }

            match parse_module_path_tokens(file, &line_tokens[1..]) {
                Ok((module_path, span)) => {
                    if analysis.module_header.is_none() {
                        analysis.module_header = Some((module_path, span));
                    } else {
                        analysis.diags.push(Diag::error(
                            file,
                            span,
                            "duplicate `module` header in file",
                        ));
                    }
                }
                Err(diag) => analysis.diags.push(diag),
            }
            saw_non_empty_line = true;
            if let Some(newline) = newline {
                analysis.filtered_tokens.push(newline);
            }
            continue;
        }

        if token_is_ident(&line_tokens[0], "import") {
            match parse_import_line(file, line_tokens) {
                Ok(import) => analysis.imports.push(import),
                Err(diag) => analysis.diags.push(diag),
            }
            saw_non_empty_line = true;
            if let Some(newline) = newline {
                analysis.filtered_tokens.push(newline);
            }
            continue;
        }

        saw_non_empty_line = true;
        if token_is_ident(&line_tokens[0], "pub") {
            if line_tokens.len() < 2 {
                analysis.diags.push(Diag::error(
                    file,
                    line_tokens[0].span.clone(),
                    "`pub` must be followed by a declaration",
                ));
            } else {
                if let Some(name) = parse_pub_export_name(&line_tokens[1..]) {
                    analysis.exports.insert(name.to_string());
                } else {
                    analysis.diags.push(
                        Diag::error(
                            file,
                            line_tokens[0].span.start..line_tokens[line_tokens.len() - 1].span.end,
                            "`pub` currently supports only labels and assignments",
                        )
                        .with_help("use `pub NAME:` or `pub NAME = expr`"),
                    );
                }
                analysis
                    .filtered_tokens
                    .extend(line_tokens[1..].iter().cloned());
            }
        } else {
            analysis.filtered_tokens.extend(line_tokens.iter().cloned());
        }

        if let Some(newline) = newline {
            analysis.filtered_tokens.push(newline);
        }
    }

    analysis
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
        span: path_span,
        file,
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

    if relative.extension().and_then(|ext| ext.to_str()) != Some("asm") {
        diags.push(Diag::error(
            file,
            0..0,
            format!(
                "module files must use `.asm` extension: `{}`",
                file_path.display()
            ),
        ));
        return None;
    }

    let mut segments = Vec::new();
    for component in relative.iter() {
        let Some(raw) = component.to_str() else {
            diags.push(Diag::error(
                file,
                0..0,
                "module path contains non-UTF8 segment",
            ));
            return None;
        };
        segments.push(raw.to_string());
    }

    let Some(last) = segments.last_mut() else {
        diags.push(Diag::error(file, 0..0, "empty module path"));
        return None;
    };

    if let Some(stem) = Path::new(last).file_stem().and_then(|stem| stem.to_str()) {
        *last = stem.to_string();
    }

    for segment in &segments {
        if !is_valid_module_segment(segment) {
            diags.push(Diag::error(
                file,
                0..0,
                format!("invalid module segment `{segment}`; expected lowercase snake_case"),
            ));
            return None;
        }
    }

    Some(segments.join("::"))
}

fn resolve_import_path(module_path: &str, roots: &[PathBuf]) -> Result<PathBuf, String> {
    let relative = module_path_to_relative_file(module_path);
    let mut hits = Vec::new();
    for root in roots {
        let candidate = root.join(&relative);
        if candidate.is_file() {
            hits.push(candidate);
        }
    }

    match hits.len() {
        0 => Err(format!("module not found: `{module_path}`")),
        1 => Ok(hits.remove(0)),
        _ => {
            let choices = hits
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(", ");
            Err(format!(
                "ambiguous module import `{module_path}`; candidates: {choices}"
            ))
        }
    }
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

fn visit_module(
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
                visit_module(&import.module_path, units, visited, visiting, order);
            }
        }
    }

    visiting.remove(module_path);
    visited.insert(module_path.to_string());
    order.push(module_path.to_string());
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
