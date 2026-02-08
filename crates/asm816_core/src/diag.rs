use std::io::{self, Write};

use ariadne::{
    Color, ColorGenerator, Config, IndexType, Label, LabelAttach, Report, ReportKind, sources,
};

use crate::source::{FileId, SourceManager, Span};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Clone, Debug)]
pub struct DiagLabel {
    pub file: FileId,
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct Diag {
    pub severity: Severity,
    pub message: String,
    pub primary: DiagLabel,
    pub labels: Vec<DiagLabel>,
    pub help: Option<String>,
    pub code: Option<String>,
}

impl Diag {
    pub fn error(file: FileId, span: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Error, file, span, message)
    }

    pub fn warning(file: FileId, span: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, file, span, message)
    }

    pub fn note(file: FileId, span: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Note, file, span, message)
    }

    pub fn with_label(mut self, label: DiagLabel) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    fn new(severity: Severity, file: FileId, span: Span, message: impl Into<String>) -> Self {
        let message = message.into();
        Self {
            severity,
            primary: DiagLabel {
                file,
                span,
                message: message.clone(),
            },
            message,
            labels: Vec::new(),
            help: None,
            code: None,
        }
    }
}

pub fn has_errors(diags: &[Diag]) -> bool {
    diags.iter().any(|diag| diag.severity == Severity::Error)
}

pub fn render_diags(source_manager: &SourceManager, diags: &[Diag]) -> io::Result<()> {
    let mut stderr = io::stderr();
    render_diags_to_writer(source_manager, diags, &mut stderr, true)
}

pub fn render_diags_to_string(
    source_manager: &SourceManager,
    diags: &[Diag],
) -> io::Result<String> {
    let mut buffer = Vec::new();
    render_diags_to_writer(source_manager, diags, &mut buffer, false)?;
    String::from_utf8(buffer).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
}

fn render_diags_to_writer<W: Write>(
    source_manager: &SourceManager,
    diags: &[Diag],
    mut writer: W,
    use_color: bool,
) -> io::Result<()> {
    let mut cache = sources(
        source_manager
            .files_iter()
            .map(|(id, file)| (id.0 as usize, file.text.clone())),
    );

    for diag in diags {
        let report_kind = match diag.severity {
            Severity::Error => ReportKind::Error,
            Severity::Warning => ReportKind::Warning,
            Severity::Note => ReportKind::Advice,
        };
        let config = report_config(diag.severity, use_color);

        let mut colors = ColorGenerator::new();
        let mut report = Report::build(
            report_kind,
            (diag.primary.file.0 as usize, diag.primary.span.clone()),
        )
        .with_message(diag.message.clone())
        .with_config(config)
        .with_label(
            Label::new((diag.primary.file.0 as usize, diag.primary.span.clone()))
                .with_message(diag.primary.message.clone())
                .with_color(primary_color(diag.severity, &mut colors))
                .with_order(0),
        );

        if let Some(code) = &diag.code {
            report = report.with_code(code.clone());
        }

        for (idx, label) in diag.labels.iter().enumerate() {
            report = report.with_label(
                Label::new((label.file.0 as usize, label.span.clone()))
                    .with_message(label.message.clone())
                    .with_color(colors.next())
                    .with_order((idx + 1) as i32),
            );
        }

        if let Some(help) = &diag.help {
            report = report.with_help(help.clone());
        }

        report.finish().write(&mut cache, &mut writer)?;
    }

    Ok(())
}

fn primary_color(severity: Severity, colors: &mut ColorGenerator) -> Color {
    match severity {
        Severity::Error => Color::Red,
        Severity::Warning => Color::Yellow,
        Severity::Note => colors.next(),
    }
}

fn report_config(severity: Severity, use_color: bool) -> Config {
    // We emit byte spans from the lexer/parser, so diagnostics must render with byte indexing.
    let base = Config::default()
        .with_color(use_color)
        .with_index_type(IndexType::Byte)
        .with_label_attach(LabelAttach::Middle)
        .with_cross_gap(true)
        .with_tab_width(4);

    match severity {
        Severity::Error => base,
        Severity::Warning => base.with_compact(true),
        Severity::Note => base.with_compact(true),
    }
}
