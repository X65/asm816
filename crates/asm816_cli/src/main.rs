use std::path::PathBuf;

use asm816_core::{
    AssembleOptions, CheckOptions, FmtOptions, assemble_path, check_path, format_path,
};
use asm816_core::{CpuMode, Width};
use clap::{Args, Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(name = "asm816")]
#[command(about = "WDC 65816/6502 assembler")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Assemble(AssembleArgs),
    Check(CheckArgs),
    Fmt(FmtArgs),
}

#[derive(Args, Debug)]
struct AssembleArgs {
    input: PathBuf,

    #[arg(short = 'o', long = "output")]
    output: PathBuf,

    #[arg(short = 'I', long = "include", value_name = "DIR")]
    include_dirs: Vec<PathBuf>,

    #[arg(long, conflicts_with = "a16")]
    a8: bool,
    #[arg(long, conflicts_with = "a8")]
    a16: bool,

    #[arg(long, conflicts_with = "i16")]
    i8: bool,
    #[arg(long, conflicts_with = "i8")]
    i16: bool,
}

#[derive(Args, Debug)]
struct CheckArgs {
    input: PathBuf,

    #[arg(short = 'I', long = "include", value_name = "DIR")]
    include_dirs: Vec<PathBuf>,

    #[arg(long, conflicts_with = "a16")]
    a8: bool,
    #[arg(long, conflicts_with = "a8")]
    a16: bool,

    #[arg(long, conflicts_with = "i16")]
    i8: bool,
    #[arg(long, conflicts_with = "i8")]
    i16: bool,
}

#[derive(Args, Debug)]
struct FmtArgs {
    input: PathBuf,
}

fn main() {
    let cli = Cli::parse();
    let exit_code = match cli.command {
        Commands::Assemble(args) => run_assemble(args),
        Commands::Check(args) => run_check(args),
        Commands::Fmt(args) => run_fmt(args),
    };

    std::process::exit(exit_code);
}

fn run_assemble(args: AssembleArgs) -> i32 {
    let opts = AssembleOptions {
        cpu_mode: mode_from_flags(args.a16, args.i16),
        include_dirs: args.include_dirs,
    };

    match assemble_path(&args.input, &args.output, &opts) {
        Ok(_) => 0,
        Err(_) => 1,
    }
}

fn run_check(args: CheckArgs) -> i32 {
    let opts = CheckOptions {
        cpu_mode: mode_from_flags(args.a16, args.i16),
        include_dirs: args.include_dirs,
    };

    match check_path(&args.input, &opts) {
        Ok(_) => 0,
        Err(_) => 1,
    }
}

fn run_fmt(args: FmtArgs) -> i32 {
    match format_path(&args.input, &FmtOptions::default()) {
        Ok(formatted) => {
            print!("{formatted}");
            0
        }
        Err(_) => 1,
    }
}

fn mode_from_flags(a16: bool, i16: bool) -> CpuMode {
    CpuMode {
        a: if a16 { Width::W16 } else { Width::W8 },
        xy: if i16 { Width::W16 } else { Width::W8 },
    }
}

#[cfg(test)]
mod tests {
    use super::{Cli, Commands};
    use clap::Parser;

    #[test]
    fn parses_assemble_command() {
        let cli = Cli::parse_from([
            "asm816", "assemble", "main.s", "-o", "out.bin", "--a16", "--i16",
        ]);

        assert!(matches!(cli.command, Commands::Assemble(_)));
    }
}
