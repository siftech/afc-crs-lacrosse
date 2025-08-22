use anyhow::{Context, Result};
use clap::{value_parser, ArgAction, Parser};
use grab_a_func::{grab_a_symbol, PathRewrites};
use log::LevelFilter;
use simple_logger::SimpleLogger;
use std::{env, ffi::OsString, io::stdout, path::PathBuf};

#[derive(Debug, Parser)]
struct Args {
    /// The (ELF) binary containing the function to find.
    binary: PathBuf,

    /// The name of the function to find.
    function: String,

    /// Rewrites to perform on the path. This uses the same syntax as the -v argument to docker
    /// run.
    #[clap(short = 'v', long = "volume")]
    volumes: Vec<OsString>,

    /// Decreases the log level.
    #[clap(
        long,
        conflicts_with("verbose"),
        action = ArgAction::Count,
        value_parser = value_parser!(u8).range(..=2)
    )]
    quiet: u8,

    /// Increases the log level.
    #[clap(
        long,
        conflicts_with("quiet"),
        action = ArgAction::Count,
        value_parser = value_parser!(u8).range(..=3)
    )]
    verbose: u8,
}

fn main() -> Result<()> {
    // Set up backtraces.
    if env::var_os("RUST_BACKTRACE").is_none() {
        env::set_var("RUST_BACKTRACE", "1");
    }

    // Parse the arguments.
    let args = Args::parse();
    let path_rewrites =
        PathRewrites::from_args(&args.volumes).context("Failed to parse -v flags")?;

    // Configure the logger.
    let log_level = match (args.quiet, args.verbose) {
        (0, 0) => LevelFilter::Warn,
        (0, 1) => LevelFilter::Info,
        (0, 2) => LevelFilter::Debug,
        (0, _) => LevelFilter::Trace,
        (1, _) => LevelFilter::Error,
        (_, _) => LevelFilter::Off,
    };
    SimpleLogger::new()
        .with_level(log_level)
        .init()
        .context("Failed to configure logger")?;

    // Get the source for the symbol.
    let source = grab_a_symbol(&path_rewrites, &args.binary, &args.function)
        .context("Failed to extract symbol")?;
    serde_json::to_writer_pretty(stdout(), &source)?;
    println!("");
    Ok(())
}
