use anyhow::{Context, Result};
use clap::{value_parser, ArgAction, Args, Parser};
use log::LevelFilter;
use simple_logger::SimpleLogger;
use std::env;

/// A helper for making an application. Parses arguments, configures a logger, and runs the `main`
/// closure.
pub fn app<T: Args>(main: impl FnOnce(T) -> Result<()>) -> Result<()> {
    #[derive(Debug, Parser)]
    struct ArgsWithVerbosity<T: Args> {
        /// The actual arguments.
        #[clap(flatten)]
        args: T,

        /// Decreases the log level.
        #[clap(
            short,
            long,
            conflicts_with("verbose"),
            action = ArgAction::Count,
            value_parser = value_parser!(u8).range(..=2)
        )]
        quiet: u8,

        /// Increases the log level.
        #[clap(
            short,
            long,
            conflicts_with("quiet"),
            action = ArgAction::Count,
            value_parser = value_parser!(u8).range(..=3)
        )]
        verbose: u8,
    }

    // Parse the arguments.
    let args = ArgsWithVerbosity::<T>::parse();

    // Set up backtraces.
    if env::var_os("RUST_BACKTRACE").is_none() {
        env::set_var("RUST_BACKTRACE", "1");
    }

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

    // Run the application.
    main(args.args)
}
