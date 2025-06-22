use clap::{Parser, Subcommand};
use functions::generate;

mod bliss_lib;
mod compiler;
mod functions;
mod utils;

#[derive(Parser)]
#[command(version, about = "The compiler and toolchain for the Bliss Programming Language.", long_about = None)]
struct Bliss {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate IR from a .bliss file
    Generate {
        /// The .bliss file to generate
        file: String,

        /// Print debug information during generation
        #[arg(short, long)]
        debug: bool,

        /// Do not include the prelude in the generated code
        #[arg(short, long)]
        no_prelude: bool,
    },
}

fn do_generate(file: &str, debug: &bool, no_prelude: &bool) {
    let res = generate::generate(file, *debug, *no_prelude);

    if let Err(reason) = res {
        eprintln!("\n\nError generating:\n{}", reason)
    }
}

fn main() {
    let bliss = Bliss::parse();

    match &bliss.command {
        Some(Commands::Generate {
            file,
            debug,
            no_prelude,
        }) => do_generate(file, debug, no_prelude),

        None => {}
    }
}
