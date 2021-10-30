use std::{path::PathBuf, process::exit};

use getopts::Options;

pub struct CliArguments {
    pub workspace_directory: PathBuf,
    pub crate_directory: PathBuf,
    pub test: String,
}

pub fn cli_argument_parser() -> Options {
    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu")
        .reqopt(
            "d",
            "directory",
            "path to the top folder of the crate being fuzzed, which must contain a 'fuzz' folder",
            "<PATH>",
        )
        .reqopt("t", "test", "name of the fuzz test", "(e.g. tests::fuzz)")
        .optopt(
            "w",
            "workspace",
            "path to the cargo workspace containing the crate, if any",
            "",
        );
    opts
}

pub fn parse_arguments(options: &Options, arguments: &[String]) -> CliArguments {
    match options.parse(arguments) {
        Ok(matches) => {
            if matches.opt_present("help") {
                println!("{}", options.usage("fuzzcheck-view -d DIRECTORY -t FUZZ_TARGET"));
                exit(0);
            }
            let crate_directory = matches.opt_get::<PathBuf>("directory").unwrap().unwrap();
            let test = matches.opt_get::<String>("test").unwrap().unwrap();

            let workspace_directory = matches
                .opt_get::<PathBuf>("workspace")
                .unwrap()
                .unwrap_or(crate_directory.clone());

            CliArguments {
                crate_directory,
                test,
                workspace_directory,
            }
        }
        Err(e) => {
            eprintln!("error: {}", e);
            println!("{}", options.usage("fuzzcheck-view -d DIRECTORY -t FUZZ_TARGET"));
            exit(1);
        }
    }
}
