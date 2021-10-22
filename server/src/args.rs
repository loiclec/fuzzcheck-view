use std::path::PathBuf;

use getopts::Options;

pub struct CliArguments {
    pub directory: PathBuf,
    pub test: String,
}

pub fn cli_argument_parser() -> Options {
    let mut opts = Options::new();
    opts.reqopt(
        "d",
        "directory",
        "path to the top folder of the crate being fuzzed, which must contain a 'fuzz' folder",
        "<PATH>",
    )
    .reqopt("t", "test", "name of the fuzz test", "(e.g. tests::fuzz)");
    opts
}

pub fn parse_arguments(options: &Options, arguments: &[String]) -> Option<CliArguments> {
    let matches = options.parse(arguments).ok()?;
    let directory = matches.opt_get::<PathBuf>("directory").ok()??;
    let test = matches.opt_get::<String>("test").ok()??;

    Some(CliArguments { directory, test })
}
