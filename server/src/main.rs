#![feature(drain_filter)]
#[macro_use]
extern crate rocket;

use fuzzcheck_view::args::CliArguments;
use fuzzcheck_view::fuzzcheck::{read_input_corpus, CorpusMap, CoverageMap, SerializedUniqCov};
use fuzzcheck_view::{
    CodeSpanKind, CoverageKindFilter, CoverageStatus, FunctionCoverage, FunctionFilter, FunctionName, InputFilter,
    InputInfo,
};
use rocket::serde::json::Json;
use rocket::{fs::NamedFile, State};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[get("/")]
async fn index() -> Option<NamedFile> {
    NamedFile::open(Path::new("../resources").join("index.html")).await.ok()
}

#[get("/functions?<input_filter>&<function_filter>&<coverage_kind_filter>")]
fn functions(
    state: &State<ManagedData>,
    input_filter: InputFilter,
    function_filter: Vec<FunctionFilter>,
    coverage_kind_filter: CoverageKindFilter,
) -> Json<Vec<(String, Vec<FunctionName>)>> {
    let exclude_100 = function_filter
        .iter()
        .find(|filter| matches!(filter, FunctionFilter::Exclude100PercentCoverage))
        .is_some();
    let exclude_0 = function_filter
        .iter()
        .find(|filter| matches!(filter, FunctionFilter::Exclude0PercentCoverage))
        .is_some();
    if !(exclude_0 || exclude_100) {
        return Json(state.functions_per_file.clone().into_iter().collect());
    }
    let input_counters = match input_filter {
        InputFilter::All => HashSet::<usize>::from_iter(state.uniq_cov.all_hit_counters.iter().copied()),
        InputFilter::Input(input_idx) => {
            let all_input_counters = HashSet::<usize>::from_iter(
                state
                    .uniq_cov
                    .counters_for_input
                    .iter()
                    .find(|(idx, _)| *idx == input_idx)
                    .unwrap()
                    .1
                    .iter()
                    .copied(),
            );
            match coverage_kind_filter {
                CoverageKindFilter::All => all_input_counters,
                CoverageKindFilter::LeastComplex => {
                    let mut input_counters = all_input_counters;
                    for (counter_idx, best_input_idx) in state.uniq_cov.best_for_counter.iter() {
                        // 1. this is the right input
                        if input_counters.contains(counter_idx) {
                            // 2. but it is not the least complex
                            if input_idx != *best_input_idx {
                                input_counters.remove(counter_idx);
                            }
                        } else {
                            input_counters.remove(counter_idx);
                        }
                    }
                    input_counters
                }
                CoverageKindFilter::Unique => todo!(),
            }
        }
    };

    let mut functions_per_file = state.functions_per_file.clone();
    let mut files_to_remove = vec![];
    for (file, function_names) in functions_per_file.iter_mut() {
        function_names.drain_filter(|function_name| {
            let function = &state.function_coverage[&function_name.name];
            let mut any_counter_hit = false;
            let mut all_counters_hit = true;
            for counter_id in function.counter_ids.iter() {
                if input_counters.contains(counter_id) {
                    any_counter_hit = true
                } else {
                    all_counters_hit = false
                }
            }
            (exclude_0 && !any_counter_hit) || (exclude_100 && all_counters_hit)
        });
        if function_names.is_empty() {
            files_to_remove.push(file.clone());
        }
    }
    for file_to_remove in files_to_remove {
        functions_per_file.remove(&file_to_remove);
    }

    Json(functions_per_file.into_iter().collect())
}

#[get("/coverage?<input_filter>&<function>")]
fn coverage(state: &State<ManagedData>, input_filter: InputFilter, function: String) -> Json<FunctionCoverage> {
    match input_filter {
        InputFilter::All => {
            let function_coverage = state.function_coverage.get(&function).unwrap();
            println!("{:?}", function_coverage);
            Json(function_coverage.clone())
        }
        InputFilter::Input(input_idx) => {
            let counters = &state
                .uniq_cov
                .counters_for_input
                .iter()
                .find(|x| x.0 == input_idx)
                .unwrap()
                .1;
            let counters = HashSet::<usize>::from_iter(counters.iter().copied());
            let function = state
                .coverage_map
                .functions
                .iter()
                .find(|f| f.name == function)
                .unwrap();
            let mut block = function.coverage();
            for line in block.lines.iter_mut() {
                for span in line.spans.iter_mut() {
                    match &mut span.kind {
                        CodeSpanKind::Untracked => {}
                        CodeSpanKind::Tracked { id, status } => {
                            *status = if counters.contains(&id) {
                                if input_idx == state.uniq_cov.best_for_counter.iter().find(|(x, _)| x == id).unwrap().1
                                {
                                    CoverageStatus::Best
                                } else {
                                    CoverageStatus::Hit
                                }
                            } else {
                                CoverageStatus::NotHit
                            };
                        }
                    }
                }
            }
            Json(block)
        }
    }
}

#[get("/best_input?<counter>")]
fn best_input_for_counter(state: &State<ManagedData>, counter: usize) -> Json<String> {
    let pool_idx = state
        .uniq_cov
        .best_for_counter
        .iter()
        .find(|x| x.0 == counter)
        .unwrap()
        .1;
    let name_input = &state.corpus_map.0.iter().find(|x| x.0 .1 == pool_idx).unwrap().1;
    Json(name_input.clone())
}

#[get("/input?<hash>")]
fn input(state: &State<ManagedData>, hash: &str) -> Json<String> {
    let data = &state.all_inputs[hash];
    let string = String::from_utf8_lossy(data).to_string();

    // let decoded: Vec<serde_json::Value> = serde_json::from_slice(&data).unwrap();
    // let string: String = serde_json::from_value(decoded[1].clone()).unwrap();

    Json(string)
}

#[get("/inputs")]
fn inputs(state: &State<ManagedData>) -> Json<Vec<InputInfo>> {
    let inputs = state
        .uniq_cov
        .ranked_inputs
        .iter()
        .map(|&pool_idx| {
            let name_input = &state.corpus_map.0.iter().find(|x| x.0 .1 == pool_idx).unwrap().1;
            InputInfo {
                pool_idx,
                hash: name_input.clone(),
            }
        })
        .collect::<Vec<_>>();
    Json(inputs)
}

// allow html to reference any file with path /static under folder "static"
#[get("/<file..>", rank = 10)] // use rank here to allow other api endpoint available as well
async fn serve_static_file(file: PathBuf) -> Option<NamedFile> {
    NamedFile::open(Path::new("../resources/").join(file)).await.ok()
}

#[launch]
fn rocket() -> _ {
    let args = std::env::args().collect::<Vec<_>>();

    let parser = fuzzcheck_view::args::cli_argument_parser();
    let args = fuzzcheck_view::args::parse_arguments(&parser, &args);

    let CliArguments {
        crate_directory,
        test: fuzz_test,
        workspace_directory: source_folder,
    } = args;

    let fuzz_folder = crate_directory.join("fuzz").join(fuzz_test);
    let stats_folder = fuzz_folder.join("stats");
    let mut stats_folders = vec![];
    for directory in std::fs::read_dir(stats_folder).unwrap() {
        let directory = directory.unwrap();
        if directory.file_type().unwrap().is_dir() {
            stats_folders.push(directory.path());
        }
    }
    stats_folders.sort();
    let stats_folder = stats_folders.last().unwrap();

    println!("launching on {}", stats_folder.display());

    let coverage_map: CoverageMap = {
        let coverage_map_path = stats_folder.join("coverage_sensor.json");
        println!("coverage map path: {}", coverage_map_path.display());
        let coverage_map =
            std::fs::read(&coverage_map_path).expect(&format!("can't read {}", coverage_map_path.display()));
        let mut coverage_map: CoverageMap = serde_json::from_slice(&coverage_map).expect("can't parse coverage map");
        coverage_map.map_relative_paths(|path| {
            *path = source_folder.join(&path);
        });
        coverage_map
    };
    let uniq_cov: SerializedUniqCov = {
        let uniq_cov_path = stats_folder.join("uniq_cov.json");
        let uniq_cov = std::fs::read(&uniq_cov_path).expect(&format!("can't read {}", uniq_cov_path.display()));
        serde_json::from_slice(&uniq_cov).expect("can't parse uniq_cov")
    };
    let corpus_map: CorpusMap = {
        let uniq_cov_path = stats_folder.join("world.json");
        let uniq_cov = std::fs::read(&uniq_cov_path).expect(&format!("can't read {}", uniq_cov_path.display()));
        serde_json::from_slice(&uniq_cov).expect("can't parse world")
    };
    let all_inputs = read_input_corpus(&fuzz_folder.join("corpus"));

    let mut cov_functions = coverage_map.functions();

    for block in cov_functions.iter_mut() {
        for line in block.lines.iter_mut() {
            for span in line.spans.iter_mut() {
                match &mut span.kind {
                    CodeSpanKind::Untracked => {}
                    CodeSpanKind::Tracked { id, status } => {
                        *status = if uniq_cov.all_hit_counters.contains(&id) {
                            CoverageStatus::Hit
                        } else {
                            CoverageStatus::NotHit
                        };
                    }
                }
            }
        }
    }

    let mut functions_per_file = HashMap::<String, Vec<FunctionName>>::new();
    let mut function_coverage = HashMap::<String, FunctionCoverage>::new();
    for c in cov_functions {
        let entry = functions_per_file.entry(c.file.clone()).or_default();
        entry.push(c.name.clone());
        let name = c.name.clone();
        let old = function_coverage.insert(c.name.name.clone(), c);
        assert!(
            old.is_none(),
            "old: {:?}\nnew: {:?}",
            old.unwrap(),
            &function_coverage[&name.name]
        );
    }
    let data = ManagedData {
        coverage_map,
        uniq_cov,
        functions_per_file,
        function_coverage,
        corpus_map,
        all_inputs,
    };

    rocket::build().manage(data).mount(
        "/",
        routes![
            index,
            inputs,
            functions,
            input,
            coverage,
            best_input_for_counter,
            serve_static_file
        ],
    )
}

struct ManagedData {
    coverage_map: CoverageMap,
    functions_per_file: HashMap<String, Vec<FunctionName>>,
    function_coverage: HashMap<String, FunctionCoverage>,
    uniq_cov: SerializedUniqCov,
    corpus_map: CorpusMap,
    all_inputs: HashMap<String, Vec<u8>>,
}
