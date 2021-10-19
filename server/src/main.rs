#![feature(drain_filter)]
#[macro_use]
extern crate rocket;

use fuzzcheck_view::fuzzcheck::{read_input_corpus, CorpusMap, CoverageMap, SerializedUniqCov};
use fuzzcheck_view::{CodeSpanKind, CoverageFilter, FunctionCoverage, FunctionName};
use rocket::serde::json::Json;
use rocket::{fs::NamedFile, State};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[get("/")]
async fn index() -> Option<NamedFile> {
    NamedFile::open(Path::new("../resources").join("index.html")).await.ok()
}

#[get("/files")]
fn files(state: &State<ManagedData>) -> Json<Vec<String>> {
    Json(state.functions_per_file.keys().cloned().collect())
}

#[get("/functions?<file>")]
fn functions(state: &State<ManagedData>, file: String) -> Json<Vec<FunctionName>> {
    let functions = state.functions_per_file[&file].clone();
    Json(functions)
}

#[get("/coverage?<filter>&<function>")]
fn coverage(state: &State<ManagedData>, filter: String, function: String) -> Json<FunctionCoverage> {
    match CoverageFilter::from_string(&filter) {
        Some(CoverageFilter::All) => {
            let function_coverage = state.function_coverage.get(&function).unwrap();
            Json(function_coverage.clone())
        }
        Some(CoverageFilter::Input(input_idx)) => {
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
                    match span.kind {
                        CodeSpanKind::Untracked => {}
                        CodeSpanKind::NotHit { id } | CodeSpanKind::Hit { id } => {
                            if counters.contains(&id) {
                                span.kind = CodeSpanKind::Hit { id };
                            } else {
                                span.kind = CodeSpanKind::NotHit { id };
                            }
                        }
                    }
                }
            }
            Json(block)
        }
        None => panic!(),
    }
}

#[get("/best_input?<counter>")]
fn best_input_for_counter(state: &State<ManagedData>, counter: usize) -> Json<(String, String)> {
    let pool_idx = state
        .uniq_cov
        .best_for_counter
        .iter()
        .find(|x| x.0 == counter)
        .unwrap()
        .1;
    let name_input = &state.corpus_map.0.iter().find(|x| x.0 .1 == pool_idx).unwrap().1;
    let data = state.all_inputs[name_input].clone();
    // let string = String::from_utf8(data).unwrap();
    // Json(string)
    let decoded: Vec<serde_json::Value> = serde_json::from_slice(&data).unwrap();
    let string: String = serde_json::from_value(decoded[1].clone()).unwrap();
    Json((name_input.clone(), string))
}

#[get("/inputs")]
fn inputs(state: &State<ManagedData>) -> Json<Vec<(usize, String)>> {
    let inputs = state
        .uniq_cov
        .ranked_inputs
        .iter()
        .map(|&pool_idx| {
            let name_input = &state.corpus_map.0.iter().find(|x| x.0 .1 == pool_idx).unwrap().1;
            let data = state.all_inputs[name_input].clone();
            let decoded: Vec<serde_json::Value> = serde_json::from_slice(&data).unwrap();
            let string: String = serde_json::from_value(decoded[1].clone()).unwrap();
            (pool_idx, string)
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
    let source_folder = Path::new(&args[1]).to_path_buf();
    let fuzz_test = args[2].clone();
    let fuzz_folder = source_folder.join("fuzz").join(fuzz_test);
    let stats_folder = fuzz_folder.join("stats");
    let mut stats_folders = vec![];
    for directory in std::fs::read_dir(stats_folder).unwrap() {
        let directory = directory.unwrap();
        if directory.file_type().unwrap().is_dir() {
            stats_folders.push(directory.path());
        }
    }
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

    let mut functions = coverage_map.functions();

    for block in functions.iter_mut() {
        for line in block.lines.iter_mut() {
            for span in line.spans.iter_mut() {
                match span.kind {
                    CodeSpanKind::Untracked => {}
                    CodeSpanKind::NotHit { id } | CodeSpanKind::Hit { id } => {
                        if uniq_cov.all_hit_counters.contains(&id) {
                            span.kind = CodeSpanKind::Hit { id };
                        } else {
                            span.kind = CodeSpanKind::NotHit { id };
                        }
                    }
                }
            }
        }
    }

    let mut functions_per_file = HashMap::<String, Vec<FunctionName>>::new();
    let mut function_coverage = HashMap::<String, FunctionCoverage>::new();
    for c in functions {
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
            files,
            functions,
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
