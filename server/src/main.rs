#![feature(drain_filter)]
#[macro_use]
extern crate rocket;

use fuzzcheck_view::fuzzcheck::CoverageMap;
use fuzzcheck_view::{CodeBlock, CodeSpan, CodeSpanKind};
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
    Json(state.code_blocks.keys().cloned().collect())
}

#[get("/code_blocks?<file>")]
fn code_blocks(state: &State<ManagedData>, file: String) -> Json<Vec<CodeBlock>> {
    let mut result = vec![];
    if let Some(blocks) = state.code_blocks.get(&file) {
        result.extend_from_slice(blocks);
    }
    Json(result)
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
    let stats_folder = Path::new(&args[2]);
    println!("launching on {}", stats_folder.display());

    let coverage_map: CoverageMap = {
        let coverage_map_path = stats_folder.join("coverage_sensor.json");
        let coverage_map =
            std::fs::read(&coverage_map_path).expect(&format!("can't read {}", coverage_map_path.display()));
        let mut coverage_map: CoverageMap = serde_json::from_slice(&coverage_map).expect("can't parse coverage map");
        coverage_map.map_relative_paths(|path| {
            *path = source_folder.join(&path);
        });
        coverage_map
    };
    let hit_counters: Vec<usize> = {
        let uniq_cov_path = stats_folder.join("uniq_cov.json");
        let uniq_cov = std::fs::read(&uniq_cov_path).expect(&format!("can't read {}", uniq_cov_path.display()));
        serde_json::from_slice(&uniq_cov).expect("can't parse uniq_cov")
    };

    let mut blocks = coverage_map.code_blocks();

    for block in blocks.iter_mut() {
        for line in block.lines.iter_mut() {
            for span in line.spans.iter_mut() {
                match span.kind {
                    CodeSpanKind::Untracked => {}
                    CodeSpanKind::NotHit { id } | CodeSpanKind::Hit { id } => {
                        if hit_counters.contains(&id) {
                            span.kind = CodeSpanKind::Hit { id };
                        } else {
                            span.kind = CodeSpanKind::NotHit { id };
                        }
                    }
                }
            }
        }
    }

    let mut code_blocks_by_file = HashMap::<String, Vec<CodeBlock>>::new();
    for c in blocks {
        let entry = code_blocks_by_file.entry(c.file.clone()).or_default();
        entry.push(c);
    }

    let data = ManagedData {
        coverage_map,
        hit_counters,
        code_blocks: code_blocks_by_file,
    };

    rocket::build()
        .manage(data)
        .mount("/", routes![index, files, code_blocks, serve_static_file])
}

struct ManagedData {
    coverage_map: CoverageMap,
    code_blocks: HashMap<String, Vec<CodeBlock>>,
    hit_counters: Vec<usize>,
}
