#[macro_use]
extern crate rocket;

use fuzzcheck_view::fuzzcheck::CoverageMap;
use fuzzcheck_view::{CodeBlock, CodeSpan, CodeSpanKind};
use rocket::serde::json::Json;
use rocket::{fs::NamedFile, State};
use std::path::{Path, PathBuf};

#[get("/")]
async fn index() -> Option<NamedFile> {
    NamedFile::open(Path::new("../resources").join("index.html")).await.ok()
}

#[get("/text")]
fn text(data: &State<ManagedData>) -> String {
    format!("{:?}", data.inner().data)
}

#[get("/code_blocks")]
fn code_blocks() -> Json<Vec<CodeBlock>> {
    let map = include_bytes!("../../resources/coverage_map.json");
    let map: CoverageMap = serde_json::from_slice(map).unwrap();
    let blocks = map.code_blocks();
    let serialized = serde_json::to_string_pretty(&blocks).unwrap();
    println!("{}", serialized);
    Json(blocks)
}

// allow html to reference any file with path /static under folder "static"
#[get("/<file..>", rank = 10)] // use rank here to allow other api endpoint available as well
async fn files(file: PathBuf) -> Option<NamedFile> {
    NamedFile::open(Path::new("../resources/").join(file)).await.ok()
}

#[launch]
fn rocket() -> _ {
    println!("launching!");
    let data = ManagedData { data: vec![3, 4] };
    rocket::build()
        .manage(data)
        .mount("/", routes![index, text, code_blocks, files])
}

struct ManagedData {
    data: Vec<u8>,
}
