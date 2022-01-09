use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CoverageMap {
    pub functions: Vec<Function>,
}
impl CoverageMap {
    pub fn map_relative_paths(&mut self, map: impl Fn(&mut PathBuf)) {
        for f in self.functions.iter_mut() {
            map(&mut f.file);
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub file: PathBuf,
    pub counters: Vec<Counter>,
    pub inferred_counters: Vec<InferredCounter>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Region {
    pub lines: (usize, usize),
    pub cols: (usize, usize),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Counter {
    pub id: usize,
    pub regions: Vec<Region>,
}
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InferredCounter {
    pub regions: Vec<Region>,
    pub from_counter_ids: Vec<usize>,
}
#[derive(Clone, Debug)]
pub struct CommonCounter {
    pub id: CounterId,
    pub regions: Vec<Region>,
}

#[derive(Clone, Debug)]
pub enum CounterId {
    Physical { id: usize },
    Inferred { from: Vec<usize> },
}

#[derive(Clone, Debug)]
pub struct CommonCounterWithSingleRegion {
    pub id: CounterId,
    pub region: Region,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializedUniqCov {
    pub all_hit_counters: Vec<usize>,
    pub best_for_counter: Vec<(usize, usize)>,
    pub ranked_inputs: Vec<usize>,
    pub counters_for_input: Vec<(usize, Vec<usize>)>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CorpusMap(pub Vec<((PathBuf, usize), String)>);

pub fn read_input_corpus(dir: &Path) -> HashMap<String, Vec<u8>> {
    let mut values = HashMap::new();
    read_input_corpus_rec(dir, &mut values);
    values
}

fn read_input_corpus_rec(corpus: &Path, values: &mut HashMap<String, Vec<u8>>) {
    if !corpus.exists() {
        return;
    }
    if !corpus.is_dir() {
        panic!()
    }
    for entry in std::fs::read_dir(corpus).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            read_input_corpus_rec(&path, values);
        } else {
            let data = std::fs::read(&path).unwrap();
            values.insert(path.file_stem().unwrap().to_str().unwrap().to_owned(), data);
        }
    }
}
