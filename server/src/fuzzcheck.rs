use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CoverageMap {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub file: String,
    pub counters: Vec<Counter>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Region {
    pub lines: (usize, usize),
    pub cols: (usize, usize),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Counter {
    pub id: usize,
    pub region: Region,
}
