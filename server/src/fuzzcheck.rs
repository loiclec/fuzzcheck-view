use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

use crate::CodeBlock;

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
