pub mod args;
/// module containing definitions from the main fuzzcheck crate
pub mod fuzzcheck;

use fuzzcheck::Counter;
use rocket::form::FromFormField;
use serde::{Deserialize, Serialize};

use crate::fuzzcheck::Region;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CodeSpanKind {
    Untracked,
    Tracked { id: usize, status: CoverageStatus },
}
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CoverageStatus {
    Hit,
    NotHit,
    Best,
    Unique,
    Unknown,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CodeLine {
    pub lineno: usize,
    pub spans: Vec<CodeSpan>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CodeSpan {
    pub text: String,
    pub kind: CodeSpanKind,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FunctionCoverage {
    pub name: FunctionName,
    pub file: String,
    pub lines: Vec<CodeLine>,
    pub counter_ids: Vec<usize>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FunctionName {
    pub name: String,
    pub demangled_name: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InputFilter {
    All,
    Input(usize),
}
#[derive(Clone, Debug, Serialize, Deserialize, FromFormField)]
pub enum FunctionFilter {
    Exclude0PercentCoverage,
    Exclude100PercentCoverage,
}
#[derive(Clone, Debug, Serialize, Deserialize, FromFormField)]
pub enum CoverageKindFilter {
    All,
    LeastComplex,
    Unique,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InputInfo {
    pub pool_idx: usize,
    pub hash: String,
}
impl<'v> FromFormField<'v> for InputFilter {
    fn from_value(field: rocket::form::ValueField<'v>) -> rocket::form::Result<'v, Self> {
        match field.value {
            "all" => Ok(InputFilter::All),
            s => {
                let idx = s.parse::<usize>().unwrap();
                Ok(InputFilter::Input(idx))
            }
        }
    }
}
impl InputFilter {
    pub fn from_string(s: &str) -> Option<Self> {
        match s {
            "all" => Some(InputFilter::All),
            s => s.parse().ok().map(InputFilter::Input),
        }
    }
}

impl fuzzcheck::CoverageMap {
    pub fn functions(&self) -> Vec<FunctionCoverage> {
        let mut code_blocks = self.functions.iter().map(|f| f.coverage()).collect::<Vec<_>>();
        code_blocks.sort_by(|x, y| ((&x.file, x.lines[0].lineno)).cmp(&(&y.file, y.lines[0].lineno)));
        code_blocks
    }
}

impl fuzzcheck::Function {
    pub fn coverage(&self) -> FunctionCoverage {
        let path = &self.file;

        let name = FunctionName {
            name: self.name.clone(),
            demangled_name: rustc_demangle::demangle(&self.name).to_string(),
        };

        let file = std::fs::read_to_string(&path).unwrap();
        let lines = file.lines().collect::<Box<[_]>>();
        let sorted_counters = {
            let mut xs = self.counters.clone();
            xs.sort_by(|c1, c2| (c1.region.lines.0, c1.region.cols.0).cmp(&(c2.region.lines.0, c2.region.cols.0)));
            xs
        };

        if sorted_counters.is_empty() {
            return FunctionCoverage {
                name,
                file: format!("no counters for {} in {}", self.name, self.file.display()),
                lines: vec![],
                counter_ids: vec![],
            };
        }
        let fst_lineno = sorted_counters[0].region.lines.0.saturating_sub(1);
        let last_lineno = sorted_counters.last().unwrap().region.lines.1;

        let mut code_lines = (fst_lineno..=last_lineno)
            .map(|lineno| CodeLine { lineno, spans: vec![] })
            .collect::<Vec<_>>();

        let mut last_region = Region {
            lines: (fst_lineno, fst_lineno),
            cols: (0, 0),
        };
        for whole_counter in sorted_counters {
            let counters = split_counter_by_line(whole_counter.clone());
            for counter in counters {
                let in_between = region_between_regions(&last_region, &counter.region);
                if !region_is_empty(&in_between) {
                    let in_between = split_region_by_line(&in_between);
                    for r in in_between {
                        code_lines[r.lines.0 - fst_lineno].spans.push(CodeSpan {
                            text: string_in_region(&lines, &r),
                            kind: CodeSpanKind::Untracked,
                        })
                    }
                }
                let region = split_region_by_line(&counter.region);
                for r in region {
                    let text = string_in_region(&lines, &r);
                    if text.is_empty() && whole_counter.region.lines.0 == whole_counter.region.lines.1 {
                        code_lines[r.lines.0 - fst_lineno].spans.push(CodeSpan {
                            text: " ".to_owned(),
                            kind: CodeSpanKind::Untracked,
                        });
                        code_lines[r.lines.0 - fst_lineno].spans.push(CodeSpan {
                            text: " ⦿ ".to_owned(),
                            kind: CodeSpanKind::Tracked {
                                id: counter.id,
                                status: CoverageStatus::Unknown,
                            },
                        });
                    } else {
                        code_lines[r.lines.0 - fst_lineno].spans.push(CodeSpan {
                            text,
                            kind: CodeSpanKind::Tracked {
                                id: counter.id,
                                status: CoverageStatus::Unknown,
                            },
                        });
                    }
                }
                last_region = counter.region;
            }
        }

        let mut counter_ids = self.counters.iter().map(|c| c.id).collect::<Vec<_>>();
        counter_ids.sort();

        FunctionCoverage {
            name,
            file: format!("{}", self.file.display()),
            lines: code_lines,
            counter_ids,
        }
    }
}

fn string_in_region(lines: &[&str], region: &Region) -> String {
    let line_start = region.lines.0.saturating_sub(1);
    let line_end = if region.lines.1 >= line_start {
        region.lines.1.saturating_sub(1)
    } else {
        line_start
    };
    if line_start >= lines.len() {
        return format!("error: line_start ( {} ) > lines.len()", line_start);
    }
    let col_start = region.cols.0.saturating_sub(1);
    let col_end = region.cols.1.saturating_sub(1);

    let fst_line = lines[line_start];
    let last_line = lines[line_end];
    let start = fst_line
        .char_indices()
        .nth(col_start)
        .map(|x| x.0)
        .unwrap_or(fst_line.len());
    let end = last_line
        .char_indices()
        .nth(col_end)
        .map(|x| x.0)
        .unwrap_or(last_line.len());
    let mut span = String::new();
    if line_end == line_start {
        span.push_str(&fst_line[start..end]);
    } else {
        // add the first line
        span.push_str(&fst_line[start..]);
        // add all the lines in between
        for i in line_start + 1..line_end {
            span.push_str("\n");
            span.push_str(lines[i]);
        }
        span.push_str("\n");
        // add last line
        span.push_str(&last_line[..end]);
    }
    span
}
fn region_between_regions(a: &Region, b: &Region) -> Region {
    Region {
        lines: (a.lines.1, b.lines.0),
        cols: (a.cols.1, b.cols.0),
    }
}
fn region_is_empty(region: &Region) -> bool {
    region.lines.1 < region.lines.0 || (region.lines.1 == region.lines.0 && region.cols.1 <= region.cols.0)
}

fn split_counter_by_line(c: Counter) -> Vec<Counter> {
    let mut counters = vec![];
    for l in c.region.lines.0..=c.region.lines.1 {
        let start_col = if l == c.region.lines.0 { c.region.cols.0 } else { 0 };
        let end_col = if l == c.region.lines.1 {
            c.region.cols.1
        } else {
            usize::MAX
        };

        counters.push(Counter {
            id: c.id,
            region: Region {
                lines: (l, l),
                cols: (start_col, end_col),
            },
        })
    }
    counters
}
fn split_region_by_line(region: &Region) -> Vec<Region> {
    let mut regions = vec![];
    for l in region.lines.0..=region.lines.1 {
        let start_col = if l == region.lines.0 { region.cols.0 } else { 0 };
        let end_col = if l == region.lines.1 { region.cols.1 } else { usize::MAX };

        regions.push(Region {
            lines: (l, l),
            cols: (start_col, end_col),
        })
    }
    regions
}
