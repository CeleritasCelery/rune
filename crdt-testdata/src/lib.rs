use libflate::gzip::Decoder;
use serde::Deserialize;
use std::fs::File;
use std::io::{BufReader, Read};

/// This file contains some simple helpers for loading test data. Its used by benchmarking and
/// testing code.

/// (position, delete length, insert content).
#[derive(Debug, Clone, Deserialize)]
pub struct TestPatch(pub usize, pub usize, pub String);

#[derive(Debug, Clone, Deserialize)]
pub struct TestTxn {
    // time: String, // ISO String. Unused.
    pub patches: Vec<TestPatch>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct TestData {
    #[serde(rename = "startContent")]
    pub start_content: String,
    #[serde(rename = "endContent")]
    pub end_content: String,

    pub txns: Vec<TestTxn>,
}

impl TestData {
    pub fn len(&self) -> usize {
        self.txns.iter().map(|txn| txn.patches.len()).sum::<usize>()
    }

    pub fn is_empty(&self) -> bool {
        !self.txns.iter().any(|txn| !txn.patches.is_empty())
    }
}

pub fn load_testing_data(filename: &str) -> TestData {
    let file = File::open(filename).unwrap();
    let input = BufReader::new(file);
    let mut decoder = Decoder::new(input).unwrap();
    let mut raw_json = vec![];
    decoder.read_to_end(&mut raw_json).unwrap();
    serde_json::from_reader(raw_json.as_slice()).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::load_testing_data;

    #[test]
    fn it_works() {
        let data = load_testing_data("data/sveltecomponent.json.gz");
        assert!(!data.txns.is_empty());
    }
}
