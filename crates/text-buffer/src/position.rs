use crate::metric::Metric;


#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Position {
    metric: Metric,
}

impl Position {
    pub(crate) fn new(metric: Metric) -> Self {
        Self { metric }
    }

    pub fn chars(&self) -> usize {
        self.metric.chars
    }

    pub fn bytes(&self) -> usize {
        self.metric.bytes
    }
}
