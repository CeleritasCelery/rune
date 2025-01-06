use crate::metric::Metric;

/// A position in the text buffer. This tracks both the bytes and characters.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Position {
    metric: Metric,
}

impl Position {
    pub(crate) fn new(metric: Metric) -> Self {
        Self { metric }
    }

    /// Return the number of characters before the current position.
    #[must_use]
    pub fn chars(&self) -> usize {
        self.metric.chars
    }

    /// Return the number of bytes before the current position.
    #[must_use]
    pub fn bytes(&self) -> usize {
        self.metric.bytes
    }
}
