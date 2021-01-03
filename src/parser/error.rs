use super::span::Span;

#[derive(Debug)]
pub struct PError {
    pub msg: String,
    pub span: Span,
}

impl PError {
    pub fn new<T: Into<String>>(msg: T, span: Span) -> Self {
        Self {
            msg: msg.into(),
            span,
        }
    }
}
