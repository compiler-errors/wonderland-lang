use crate::util::{PResult, Span, Visit, Context};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, inner: T) -> Spanned<T> {
        Spanned { span, inner }
    }
}

impl<T, A> Visit<A> for Spanned<T> where T: Visit<A> {
    fn visit(self, adapter: &mut A) -> PResult<Self> {
        let Spanned { span, inner } = self;
        let inner = inner.visit(adapter).with_context(span)?;
        Ok(Spanned { inner, span })
    }
}