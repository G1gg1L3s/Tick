type BytePos = u32;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Self {
            lo: lo as BytePos,
            hi: hi as BytePos,
        }
    }

    pub fn extract<'a, 'b>(&'a self, input: &'b str) -> &'b str {
        let lo = self.lo as usize;
        let hi = self.hi as usize;
        &input[lo..hi]
    }

    pub fn to(self, hi: Self) -> Self {
        Self {
            lo: self.lo,
            hi: hi.hi,
        }
    }

    pub fn split(self, len: BytePos) -> (Self, Self) {
        let Self{ lo, hi } = self;
        let mid = lo + len;
        assert!(lo < mid && mid < hi);
        let (lo, mid, hi) = (lo as usize, mid as usize, hi as usize);
        (Span::new(lo, mid), Span::new(mid, hi))
    }
}
