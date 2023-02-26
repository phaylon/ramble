
/// Per-line input stream for [`FromInput`] implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input<'a> {
    content: &'a str,
    offset: usize,
}

impl<'a> Input<'a> {
    pub(crate) fn new(content: &'a str, offset: usize) -> Self {
        Self { content, offset }
    }

    /// The rest of the available content (includes trailing comments and whitespaces).
    pub fn content(&self) -> &'a str {
        self.content
    }

    pub(crate) fn offset(&self) -> usize {
        self.offset
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.content.is_empty()
    }

    /// Trim leading whitespace.
    pub fn trim(&self) -> Self {
        let content = self.content.trim_start();
        Self {
            content,
            offset: self.offset + (self.content.len() - content.len()),
        }
    }

    /// Skip a number of bytes of content.
    pub fn skip(&self, bytes: usize) -> Self {
        Self {
            content: &self.content[bytes..],
            offset: self.offset + bytes,
        }
    }

    /// Extract a slice of matching characters.
    pub fn matching<F>(&self, matcher: F) -> Option<(&'a str, Self)>
    where
        F: FnMut(char) -> bool,
    {
        let rest = self.content.trim_start_matches(matcher);
        let len = self.content.len() - rest.len();
        if len == 0 {
            return None;
        }
        Some((&self.content[..len], Self {
            content: &self.content[len..],
            offset: self.offset + len,
        }))
    }
}

impl std::ops::Deref for Input<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.content
    }
}

/// Parse a single item from an [`Input`] stream.
pub trait FromInput: Sized {
    type Err;

    fn from_input<'a>(input: Input<'a>) -> Result<(Self, Input<'a>), Self::Err>;
}

macro_rules! impl_str {
    ($t:ty : |$s:ident| $e:expr) => {
        impl FromInput for $t {
            type Err = ();

            fn from_input<'a>(input: Input<'a>) -> Result<(Self, Input<'a>), Self::Err> {
                input.matching(|c| !c.is_whitespace())
                    .map(|($s, rest)| { ($e, rest) })
                    .ok_or(())
            }
        }
    }
}

impl_str!(String: |s| s.into());
impl_str!(std::borrow::Cow<'static, str>: |s| std::borrow::Cow::Owned(s.into()));
impl_str!(std::sync::Arc<str>: |s| s.into());
impl_str!(std::rc::Rc<str>: |s| s.into());
