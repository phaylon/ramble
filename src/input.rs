use crate::{Settings, ErrorKind};


/// Per-line input stream for [`FromInput`] implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input<'a> {
    settings: &'a Settings,
    content: &'a str,
    offset: usize,
}

impl<'a> Input<'a> {
    pub(crate) fn new(settings: &'a Settings, content: &'a str, offset: usize) -> Self {
        Self { settings, content, offset }
    }

    /// The rest of the available content (includes trailing comments and whitespaces).
    pub fn content(&self) -> &'a str {
        self.content
    }

    /// The [`Settings`] used to parse this input.
    pub fn settings(&self) -> &'a Settings {
        &self.settings
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
            settings: self.settings,
            content,
            offset: self.offset + (self.content.len() - content.len()),
        }
    }

    /// Skip a number of bytes of content.
    pub fn skip(&self, bytes: usize) -> Self {
        Self {
            settings: self.settings,
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
            settings: self.settings,
            content: &self.content[len..],
            offset: self.offset + len,
        }))
    }

    /// Parse until an end-condition is reached.
    pub fn parse_until<T, F>(&self, mut until: F) -> Result<(Vec<T>, Self), ErrorKind<T::Err>>
    where
        T: FromInput,
        F: FnMut(Self) -> bool,
    {
        let mut items = Vec::new();
        let mut input = *self;

        'items: loop {
            input = input.trim();
            if input.is_empty()
                || self.settings().is_skipped_line(input.content())
                || until(input)
            {
                break 'items;
            }

            let (item, rest_input) = match T::from_input(input) {
                Ok(parsed) => parsed,
                Err(error) => {
                    return Err(ErrorKind::Item {
                        error,
                        offset: input.offset(),
                    });
                },
            };

            if input.content().len() == rest_input.content().len() {
                let item_type = std::any::type_name::<T>();
                let rest_content = input.content();
                panic!("item parser `{item_type}` did not consume any input from `{rest_content}`");
            }

            input = rest_input;
            items.push(item);
        }

        Ok((items, input))
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
