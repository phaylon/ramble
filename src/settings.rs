use std::borrow::Cow;


/// Settings for [`parse`](crate::parse).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Settings {
    indentation: Cow<'static, str>,
    line_comment_mark: Option<Cow<'static, str>>,
    node_comment_mark: Option<Cow<'static, str>>,
}

impl Settings {

    /// Construct settings with only indentation.
    pub fn new(indentation: Cow<'static, str>) -> Result<Self, SettingsError> {
        if indentation.len() == 0 {
            Err(SettingsError::EmptyToken)
        } else {
            Ok(Self {
                indentation,
                line_comment_mark: None,
                node_comment_mark: None,
            })
        }
    }

    /// Full construction from static string slices for constant contexts.
    ///
    /// # Panics
    ///
    /// This function will panic if any of the provided tokens are invalid.
    pub const fn from_static(
        indentation: &'static str,
        line_comment_mark: Option<&'static str>,
        node_comment_mark: Option<&'static str>,
    ) -> Self {
        assert!(indentation.len() > 0, "indentation length cannot be empty");
        if let Some(mark) = line_comment_mark {
            assert!(mark.len() > 0, "line comment mark cannot be empty");
        }
        if let Some(mark) = node_comment_mark {
            assert!(mark.len() > 0, "node comment mark cannot be empty");
        }
        Self {
            indentation: Cow::Borrowed(indentation),
            line_comment_mark: match line_comment_mark {
                Some(mark) => Some(Cow::Borrowed(mark)),
                None => None,
            },
            node_comment_mark: match node_comment_mark {
                Some(mark) => Some(Cow::Borrowed(mark)),
                None => None,
            },
        }
    }

    /// Set indentation.
    pub fn with_indentation<T>(mut self, indentation: T) -> Result<Self, SettingsError>
    where
        T: Into<Cow<'static, str>>,
    {
        let indentation = indentation.into();
        if indentation.is_empty() {
            Err(SettingsError::EmptyToken)
        } else {
            self.indentation = indentation;
            Ok(self)
        }
    }

    /// Set line comment mark.
    pub fn with_line_comment_mark<T>(mut self, mark: T) -> Result<Self, SettingsError>
    where
        T: Into<Cow<'static, str>>,
    {
        let mark = mark.into();
        if mark.is_empty() {
            Err(SettingsError::EmptyToken)
        } else {
            self.line_comment_mark = Some(mark.into());
            Ok(self)
        }
    }

    /// Unset line comment mark.
    pub fn without_line_comment_mark(mut self) -> Self {
        self.line_comment_mark = None;
        self
    }

    /// Set node comment mark.
    pub fn with_node_comment_mark<T>(mut self, mark: T) -> Result<Self, SettingsError>
    where
        T: Into<Cow<'static, str>>,
    {
        let mark = mark.into();
        if mark.is_empty() {
            Err(SettingsError::EmptyToken)
        } else {
            self.node_comment_mark = Some(mark.into());
            Ok(self)
        }
    }

    /// Unset node comment mark.
    pub fn without_node_comment_mark(mut self) -> Self {
        self.node_comment_mark = None;
        self
    }

    pub(crate) fn is_skipped_node(&self, line: &str) -> bool {
        self.node_comment_mark.as_ref().map_or(false, |mark| {
            line.trim_start().starts_with(mark.as_ref())
        })
    }

    pub(crate) fn is_skipped_line(&self, line: &str) -> bool {
        line.chars().all(|c| c.is_whitespace())
        || self.line_comment_mark.as_ref().map_or(false, |mark| {
            line.trim_start().starts_with(mark.as_ref())
        })
    }

    pub(crate) fn determine_line_indentation<'a>(&self, mut line: &'a str) -> Option<(usize, &'a str)> {
        let mut indentation = 0;
        while let Some(rest) = line.strip_prefix(self.indentation.as_ref()) {
            indentation += 1;
            line = rest;
        }
        if line.starts_with(|c: char| c.is_whitespace()) {
            None
        } else {
            Some((indentation, line))
        }
    }

    pub(crate) fn indentation_len(&self, depth: usize) -> usize {
        depth * self.indentation.len()
    }
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            indentation: "    ".into(),
            line_comment_mark: None,
            node_comment_mark: None,
        }
    }
}

/// Signals invalid [`Settings`] states.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SettingsError {
    EmptyToken,
}

impl std::fmt::Display for SettingsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The supplied token value is empty")
    }
}