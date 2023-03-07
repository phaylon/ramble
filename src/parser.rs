
use crate::display::display_fn;
use crate::{Tree, NodeLocation, Span, Item, ItemKind, GroupKind, Node, NumError};


const INDENT: &str = "  ";
const COMMENT: char = ';';

const GROUP_LEN: usize = 3;
const GROUP_BEGIN: [char; GROUP_LEN] = ['(', '[', '{'];
const GROUP_END: [char; GROUP_LEN] = [')', ']', '}'];
const GROUP_KIND: [GroupKind; GROUP_LEN] = [
    GroupKind::Parentheses,
    GroupKind::Brackets,
    GroupKind::Braces,
];

const NON_TERM: &[&[char]] = &[
    &[COMMENT],
    &GROUP_BEGIN,
    &GROUP_END,
];

pub(crate) fn parse_str(content: &str, punctuation: &[char]) -> Result<Tree, ParseError> {
    let mut stack = TreeStack::default();
    'lines: for (full_line, location) in LineIter::new(content) {
        if is_skipped_line(full_line) {
            continue 'lines;
        }
        let (depth, line_input) = LineInput::new(full_line, &location, punctuation)?;
        let items = parse_line(line_input)?;
        let node = Node::with_items(location, items);
        stack.insert_at_depth(depth, node)?;
    }
    Ok(stack.into_tree())
}

fn parse_line(line: LineInput<'_>) -> Result<Vec<Item>, ParseError> {
    parse_items(line, None).map(|(items, _)| items)
}

fn parse_items<'a>(
    line: LineInput<'a>,
    group: Option<&GroupContext>,
) -> Result<(Vec<Item>, LineInput<'a>), ParseError> {
    let mut items = Vec::new();
    let mut line = line.trim();
    while !line.is_empty() {
        if let Some(rest_line) = try_parse_group_end(line, group)? {
            return Ok((items, rest_line));
        }
        let (item, rest_line) = parse_item(line)?;
        line = rest_line.trim();
        items.push(item);
    }
    if let Some(group) = group {
        Err(line.item_error(
            group.begin_span,
            ParseItemErrorKind::MissingGroupClose(group.end_marker),
        ))
    } else {
        Ok((items, line))
    }
}

fn try_parse_group_end<'a>(
    line: LineInput<'a>,
    group: Option<&GroupContext>,
) -> Result<Option<LineInput<'a>>, ParseError> {
    if let Some((c, span, rest_line, _)) = line.try_take_any_char(&GROUP_END) {
        if let Some(group) = group {
            if group.end_marker == c {
                Ok(Some(rest_line))
            } else {
                Err(line.item_error(
                    span,
                    ParseItemErrorKind::GroupCloseMismatch(c, group.begin_span),
                ))
            }
        } else {
            Err(line.item_error(
                span,
                ParseItemErrorKind::UnexpectedGroupClose(c),
            ))
        }
    } else {
        Ok(None)
    }
}

fn parse_item<'a>(line: LineInput<'a>) -> Result<(Item, LineInput<'a>), ParseError> {
    if let Some((_, begin_span, rest_line, index)) = line.try_take_any_char(&GROUP_BEGIN) {
        let group = GroupContext { end_marker: GROUP_END[index], begin_span };
        let (items, rest_line) = parse_items(rest_line, Some(&group))?;
        let item = ItemKind::Group(GROUP_KIND[index], items).at(line.span_to(&rest_line));
        Ok((item, rest_line))
    } else if let Some((c, span, rest_line, _)) = line.try_take_any_char(line.punctuation) {
        let item = ItemKind::Punctuation(c).at(span);
        Ok((item, rest_line))
    } else if let Some((term, span, rest_line)) = line.try_take_term() {
        if term.starts_with(|c: char| c.is_numeric() || c == '-') {
            term.parse()
                .map(|n| (ItemKind::Num(n).at(span), rest_line))
                .map_err(|error| line.item_error(span, ParseItemErrorKind::InvalidNum(error)))
        } else {
            let item = ItemKind::Word(term.into()).at(span);
            Ok((item, rest_line))
        }
    } else {
        unreachable!("unexpected input char `{:?}`", line.content.chars().next())
    }
}

fn is_skipped_line(line: &str) -> bool {
    let content = line.trim_start();
    content.is_empty() || content.starts_with(COMMENT)
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub location: NodeLocation,
}

impl ParseError {
    pub fn spans(&self) -> impl Iterator<Item = Span> + '_ {
        match &self.kind {
            ParseErrorKind::Item { kind, span } => match kind {
                ParseItemErrorKind::InvalidNum(_) |
                ParseItemErrorKind::MissingGroupClose(_) |
                ParseItemErrorKind::UnexpectedGroupClose(_) => {
                    SpanIter::Single(*span)
                },
                ParseItemErrorKind::GroupCloseMismatch(_, begin_span) => {
                    SpanIter::from_multiple(*begin_span, *span)
                },
            },
            ParseErrorKind::InvalidIndentation |
            ParseErrorKind::InvalidIndentationDepth => {
                SpanIter::Empty
            },
        }
    }

    pub fn fmt_message(&self) -> impl std::fmt::Display + '_ {
        display_fn(|f| {
            match &self.kind {
                ParseErrorKind::Item { kind, .. } => match kind {
                    ParseItemErrorKind::InvalidNum(num_error) => {
                        write!(f, "Cannot parse numeric value because {num_error}")
                    },
                    ParseItemErrorKind::MissingGroupClose(c) => {
                        write!(f, "Missing corresponding group close character `{c}`")
                    },
                    ParseItemErrorKind::UnexpectedGroupClose(c) => {
                        write!(f, "Unexpected group close character `{c}`")
                    },
                    ParseItemErrorKind::GroupCloseMismatch(c, _) => {
                        write!(f, "Wrong close character `{c}` for current group")
                    },
                },
                ParseErrorKind::InvalidIndentation => {
                    write!(f, "Indentation is not a multiple of 2")
                },
                ParseErrorKind::InvalidIndentationDepth => {
                    write!(f, "Indentation is too deep for this level")
                },
            }
        })
    }

    pub fn fmt_location(&self) -> impl std::fmt::Display + '_ {
        display_fn(|f| {
            let line_number = self.location.line_index + 1;
            if let ParseErrorKind::Item { span, .. } = self.kind {
                let byte_column = span.start;
                write!(f, "line {line_number}, byte column {byte_column}")
            } else {
                write!(f, "line {line_number}")
            }
        })
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} on {}", self.fmt_message(), self.fmt_location())
    }
}

#[derive(Clone)]
enum SpanIter {
    Empty,
    Single(Span),
    Multiple(Span, Span),
}

impl SpanIter {
    fn from_multiple(mut a: Span, mut b: Span) -> Self {
        if a.start > b.start {
            std::mem::swap(&mut a, &mut b);
        }
        if a.end() >= b.start {
            let start = a.start;
            let end = a.end().max(b.end());
            Self::Single(Span { start, len: end - start })
        } else {
            Self::Multiple(a, b)
        }
    }
}

impl Iterator for SpanIter {
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Self::Empty => None,
            Self::Single(span) => {
                *self = Self::Empty;
                Some(span)
            },
            Self::Multiple(span, left) => {
                *self = Self::Single(left);
                Some(span)
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    Item {
        kind: ParseItemErrorKind,
        span: Span,
    },
    InvalidIndentation,
    InvalidIndentationDepth,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseItemErrorKind {
    InvalidNum(NumError),
    MissingGroupClose(char),
    UnexpectedGroupClose(char),
    GroupCloseMismatch(char, Span),
}

struct GroupContext {
    end_marker: char,
    begin_span: Span,
}

struct LineIter<'a> {
    content: &'a str,
    line_index: usize,
    line_span_start: usize,
}

impl<'a> LineIter<'a> {
    fn new(content: &'a str) -> Self {
        LineIter {
            content,
            line_index: 0,
            line_span_start: 0,
        }
    }
}

impl<'a> Iterator for LineIter<'a> {
    type Item = (&'a str, NodeLocation);

    fn next(&mut self) -> Option<Self::Item> {
        if self.content.is_empty() {
            return None;
        }
        let (line_content_len, newline_len) = self.content.find('\n')
            .map_or_else(|| (self.content.len(), 0), |index| (index, 1));
        let full_line_len = line_content_len + newline_len;
        let content = &self.content[..line_content_len];
        let span = Span {
            start: self.line_span_start,
            len: line_content_len,
        };
        let location = NodeLocation {
            line_index: self.line_index,
            line_span: span,
        };
        self.content = &self.content[full_line_len..];
        self.line_index += 1;
        self.line_span_start += full_line_len;
        Some((content, location))
    }
}

#[derive(Clone, Copy)]
struct LineInput<'a> {
    content: &'a str,
    punctuation: &'a [char],
    line_location: &'a NodeLocation,
    item_span_start: usize,
}

impl<'a> LineInput<'a> {
    fn new(
        mut line: &'a str,
        location: &'a NodeLocation,
        punctuation: &'a [char],
    ) -> Result<(usize, Self), ParseError> {
        let mut depth = 0;
        while let Some(rest) = line.strip_prefix(INDENT) {
            depth += 1;
            line = rest;
        }
        if line.starts_with(char::is_whitespace) {
            Err(ParseError {
                kind: ParseErrorKind::InvalidIndentation,
                location: location.clone(),
            })
        } else {
            Ok((depth, Self {
                content: line,
                line_location: location,
                item_span_start: depth * INDENT.len(),
                punctuation,
            }))
        }
    }

    fn item_error(&self, span: Span, kind: ParseItemErrorKind) -> ParseError {
        ParseError {
            kind: ParseErrorKind::Item { span, kind },
            location: self.line_location.clone(),
        }
    }

    fn span(&self, len: usize) -> Span {
        Span {
            start: self.item_span_start,
            len,
        }
    }

    fn span_to(&self, other: &Self) -> Span {
        Span {
            start: self.item_span_start,
            len: other.item_span_start.checked_sub(self.item_span_start)
                .expect("correct line input order for ranged span construction"),
        }
    }

    fn skip(&self, len: usize) -> Self {
        Self {
            content: &self.content[len..],
            line_location: self.line_location,
            item_span_start: self.item_span_start + len,
            punctuation: self.punctuation,
        }
    }

    fn try_take_term(&self) -> Option<(&'a str, Span, Self)> {
        let rest = self.content.trim_start_matches(|c: char| {
            !c.is_whitespace()
            && !self.punctuation.contains(&c)
            && NON_TERM.iter().all(|nt_chars| !nt_chars.contains(&c))
        });
        let len = self.content.len() - rest.len();
        if len > 0 {
            Some((&self.content[..len], self.span(len), self.skip(len)))
        } else {
            None
        }
    }

    fn try_take_any_char(&self, chars: &[char]) -> Option<(char, Span, Self, usize)> {
        let next = self.content.chars().next()?;
        for (index, searched) in chars.iter().copied().enumerate() {
            if searched == next {
                let len = next.len_utf8();
                return Some((next, self.span(len), self.skip(len), index));
            }
        }
        None
    }

    fn trim(&self) -> Self {
        let rest = self.content.trim_start();
        let rest = if rest.starts_with(COMMENT) { "" } else { rest };
        self.skip(self.content.len() - rest.len())
    }
}

impl<'a> std::ops::Deref for LineInput<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.content
    }
}

#[derive(Default)]
struct TreeStack {
    tree: Tree,
    levels: Vec<Node>,
}

impl TreeStack {
    fn insert_at_depth(
        &mut self,
        depth: usize,
        node: Node,
    ) -> Result<(), ParseError> {
        self.vacate_depth(depth);
        if self.levels.len() == depth {
            self.levels.push(node);
            Ok(())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::InvalidIndentationDepth,
                location: node.location,
            })
        }
    }

    fn into_tree(mut self) -> Tree {
        self.vacate_depth(0);
        self.tree
    }

    fn vacate_depth(&mut self, depth: usize) {
        while self.levels.len() > depth {
            let lowest_node = self.levels.pop().unwrap();
            self.levels.last_mut()
                .map(|node| &mut node.subtree.nodes)
                .unwrap_or(&mut self.tree.nodes)
                .push(lowest_node);
        }
    }
}
