use std::num::{ParseIntError, ParseFloatError};

use crate::{Tree, NodeLocation, Span, Item, ItemKind, GroupKind, Num, Node};


const INDENT: &str = "  ";
const COMMENT: char = ';';
const PUNCT: [char; 3] = [':', '!', '?'];

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
    &PUNCT,
    &GROUP_BEGIN,
    &GROUP_END,
];

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub location: NodeLocation,
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
    Unexpected(char),
    InvalidInt(ParseIntError),
    InvalidFloat(ParseFloatError),
    MissingGroupClose(char),
    UnexpectedGroupClose(char),
    GroupCloseMismatch(char, Span),
}

pub(crate) fn parse_str(content: &str) -> Result<Tree, ParseError> {
    let mut stack = TreeStack::default();
    'lines: for (line, location) in LineIter::new(content) {
        if is_skipped_line(line) {
            continue 'lines;
        }
        let (depth, line) = destruct_line(line, &location)?;
        let items = parse_line(line)?;
        let node = Node {
            items,
            location,
            subtree: Tree::default(),
        };
        stack.insert_at_depth(depth, node)?;
    }
    Ok(stack.into_tree())
}

fn parse_line(line: Line<'_>) -> Result<Vec<Item>, ParseError> {
    parse_items(line, None).map(|(items, _)| items)
}

fn parse_items<'a>(
    line: Line<'a>,
    group: Option<&GroupContext>,
) -> Result<(Vec<Item>, Line<'a>), ParseError> {
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
    line: Line<'a>,
    group: Option<&GroupContext>,
) -> Result<Option<Line<'a>>, ParseError> {
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

fn parse_item<'a>(line: Line<'a>) -> Result<(Item, Line<'a>), ParseError> {
    if let Some((c, span, rest_line, _)) = line.try_take_any_char(&PUNCT) {
        let item = Item {
            kind: ItemKind::Punctuation(c),
            inline_span: span,
        };
        Ok((item, rest_line))
    } else if let Some((_, span, rest_line, index)) = line.try_take_any_char(&GROUP_BEGIN) {
        let group = GroupContext {
            end_marker: GROUP_END[index],
            begin_span: span,
        };
        let (items, rest_line) = parse_items(rest_line, Some(&group))?;
        let item = Item {
            kind: ItemKind::Group(GROUP_KIND[index], items),
            inline_span: line.span_to(&rest_line),
        };
        Ok((item, rest_line))
    } else if let Some((term, span, rest_line)) = line.try_take_term() {
        if term.starts_with(|c: char| c.is_numeric() || ['+', '-'].contains(&c)) {
            (if term.contains('.') {
                term.parse::<f64>()
                    .map(|f| Item { kind: ItemKind::Num(Num::Float(f)), inline_span: span })
                    .map_err(|error| line.item_error(span, ParseItemErrorKind::InvalidFloat(error)))
            } else {
                term.parse::<i64>()
                    .map(|f| Item { kind: ItemKind::Num(Num::Int(f)), inline_span: span })
                    .map_err(|error| line.item_error(span, ParseItemErrorKind::InvalidInt(error)))
            }).map(|item| (item, rest_line))
        } else {
            let item = Item {
                kind: ItemKind::Word(term.into()),
                inline_span: span,
            };
            Ok((item, rest_line))
        }
    } else {
        let (c, span, _) = line.take_char().expect("item parser only applied to non-empty inputs");
        Err(line.item_error(span, ParseItemErrorKind::Unexpected(c)))
    }
}

struct GroupContext {
    end_marker: char,
    begin_span: Span,
}

fn is_skipped_line(line: &str) -> bool {
    let content = line.trim_start();
    content.is_empty() || content.starts_with(COMMENT)
}

fn destruct_line<'l>(
    mut line: &'l str,
    location: &'l NodeLocation,
) -> Result<(usize, Line<'l>), ParseError> {
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
        Ok((depth, Line {
            content: line,
            line_location: location,
            item_span_start: depth * INDENT.len(),
        }))
    }
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
struct Line<'a> {
    content: &'a str,
    line_location: &'a NodeLocation,
    item_span_start: usize,
}

impl<'a> Line<'a> {
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
        }
    }

    fn try_take_term(&self) -> Option<(&'a str, Span, Self)> {
        let rest = self.content.trim_start_matches(|c: char| {
            !c.is_whitespace() && NON_TERM.iter().all(|chars| !chars.contains(&c))
        });
        let len = self.content.len() - rest.len();
        if len > 0 {
            Some((&self.content[..len], self.span(len), self.skip(len)))
        } else {
            None
        }
    }

    fn take_char(&self) -> Option<(char, Span, Self)> {
        self.content.chars().next().map(|c| {
            let len = c.len_utf8();
            (c, self.span(len), self.skip(len))
        })
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

impl<'a> std::ops::Deref for Line<'a> {
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
    fn insert_at_depth(&mut self, depth: usize, node: Node) -> Result<(), ParseError> {
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
