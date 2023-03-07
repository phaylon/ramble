use std::num::{ParseIntError, ParseFloatError};

use smol_str::SmolStr;

pub use parser::*;


mod parser;
mod display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

impl Span {
    pub fn end(self) -> usize {
        self.start + self.len
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Tree {
    pub nodes: Vec<Node>,
}

impl Tree {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse(content: &str, punctuation: &[char]) -> Result<Self, ParseError> {
        parse_str(content, punctuation)
    }
}

impl std::ops::Index<usize> for Tree {
    type Output = Node;

    #[track_caller]
    fn index(&self, index: usize) -> &Self::Output {
        &self.nodes[index]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub items: Vec<Item>,
    pub subtree: Tree,
    pub location: NodeLocation,
}

impl Node {
    pub fn new(location: NodeLocation) -> Self {
        Self::with_items(location, Vec::new())
    }

    pub fn with_items(location: NodeLocation, items: Vec<Item>) -> Self {
        Self {
            subtree: Tree::default(),
            items,
            location,
        }
    }
}

impl std::ops::Deref for Node {
    type Target = Tree;

    fn deref(&self) -> &Self::Target {
        &self.subtree
    }
}

impl std::ops::DerefMut for Node {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.subtree
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeLocation {
    pub line_index: usize,
    pub line_span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub inline_span: Span,
}

macro_rules! fn_item_variant {
    ($name:ident -> $output:ty : $case:pat => $produce:expr) => {
        pub fn $name(&self) -> Option<$output> {
            if let $case = &self.kind {
                Some($produce)
            } else {
                None
            }
        }
    }
}

impl Item {
    fn_item_variant!(word -> &SmolStr: ItemKind::Word(word) => word);
    fn_item_variant!(word_str -> &str: ItemKind::Word(word) => word);
    fn_item_variant!(num -> Num: ItemKind::Num(num) => *num);
    fn_item_variant!(int -> i64: ItemKind::Num(Num::Int(i)) => *i);
    fn_item_variant!(float -> f64: ItemKind::Num(Num::Float(f)) => *f);
    fn_item_variant!(punctuation -> char: ItemKind::Punctuation(c) => *c);
    fn_item_variant!(group -> (GroupKind, &[Item]): ItemKind::Group(g, i) => (*g, i));
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Word(SmolStr),
    Num(Num),
    Punctuation(char),
    Group(GroupKind, Vec<Item>),
}

impl ItemKind {
    pub fn at(self, inline_span: Span) -> Item {
        Item { kind: self, inline_span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Num {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumError {
    Int(ParseIntError),
    Float(ParseFloatError),
}

impl std::fmt::Display for NumError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumError::Int(error) => error.fmt(f),
            NumError::Float(error) => error.fmt(f),
        }
    }
}

impl std::str::FromStr for Num {
    type Err = NumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains('.') {
            s.parse().map(Num::Float).map_err(NumError::Float)
        } else {
            s.parse().map(Num::Int).map_err(NumError::Int)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GroupKind {
    Braces,
    Brackets,
    Parentheses,
}
