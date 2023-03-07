use smol_str::SmolStr;

pub use parser::*;


mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Tree {
    pub nodes: Vec<Node>,
}

impl Tree {
    pub fn parse(content: &str) -> Result<Self, ParseError> {
        parse_str(content)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub items: Vec<Item>,
    pub subtree: Tree,
    pub location: NodeLocation,
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

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Word(SmolStr),
    Num(Num),
    Punctuation(char),
    Group(GroupKind, Vec<Item>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Num {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GroupKind {
    Braces,
    Brackets,
    Parentheses,
}
