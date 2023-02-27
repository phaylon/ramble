use std::marker::PhantomData;

use crate::{Settings, Tree, FromInput, Input, Node};


/// Parse a string slice into a [`Tree`](crate::Tree).
pub fn parse<A>(input: &str, settings: &Settings) -> Result<Tree<A>, Error<A::Err>>
where
    A: FromInput,
{
    let mut stack: Stack<A> = Stack::new();

    'lines: for (line_index, line) in input.lines().enumerate() {
        if settings.is_skipped_line(line) {
            continue 'lines;
        }

        let Some((depth, line)) = settings.determine_line_indentation(line) else {
            return Err(ErrorKind::IndentationInvalid.into_error(line_index));
        };

        let items = {
            let input = Input::new(settings, line, settings.indentation_len(depth));
            let items = parse_input(input)
                .map_err(|kind| kind.into_error(line_index))?;
            if settings.is_skipped_node(line) {
                StackItem::Skip
            } else {
                StackItem::Node(items)
            }
        };

        stack.insert_node(depth, items)
            .map_err(|kind| kind.into_error(line_index))?;
    }

    Ok(stack.into_tree())
}

fn parse_input<A>(input: Input<'_>) -> Result<Vec<A>, ErrorKind<A::Err>>
where
    A: FromInput,
{
    input.iter(|_input| false).collect()
}

/// A parsing item iterator produced by an [`Input`].
pub struct ItemIter<'a, T, F> {
    input: Input<'a>,
    until: F,
    is_finished: bool,
    _item: PhantomData<fn() -> T>,
}

impl<'a, T, F> ItemIter<'a, T, F>
where
    T: FromInput,
    F: FnMut(Input<'a>) -> bool,
{
    pub(crate) fn new(input: Input<'a>, until: F) -> Self {
        Self {
            input,
            until,
            is_finished: false,
            _item: PhantomData,
        }
    }

    /// Collect all items and return them with the rest [`Input`].
    pub fn collect_with_input<O>(mut self) -> Result<(O, Input<'a>), ErrorKind<T::Err>>
    where
        O: FromIterator<T>,
    {
        let collected = (&mut self).collect::<Result<O, _>>()?;
        Ok((collected, self.input))
    }
}

impl<'a, T, F> Iterator for ItemIter<'a, T, F>
where
    T: FromInput,
    F: FnMut(Input<'a>) -> bool,
{
    type Item = Result<T, ErrorKind<T::Err>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_finished {
            return None;
        }

        let input = self.input.trim();
        if input.is_empty()
            || input.settings().is_skipped_line(input.content())
            || (&mut self.until)(input)
        {
            self.is_finished = true;
            return None;
        }

        let (item, rest_input) = match T::from_input(input) {
            Ok(parsed) => parsed,
            Err(error) => {
                self.is_finished = true;
                return Some(Err(ErrorKind::Item {
                    error,
                    offset: input.offset(),
                }));
            },
        };

        if input.len() == rest_input.len() {
            let item_type = std::any::type_name::<T>();
            let rest_content = input.content();
            panic!("item parser `{item_type}` did not consume any input from `{rest_content}`");
        }

        self.input = rest_input;
        Some(Ok(item))
    }
}

struct Stack<A> {
    root: Tree<A>,
    node_stack: Vec<StackItem<Node<A>>>,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Self {
            root: Tree::default(),
            node_stack: Vec::new(),
        }
    }

    fn into_tree(mut self) -> Tree<T> {
        self.propagate_from_depth(0);
        self.root
    }

    fn is_occupied_depth(&self, depth: usize) -> bool {
        self.node_stack.len() > depth
    }

    fn insert_node<E>(
        &mut self,
        depth: usize,
        node_items: StackItem<Vec<T>>,
    ) -> Result<(), ErrorKind<E>> {
        if depth > 0 && !self.is_occupied_depth(depth - 1) {
            return Err(ErrorKind::IndentationTooDeep);
        }
        self.propagate_from_depth(depth);
        let node = match self.node_stack.last() {
            None | Some(StackItem::Node(_)) => node_items.map(|items| Node::with_items(items)),
            Some(StackItem::Skip) => StackItem::Skip,
        };
        self.node_stack.push(node);
        Ok(())
    }

    fn propagate_from_depth(&mut self, depth: usize) {
        while self.is_occupied_depth(depth) {
            if let StackItem::Node(node) = self.node_stack.pop().unwrap() {
                match self.node_stack.last_mut() {
                    None => self.root.children_mut().push(node),
                    Some(StackItem::Skip) => (),
                    Some(StackItem::Node(parent)) => parent.children_mut().push(node),
                }
            }
        }
    }
}

enum StackItem<T> {
    Node(T),
    Skip,
}

impl<T> StackItem<T> {
    fn map<U, F>(self, map_node: F) -> StackItem<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Node(node) => StackItem::Node(map_node(node)),
            Self::Skip => StackItem::Skip,
        }
    }
}

/// Details the kind of [`Error`] that occured during [`parse`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorKind<E> {
    /// The item's [`FromInput`](crate::FromInput) implementation signalled an error.
    Item {
        error: E,
        offset: usize,
    },
    /// The line contained additional whitespace that didn't fit the indentation setting.
    IndentationInvalid,
    /// The indentation level of the line is too high given it's logical parent.
    IndentationTooDeep,
}

impl<E> ErrorKind<E> {
    fn into_error(self, line_index: usize) -> Error<E> {
        Error {
            kind: self,
            line_index,
        }
    }
}

/// Signals an error occurance during [`parse`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error<E> {
    pub kind: ErrorKind<E>,
    pub line_index: usize,
}

impl<E> std::fmt::Display for Error<E>
where
    E: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line = self.line_index + 1;
        match self.kind {
            ErrorKind::Item { ref error, offset } => {
                write!(f, "Unable to parse line {line}, byte offset {offset}: {error}")
            },
            ErrorKind::IndentationInvalid => {
                write!(f, "Invalid indentation on line {line}")
            },
            ErrorKind::IndentationTooDeep => {
                write!(f, "Indentation depth is too deep at line {line}")
            },
        }
    }
}
