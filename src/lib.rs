

mod settings;
pub use settings::*;

mod input;
pub use input::*;

mod parser;
pub use parser::*;

/// The root of a tree containing [`Node`]s.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tree<A> {
    children: Vec<Node<A>>,
}

impl<A> Tree<A> {
    pub fn new(children: Vec<Node<A>>) -> Self {
        Self { children }
    }

    pub fn children(&self) -> &[Node<A>] {
        &self.children
    }

    pub fn children_mut(&mut self) -> &mut Vec<Node<A>> {
        &mut self.children
    }
}

impl<A> Default for Tree<A> {
    fn default() -> Self {
        Self {
            children: Vec::new(),
        }
    }
}

/// A node is a [`Tree`] with an additional set of item values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node<A> {
    items: Vec<A>,
    subtree: Tree<A>,
}

impl<A> Node<A> {
    pub fn new(items: Vec<A>, children: Vec<Node<A>>) -> Self {
        Self {
            items,
            subtree: Tree::new(children),
        }
    }

    pub fn with_items(items: Vec<A>) -> Self {
        Self {
            items,
            subtree: Tree::default(),
        }
    }

    pub fn with_subtree(items: Vec<A>, subtree: Tree<A>) -> Self {
        Self { items, subtree }
    }

    pub fn items(&self) -> &[A] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut Vec<A> {
        &mut self.items
    }
}

impl<A> std::ops::Deref for Node<A> {
    type Target = Tree<A>;

    fn deref(&self) -> &Self::Target {
        &self.subtree
    }
}

impl<A> std::ops::DerefMut for Node<A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.subtree
    }
}

impl<A> Default for Node<A> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            subtree: Tree::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! concatln {
        ($( $l:literal ),* $(,)?) => {
            concat!($($l, '\n'),*)
        }
    }

    macro_rules! assert_node_items {
        ($tree:expr, $($index:literal):+, $($item:literal),* $(,)?) => {
            {
                let node = &$tree;
                $(
                    let node = &node.children()[$index];
                )*
                let items = vec![$($item),*];
                assert_eq!(node.items(), &items);
            }
        }
    }

    #[test]
    fn parser() {
        let settings = &Settings::default()
            .with_indentation("\t").unwrap()
            .with_line_comment_mark(";").unwrap()
            .with_node_comment_mark("#").unwrap();

        let tree: Tree<String> = parse(concatln! {
            "first top",
            "\tfirst child",
            "\t\tfirst level2 child",
            "\t\tfirst level2 child2",
            "\tsecond child",
            "\t\tsecond level2 child",
            "\t\t#skipped child",
            "\t\t\tskipped sub child",
            "",
            "; comment",
            " ; comment don't have to follow indentation",
            "second top ; trailing spaces and comments are supported",
            "third top",
            "",
            "\tchild after empty",
        }, &settings).unwrap();

        assert_node_items!(tree, 0, "first", "top");
        assert_node_items!(tree, 0:0, "first", "child");
        assert_node_items!(tree, 0:0:0, "first", "level2", "child");
        assert_node_items!(tree, 0:0:1, "first", "level2", "child2");
        assert_node_items!(tree, 0:1, "second", "child");
        assert_node_items!(tree, 0:1:0, "second", "level2", "child");
        assert_node_items!(tree, 1, "second", "top");
        assert_node_items!(tree, 2, "third", "top");
        assert_node_items!(tree, 2:0, "child", "after", "empty");
    }
}