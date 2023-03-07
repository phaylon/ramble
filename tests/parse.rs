
use assert_matches::assert_matches;
use ramble::{Tree, ParseError, Span, GroupKind, ParseErrorKind, ParseItemErrorKind, NumError, NodeLocation};


fn deindent(content: &str) -> String {
    let common = content.lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim().len())
        .min()
        .unwrap_or(0);
    content.lines()
        .map(|line| if line.trim().is_empty() { "" } else { &line[common..] })
        .fold(String::new(), |s, line| s + line + "\n")
}

fn tree(content: &str) -> Result<Tree, ParseError> {
    Tree::parse(&deindent(content), &['!'])
}

#[test]
fn depth_structure() {
    let t = tree("
        a
          b
          c

        d
        e
          f
    ").unwrap();

    assert_eq!(t.nodes.len(), 3);

    assert_eq!(t[0].nodes.len(), 2);
    assert_eq!(t[0].items[0].word_str(), Some("a"));
    assert_eq!(t[0][0].items[0].word_str(), Some("b"));
    assert_eq!(t[0][1].items[0].word_str(), Some("c"));
    assert_eq!(t[0][1].items[0].inline_span, Span { start: 2, len: 1 });

    assert_eq!(t[1].nodes.len(), 0);
    assert_eq!(t[1].items[0].word_str(), Some("d"));

    assert_eq!(t[2].nodes.len(), 1);
    assert_eq!(t[2].items[0].word_str(), Some("e"));
    assert_eq!(t[2][0].items[0].word_str(), Some("f"));

    assert_eq!(t[0].location.line_index, 1);
    assert_eq!(t[2].location.line_index, 6);

    assert_matches!(tree("
        a
         b
    "), Err(ParseError {
        kind: ParseErrorKind::InvalidIndentation,
        location: NodeLocation {
            line_index: 2,
            line_span: Span { start: 3, len: 2 },
        },
        ..
    }));

    assert_matches!(tree("
        a
            b
    "), Err(ParseError {
        kind: ParseErrorKind::InvalidIndentationDepth,
        location: NodeLocation {
            line_index: 2,
            line_span: Span { start: 3, len: 5 },
        },
        ..
    }));
}

#[test]
fn symbol_items() {
    let t = tree("
        foo
        foo!
        foo;
        foo()
    ").unwrap();
    for node in &t.nodes {
        assert_eq!(node.items[0].word_str(), Some("foo"));
        assert_eq!(node.items[0].inline_span, Span { start: 0, len: 3 });
    }
}

#[test]
fn punctuation_items() {
    let t = tree("
        foo!
    ").unwrap();
    assert_eq!(t[0].items[1].punctuation(), Some('!'));
    assert_eq!(t[0].items[1].inline_span, Span { start: 3, len: 1 });
}

#[test]
fn number_items() {
    let t = tree("
        23
        -23
        0.0
        -0.0
    ").unwrap();
    assert_eq!(t[0].items[0].int(), Some(23));
    assert_eq!(t[1].items[0].int(), Some(-23));
    assert_eq!(t[2].items[0].float(), Some(0.0));
    assert_eq!(t[3].items[0].float(), Some(-0.0));

    assert_matches!(tree("23foo"), Err(ParseError {
        kind: ParseErrorKind::Item {
            kind: ParseItemErrorKind::InvalidNum(NumError::Int(_)),
            span: Span { start: 0, len: 5 },
        },
        ..
    }));

    assert_matches!(tree("23.foo"), Err(ParseError {
        kind: ParseErrorKind::Item {
            kind: ParseItemErrorKind::InvalidNum(NumError::Float(_)),
            span: Span { start: 0, len: 6 },
        },
        ..
    }));
}

#[test]
fn comments() {
    let t = tree("
        ; a
         ;  y
        b; c
    ").unwrap();
    assert_eq!(t[0].items.len(), 1);
    assert_eq!(t[0].items[0].word_str(), Some("b"));
    assert_eq!(t[0].location.line_index, 3);
}

#[test]
fn group_items() {
    use GroupKind::*;
    let t = tree("
        (a b) ()
        [a b] []
        {a b} {}
    ").unwrap();
    for (node, kind) in t.nodes.into_iter().zip([Parentheses, Brackets, Braces]) {
        assert_eq!(node.items.len(), 2);

        assert_eq!(node.items[0].inline_span, Span { start: 0, len: 5 });
        let group = node.items[0].group().unwrap();
        assert_eq!(group.0, kind);
        assert_eq!(group.1.len(), 2);
        assert_eq!(group.1[0].word_str(), Some("a"));
        assert_eq!(group.1[0].inline_span, Span { start: 1, len: 1 });
        assert_eq!(group.1[1].word_str(), Some("b"));
        assert_eq!(group.1[1].inline_span, Span { start: 3, len: 1 });

        assert_eq!(node.items[1].inline_span, Span { start: 6, len: 2 });
        let group = node.items[1].group().unwrap();
        assert_eq!(group.0, kind);
        assert_eq!(group.1.len(), 0);
    }

    assert_matches!(tree("a(b"), Err(ParseError {
        kind: ParseErrorKind::Item {
            kind: ParseItemErrorKind::MissingGroupClose(')'),
            span: Span { start: 1, len: 1 },
        },
        ..
    }));

    assert_matches!(tree("a)b"), Err(ParseError {
        kind: ParseErrorKind::Item {
            kind: ParseItemErrorKind::UnexpectedGroupClose(')'),
            span: Span { start: 1, len: 1 },
        },
        ..
    }));

    assert_matches!(tree("a(b]c"), Err(ParseError {
        kind: ParseErrorKind::Item {
            kind: ParseItemErrorKind::GroupCloseMismatch(']', Span { start: 1, len: 1 }),
            span: Span { start: 3, len: 1 },
        },
        ..
    }));
}