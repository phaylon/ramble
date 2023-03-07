
struct DisplayFn<F>(F);

pub(crate) fn display_fn<'a, F>(body: F) -> impl std::fmt::Display + 'a
where
    F: Fn(&mut std::fmt::Formatter<'_>) -> std::fmt::Result + 'a,
{
    DisplayFn(body)
}

impl<F> std::fmt::Display for DisplayFn<F>
where
    F: Fn(&mut std::fmt::Formatter<'_>) -> std::fmt::Result,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0(f)
    }
}
