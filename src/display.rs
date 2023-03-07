use std::fmt::{Display, Result, Formatter};


struct DisplayFn<F>(F);

pub(crate) fn display_fn<'a, F>(body: F) -> impl Display + 'a
where
    F: Fn(&mut Formatter<'_>) -> Result + 'a,
{
    DisplayFn(body)
}

impl<F> Display for DisplayFn<F>
where
    F: Fn(&mut Formatter<'_>) -> Result,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0(f)
    }
}
