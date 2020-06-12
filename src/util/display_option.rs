pub enum DisplayOption<T> {
    Some(T),
    None,
}

impl<T> std::fmt::Display for DisplayOption<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let DisplayOption::Some(s) = self {
            s.fmt(f)
        } else {
            write!(f, "<NONE>")
        }
    }
}

pub trait IntoDisplay<T> {
    fn display(self) -> DisplayOption<T>;
}

impl<T> IntoDisplay<T> for Option<T> {
    fn display(self) -> DisplayOption<T> {
        match self {
            Some(t) => DisplayOption::Some(t),
            None => DisplayOption::None,
        }
    }
}
