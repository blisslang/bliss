use std::collections::VecDeque;

const DELIMS: &str = "[]();\"";

pub fn is_delim(c: char) -> bool {
    DELIMS.contains(c)
}

pub fn drop_while_inclusive<T, F>(vec: &mut VecDeque<T>, mut predicate: F)
where
    F: FnMut(&T) -> bool,
{
    let index = vec.iter().position(|x| !predicate(x)).unwrap_or(vec.len()) + 1;
    vec.drain(..index);
}
