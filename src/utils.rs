pub fn is_delim(c: &char) -> bool {
    ['[', ']', '(', ')', ';'].contains(c)
}
