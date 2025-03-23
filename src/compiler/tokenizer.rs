use crate::utils::is_delim;

fn pad_delims(str: &str) -> String {
    str.chars()
        .flat_map(|c| {
            if is_delim(c) {
                vec![' ', c, ' ']
            } else {
                vec![c]
            }
        })
        .collect()
}

pub fn tokenize(code: &str) -> Vec<String> {
    pad_delims(code)
        .split_whitespace()
        .map(String::from)
        .collect()
}
