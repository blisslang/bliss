use crate::utils::is_delim;

fn pad_delims(str: &str) -> String {
    str.chars()
        .flat_map(|c| {
            if is_delim(&c) {
                vec![' ', c, ' ']
            } else {
                vec![c]
            }
        })
        .collect()
}

fn do_tokenize(input: &str) -> Vec<String> {
    let mut tokens: Vec<String> = vec![];
    let mut chars = input.trim().chars().peekable();

    while let Some(&c) = chars.peek() {
        // Start of a string
        if c == '"' {
            chars.next();
            tokens.push("\"".to_string());

            let string_content = chars.by_ref().take_while(|&c| c != '"').collect::<String>();
            tokens.push(string_content);

            chars.next();
            tokens.push("\"".to_string());
        }
        // Start of a comment
        else if c == ';' {
            chars.next();

            chars.by_ref().take_while(|&c| c != ';').for_each(drop);

            chars.next();
        }
        // Regular word
        else if !c.is_whitespace() {
            let word = chars
                .by_ref()
                .take_while(|c| !c.is_whitespace())
                .collect::<String>();

            tokens.push(word);
        // Whitespace
        } else {
            while let Some(&next_char) = chars.peek() {
                if !next_char.is_whitespace() {
                    break;
                }
                chars.next();
            }
        }
    }

    tokens
}

pub fn tokenize(code: &str) -> Vec<String> {
    let padded_code = pad_delims(code);

    do_tokenize(&padded_code)
}
