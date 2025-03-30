struct Tokenizer {
    private let WHITESPACE: [Character] = [" ", "\t", "\n"]

    private func padDelimiters(in string: String) -> String {
        return String(
            Array(string).flatMap { c in
                if isDelimiter(c) {
                    return [" ", c, " "]
                } else {
                    return [c]
                }
            })
    }

    func tokenize(code: String) -> [String] {
        return padDelimiters(in: code)
            .split(omittingEmptySubsequences: true, whereSeparator: { WHITESPACE.contains($0) })
            .map(String.init)
    }
}
