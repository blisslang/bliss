let DELIMITERS = "[]();\""

func isDelimiter(_ c: Character) -> Bool {
    return DELIMITERS.contains(c)
}
