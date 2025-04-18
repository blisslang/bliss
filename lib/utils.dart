const _delimiters = "[]();\"";

bool isDelim(String c) {
  return _delimiters.contains(c);
}
