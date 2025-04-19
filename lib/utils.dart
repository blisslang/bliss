const _delimiters = "[]();\"";

bool isDelim(String c) {
  return _delimiters.contains(c);
}

String camelize(String input) {
  return input.split("-").indexed.map((elem) {
    return elem.$1 > 0
        ? "${elem.$2[0].toUpperCase()}${elem.$2.substring(1)}"
        : elem.$2;
  }).join();
}

final _validIdentifierCharsRegex = RegExp("[a-zA-Z0-9]");

String escapeInvalidChars(String input) {
  return input.splitMapJoin(
    _validIdentifierCharsRegex,
    onNonMatch: (c) {
      if (c.isEmpty) return "";

      print("ESCAPE INVALID CHARS C: '$c'");
      return "\$\$${c.runes.first}\$\$";
    },
  );
}
