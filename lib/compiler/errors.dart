sealed class CompilerError implements Exception {
  final String message;
  const CompilerError(this.message);

  @override
  String toString() {
    return "$runtimeType: $message";
  }
}

class InvalidInputFileError extends CompilerError {
  const InvalidInputFileError(super.message);
}

class UnexpectedNodeError extends CompilerError {
  const UnexpectedNodeError(super.message);
}

class InvalidNodeTypeError extends CompilerError {
  const InvalidNodeTypeError(super.message);
}
