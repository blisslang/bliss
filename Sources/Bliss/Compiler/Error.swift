enum CompilerError: Error {
    case invalidInputFileError(String)
    case unexpectedNodeError(String)
    case invalidNodeTypeError(String)
}
