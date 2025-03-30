enum Node {
    indirect case List([Node])
    indirect case ValueList([Node])
    case String(String)
    case Symbol(String)
    case Number(Float)

    mutating func add(node: Node) throws(CompilerError) {
        switch self {
        case .List(var nodes):
            nodes.append(node)
            self = .List(nodes)

        case .ValueList(var nodes):
            nodes.append(node)
            self = .ValueList(nodes)

        case .String(var string):
            guard case .String(let nodeString) = node else {
                throw .invalidNodeTypeError("Expected a `String` node, got \(node)")
            }

            string += " " + nodeString
            self = .String(string)

        default:
            throw .invalidNodeTypeError("Expected a collection node, got \(node)")
        }
    }
}
