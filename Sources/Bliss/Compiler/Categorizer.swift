struct Categorizer {
    private var stack: [Node] = []
    private var current: Node = .List([])

    private func toAtom(_ token: String) -> Node {
        switch Float(token) {
        case .some(let float): return .Number(float)
        case .none: return .Symbol(token)
        }
    }

    private func toString(_ token: String) -> Node {
        return .String(token)
    }

    mutating func categorizeOpening(newCurrent: Node) {
        stack.append(current)
        current = newCurrent
    }

    mutating func categorizeClosing(stringRepr: String) throws(CompilerError) {
        guard case .some(var newCurrent) = stack.popLast() else {
            throw .unexpectedNodeError("Unexpected node: \(stringRepr)")
        }

        try newCurrent.add(node: current)
        current = newCurrent
    }

    mutating func categorizeElse(_ token: String, typeFun: (_ token: String) -> Node)
        throws(CompilerError)
    {
        try current.add(node: typeFun(token))
    }

    mutating func categorizeComment(_ tokens: inout [String]) {
        tokens = Array(
            tokens
                .drop { $0 != ";" }
                .dropFirst()
        )
    }

    mutating func categorize(tokens inTokens: [String]) throws(CompilerError) -> Node {
        var tokens = inTokens

        while tokens.count > 0 {
            let token = tokens.remove(at: 0)

            switch current {
            case .String(let stringValue):
                switch token {
                case "\"" where stringValue.hasSuffix("\\"):
                    try categorizeClosing(stringRepr: "\"")

                default:
                    try categorizeElse(token, typeFun: toString)
                }

            default:
                switch token {
                case ";": categorizeComment(&tokens)

                case "(": categorizeOpening(newCurrent: .List([]))
                case "[": categorizeOpening(newCurrent: .List([]))
                case "\"": categorizeOpening(newCurrent: .String(""))

                case ")": try categorizeClosing(stringRepr: ")")
                case "]": try categorizeClosing(stringRepr: "]")

                default: try categorizeElse(token, typeFun: toAtom)
                }
            }
        }

        return current
    }
}
