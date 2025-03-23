pub mod categorizer;
pub mod tokenizer;

#[derive(Debug, Clone)]
pub enum Node {
    List(Vec<Node>),
    ValueList(Vec<Node>),
    String(String),
    Symbol(String),
    Number(f32),
}

impl Node {
    pub fn add(&mut self, node: Node) -> Result<(), String> {
        match self {
            Node::List(nodes) | Node::ValueList(nodes) => {
                nodes.push(node);
                Ok(())
            }
            Node::String(string) => {
                if let Node::String(node_str) = node {
                    string.push(' ');
                    string.push_str(&node_str);
                    Ok(())
                } else {
                    Err(format!("Expected string node, got {:?}", node))
                }
            }
            _ => Err(format!("'{:?}' has to be a collection node", self)),
        }
    }
}

impl Default for Node {
    fn default() -> Self {
        Self::List(vec![])
    }
}
