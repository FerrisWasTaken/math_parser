use crate::{Expr, Atom};

pub trait Visitor {
    fn visit_binary_op(&mut self, b: &Expr) {
        walk_binary_op(self, b);
    }
    #[allow(unused_variables)]
    fn visit_atom(&mut self, atom: &Atom) {}
    fn visit_expr(&mut self, e: &Expr) {
        walk_expr(self, e);
    }
}

/// walk through a binary operation
pub fn walk_binary_op<V: Visitor + ?Sized>(visitor: &mut V, b: &Expr) {
    if let Expr::BinOp { ref lhs, op: _, ref rhs } = b {
    visitor.visit_expr(lhs);
    visitor.visit_expr(rhs);
}}

/// walk through a expression
pub fn walk_expr<V: Visitor + ?Sized>(visitor: &mut V, e: &Expr) {
    match *e {
        Expr::Unary(ref a) => visitor.visit_atom(a),
        Expr::BinOp { lhs: _, op: _, rhs: _ } => visitor.visit_binary_op(e)
    }
}
