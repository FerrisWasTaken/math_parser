use lazy_static::lazy_static;
use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    prec_climber::*,
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ExprParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        PrecClimber::new(vec![
            Operator::new(Rule::eq, Assoc::Left),
            Operator::new(Rule::power, Assoc::Left),
            Operator::new(Rule::add, Assoc::Left),
            Operator::new(Rule::sub, Assoc::Left),
            Operator::new(Rule::modulo, Assoc::Left),
            Operator::new(Rule::div, Assoc::Left),
            Operator::new(Rule::mul, Assoc::Left),
        ])
    };
}

/// The atom type
#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Ident(String),
    Integer(f64),
}

/// Various operations
#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Modulo,
    Power,
}

/// The main AST type **should only be created with the `parse` method**
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Unary(Atom),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}

/// Main method to convert a `&str` to a Expr
/// ```
/// # use math_parser::{parse, Expr, Op, Atom};
/// assert_eq!(
///     parse("2 + 3").unwrap(),
///     Expr::BinOp {
///         lhs: Box::new(Expr::Unary(Atom::Integer(2.0))),
///         op: Op::Add,
///         rhs: Box::new(Expr::Unary(Atom::Integer(3.0)))
///     }
/// );
/// ```
pub fn parse(input: &str) -> Result<Expr, Error<Rule>> {
    let ast = ExprParser::parse(Rule::input, input)?;
    drop(input);
    Ok(parse_pairs(ast))
}
fn parse_pairs(ast: Pairs<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        ast,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::float | Rule::integer => {
                Expr::Unary(Atom::Integer(pair.as_str().trim().parse::<f64>().unwrap()))
            }
            Rule::ident => Expr::Unary(Atom::Ident(pair.as_str().trim().to_string())),
            Rule::nest => parse_pairs(pair.into_inner()),
            rule => panic!(
                "expected Rule::integer or Rule::float or Rule::ident found {:?}",
                rule
            ),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| {
            let op = match op.as_rule() {
                Rule::add => Op::Add,
                Rule::sub => Op::Sub,
                Rule::mul => Op::Mul,
                Rule::div => Op::Div,
                Rule::eq => Op::Eq,
                Rule::modulo => Op::Modulo,
                Rule::power => Op::Power,
                _ => unreachable!(),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        },
    )
}

#[cfg(feature = "visitor")]
pub mod visitor;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_expr_tests() {
        assert_eq!(
            parse("2 + 3").unwrap(),
            Expr::BinOp {
                lhs: Box::new(Expr::Unary(Atom::Integer(2.0))),
                op: Op::Add,
                rhs: Box::new(Expr::Unary(Atom::Integer(3.0)))
            }
        );
        assert_eq!(
            parse("2 * 2 - 3").unwrap(),
            Expr::BinOp {
                lhs: Box::new(Expr::BinOp {
                    lhs: Box::new(Expr::Unary(Atom::Integer(2.0))),
                    op: Op::Mul,
                    rhs: Box::new(Expr::Unary(Atom::Integer(2.0)))
                }),
                op: Op::Sub,
                rhs: Box::new(Expr::Unary(Atom::Integer(3.0)))
            }
        );
        assert_eq!(
            parse("(2 * 2) - 3").unwrap(),
            Expr::BinOp {
                lhs: Box::new(Expr::BinOp {
                    lhs: Box::new(Expr::Unary(Atom::Integer(2.0))),
                    op: Op::Mul,
                    rhs: Box::new(Expr::Unary(Atom::Integer(2.0)))
                }),
                op: Op::Sub,
                rhs: Box::new(Expr::Unary(Atom::Integer(3.0)))
            }
        );
        assert_eq!(
            parse("2 * (2 - 3)").unwrap(),
            Expr::BinOp {
                rhs: Box::new(Expr::BinOp {
                    lhs: Box::new(Expr::Unary(Atom::Integer(2.0))),
                    op: Op::Sub,
                    rhs: Box::new(Expr::Unary(Atom::Integer(3.0)))
                }),
                op: Op::Mul,
                lhs: Box::new(Expr::Unary(Atom::Integer(2.0)))
            }
        );
        assert_eq!(parse("-9").unwrap(), Expr::Unary(Atom::Integer(-9.0)));
        assert_eq!(
            parse("x = 3 * 5").unwrap(),
            Expr::BinOp {
                lhs: Box::new(Expr::Unary(Atom::Ident("x".to_string()))),
                op: Op::Eq,
                rhs: Box::new(Expr::BinOp {
                    lhs: Box::new(Expr::Unary(Atom::Integer(3.0))),
                    op: Op::Mul,
                    rhs: Box::new(Expr::Unary(Atom::Integer(5.0)))
                })
            }
        )
    }
}
