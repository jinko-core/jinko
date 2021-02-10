//! ShuntingYard parses operators and operands according to operator precedence,
//! returning a BinaryOp in the end

use crate::instruction::{BinaryOp, Instruction, Operator};
use crate::parser::{BoxConstruct, Construct, Token};
use crate::utils::{Queue, Stack};

use nom::{branch::alt, Err, IResult};

/// SyPairs are contained in the ShuntingYard's output queue. When building the parser,
/// this queue is created in an "infix" way, but using SyPair instead of traditional
/// strings. This infix notation is then transformed into an AST
enum SyPair {
    Op(Operator),
    Num(Box<dyn Instruction>),
}

pub struct ShuntingYard {
    operators: Stack<Operator>,
    output: Queue<SyPair>,
}

impl ShuntingYard {
    fn operator<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, op) = alt((
            Token::add,
            Token::sub,
            Token::mul,
            Token::div,
            Token::left_parenthesis,
            Token::right_parenthesis,
        ))(input)?;

        let (input, _) = Token::maybe_consume_extra(input)?;

        let op = Operator::new(op);

        if op != Operator::LeftParenthesis && op != Operator::RightParenthesis {
            while !self.operators.is_empty()
                && (self.operators.peek().unwrap().precedence() > op.precedence()
                    || (self.operators.peek().unwrap().precedence() == op.precedence()
                        && op.is_left_associative()))
                && (self.operators.peek() != Some(&Operator::LeftParenthesis))
            {
                // We can unwrap safely since we checked that self.operators is not
                // empty
                self.output.push(SyPair::Op(self.operators.pop().unwrap()));
            }

            self.operators.push(op);
        } else if op == Operator::LeftParenthesis {
            self.operators.push(op);
        } else if op == Operator::RightParenthesis {
            while self.operators.peek() != Some(&Operator::LeftParenthesis) {
                match self.operators.pop() {
                    None => {
                        return Err(nom::Err::Error((
                            "Unclosed right parenthesis",
                            nom::error::ErrorKind::OneOf,
                        )))
                    }

                    Some(op) => self.output.push(SyPair::Op(op)),
                }
            }

            if self.operators.peek() == Some(&Operator::LeftParenthesis) {
                self.operators.pop();
            }
        }

        Ok((input, ()))
    }

    fn operand<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, expr) = alt((
            BoxConstruct::method_call,
            BoxConstruct::function_call,
            Construct::constant,
            BoxConstruct::variable,
        ))(input)?;

        self.output.push(SyPair::Num(expr));

        Ok((input, ()))
    }

    fn handle_token<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_extra(input)?;

        let (input, _) = match input.chars().next() {
            None => {
                return Err(Err::Error((
                    "Not a valid binary expression",
                    nom::error::ErrorKind::OneOf,
                )))
            }
            Some(c) => {
                // Return early if a finishing character is found
                if c == '}' || c == ';' {
                    return Err(nom::Err::Error((
                        "Finished binary expression",
                        nom::error::ErrorKind::OneOf,
                    )));
                }

                match Token::is_operator(c) {
                    true => self.operator(input)?,
                    false => self.operand(input)?,
                }
            }
        };

        let (input, _) = Token::maybe_consume_extra(input)?;

        Ok((input, ()))
    }

    /// Create a new, empty ShuntingYard parser
    fn new() -> ShuntingYard {
        ShuntingYard {
            operators: Stack::new(),
            output: Queue::new(),
        }
    }

    /// Reduce the RPN produced by the ShuntingYard to an Expression
    fn reduce<'i>(input: &'i str, rpn: Queue<SyPair>) -> IResult<&'i str, Box<dyn Instruction>> {
        let mut stack = Stack::new();

        for sy_pair in rpn.into_iter() {
            match sy_pair {
                SyPair::Num(num) => {
                    stack.push(num);
                }
                SyPair::Op(op) => {
                    if let Some(rhs) = stack.pop() {
                        if let Some(lhs) = stack.pop() {
                            stack.push(Box::new(BinaryOp::new(lhs, rhs, op)));
                            continue;
                        }
                    }
                }
            }
        }

        match stack.pop() {
            Some(value) => Ok((input, value)),
            None => Err(nom::Err::Error((
                "Unclosed right parenthesis",
                nom::error::ErrorKind::OneOf,
            ))),
        }
    }

    /// Create a BinaryOp from an input string, executing the shunting yard
    /// algorithm
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Instruction>> {
        let mut sy = ShuntingYard::new();

        let mut input = i.clone();

        match sy.handle_token(input) {
            Err(nom::Err::Error(_)) => {
                return Err(Err::Error((
                    "Not a valid binary expression",
                    nom::error::ErrorKind::Many1,
                )))
            }
            Err(e) => return Err(e),
            Ok((new_i, _)) => {
                input = new_i;

                loop {
                    match sy.handle_token(input) {
                        // FIXME: Maybe don't use OneOf as error type?
                        Err(nom::Err::Error((_, nom::error::ErrorKind::OneOf))) => break,
                        Err(e) => return Err(e),
                        Ok((new_i, _)) => {
                            if new_i == input {
                                break;
                            }

                            input = new_i;
                        }
                    }
                }
            }
        }

        // We are done, pop everything from the operator stack
        while !sy.operators.is_empty() {
            sy.output.push(SyPair::Op(sy.operators.pop().unwrap()));
        }

        ShuntingYard::reduce(input, sy.output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::*;

    // FIXME: Add more tests with more operators

    fn sy_assert(input: &str, result: i64) {
        use crate::instance::ToObjectInstance;
        use crate::{InstrKind, Interpreter};

        let boxed_output = ShuntingYard::parse(input).unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();

        let mut i = Interpreter::new();

        assert_eq!(
            output.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(result).to_instance()))
        );
    }

    #[test]
    fn t_sy_execute_natural_order() {
        sy_assert("4 + 7 + 3", 14);
    }

    #[test]
    fn t_sy_execute_mult_priority() {
        sy_assert("4 + 2 * 3", 10);
    }

    #[test]
    fn t_sy_execute_mult_natural_priority() {
        sy_assert("2 * 3 + 4", 10);
    }

    #[test]
    fn t_sy_valid_add() {
        sy_assert("1 + 2", 3);
    }

    #[test]
    fn t_sy_valid_mul() {
        sy_assert("1 * 2", 2);
    }

    #[test]
    fn t_sy_valid_normal_priority() {
        sy_assert("1 * 2 + 3", 5);
    }

    #[test]
    fn t_sy_valid_back_priority() {
        sy_assert("3 + 1 * 2", 5);
    }

    #[test]
    fn t_sy_valid_parentheses_priority() {
        sy_assert("(3 + 1) * 2", 8);
    }

    #[test]
    fn t_sy_valid_parentheses_priority_reverse() {
        sy_assert("2 * (3 + 1)", 8);
    }

    #[test]
    fn t_sy_valid_complex_expr() {
        sy_assert("1 + 4 * 2 - 1 + 2", 1 + 4 * 2 - 1 + 2);
    }

    #[test]
    fn t_sy_valid_multi_expr() {
        sy_assert("3 + 4 * 2 + 5", 3 + 4 * 2 + 5);
    }

    #[test]
    fn t_sy_valid_extremely_complex_expr() {
        sy_assert(
            "1 + 4 * 2 - 1 + 2 * (14 + (2 - 17) * 1) - 12 + 3 / 2",
            1 + 4 * 2 - 1 + 2 * (14 + (2 - 17) * 1) - 12 + 3 / 2,
        );
    }
}
