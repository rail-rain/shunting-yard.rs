extern crate smallvec;
use smallvec::SmallVec;
use std::cmp::Ordering;

pub trait Operators {
    fn precedence(&self) -> u8;
    fn is_left_associative(&self) -> bool;
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token<V, O: Operators, F> {
    Value(V),
    Operator(O),
    OpeningFunction(F),
    ArgumentSeparator,
    OpeningBracket,
    ClosingBracket,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol<V, O: Operators, F> {
    Value(V),
    Operator(O),
    Function(F),
}

enum Stackable<O: Operators, F> {
    Operator(O),
    OpeningFunction(F),
    OpeningBracket,
}

enum DoingState<O: Operators> {
    ReadToken,
    LoopOperator(O),
    LoopArgumentSeparator,
    LoopClosingBracket,
}

pub struct Parser<I, O: Operators, F> {
    input_queue: I,
    stack: SmallVec<[Stackable<O, F>; 8]>,
    doing_state: DoingState<O>,
}

impl<I, O: Operators, F> Parser<I, O, F> {
    fn stacktop_have_precedence_over(&self, reading_op: &O) -> bool {
        if let Some(&Stackable::Operator(ref op_at_stacktop)) = self.stack.last() {
            match op_at_stacktop.precedence().cmp(&reading_op.precedence()) {
                Ordering::Greater => true,
                Ordering::Equal if reading_op.is_left_associative() => true,
                _ => false,
            }
        } else { false }
    }
}

impl<I, V, O: Operators, F> Iterator for Parser<I, O, F> where I: Iterator<Item=Token<V, O, F>> {
    type Item = Symbol<V, O, F>;

    fn next(&mut self) -> Option<Symbol<V, O, F>> {
        loop {
            match self.doing_state {
                DoingState::ReadToken => if let Some(token) = self.input_queue.next() {
                    match token {
                        Token::Value(n) => break Some(Symbol::Value(n)),
                        Token::Operator(o) => self.doing_state = DoingState::LoopOperator(o),
                        Token::OpeningFunction(f) => self.stack.push(Stackable::OpeningFunction(f)),
                        Token::ArgumentSeparator => self.doing_state = DoingState::LoopArgumentSeparator,
                        Token::OpeningBracket => self.stack.push(Stackable::OpeningBracket),
                        Token::ClosingBracket => self.doing_state = DoingState::LoopClosingBracket,
                    }
                } else {
                    match self.stack.pop() {
                        Some(Stackable::Operator(o)) => break Some(Symbol::Operator(o)),
                        Some(Stackable::OpeningFunction(f)) => panic!("missing closing bracket"),
                        Some(Stackable::OpeningBracket) => panic!("mismached bracket"),
                        None => break None,
                    }
                },
                DoingState::LoopOperator(ref reading_op) if self.stacktop_have_precedence_over(reading_op) => {
                    match self.stack.pop() {
                        Some(Stackable::Operator(o)) => break Some(Symbol::Operator(o)),
                        _ => unreachable!(),
                    }
                },
                DoingState::LoopOperator(_) => {
                    let previous_state = std::mem::replace(&mut self.doing_state, DoingState::ReadToken);
                    match previous_state {
                        DoingState::LoopOperator(reading_op) => self.stack.push(Stackable::Operator(reading_op)),
                        _ => unreachable!(),
                    }
                },
                DoingState::LoopArgumentSeparator => match self.stack.last() {
                    Some(&Stackable::Operator(_)) => match self.stack.pop() {
                        Some(Stackable::Operator(o)) => break Some(Symbol::Operator(o)),
                        _ => unreachable!(),
                    },
                    Some(&Stackable::OpeningFunction(_)) => self.doing_state = DoingState::ReadToken,
                    Some(&Stackable::OpeningBracket) => panic!("mismached bracket"),
                    None => panic!("invalid argument separator"),
                },
                DoingState::LoopClosingBracket => match self.stack.pop() {
                    Some(Stackable::Operator(op)) => break Some(Symbol::Operator(op)),
                    Some(Stackable::OpeningBracket) => self.doing_state = DoingState::ReadToken,
                    Some(Stackable::OpeningFunction(f)) => {
                        self.doing_state = DoingState::ReadToken;
                        break Some(Symbol::Function(f));
                    },
                    None => panic!("mismached bracket"),
                },
            };
        }
    }
}

pub fn parse<T, I, V, O: Operators, F>(source: T) -> Parser<I, O, F>
    where I: Iterator<Item=Token<V, O, F>>, T: IntoIterator<IntoIter=I, Item=I::Item> {

    Parser {
        input_queue: source.into_iter(),
        stack: SmallVec::new(),
        doing_state: DoingState::ReadToken,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[derive(Debug, PartialEq, Eq)]
    enum Operator {
        Add, Sub, Mul, Div, Pow
    }

    impl Operators for Operator {
        fn precedence(&self) -> u8 {
            match *self {
                Operator::Add => 2,
                Operator::Sub => 2,
                Operator::Mul => 3,
                Operator::Div => 3,
                Operator::Pow => 4,
            }
        }

        fn is_left_associative(&self) -> bool {
            match *self {
                Operator::Add => true,
                Operator::Sub => true,
                Operator::Mul => true,
                Operator::Div => true,
                Operator::Pow => false,
            }
        }
    }
    #[test]
    fn base() {
        use self::Operator::*;
        let parsed: Vec<_> = {
            use Token::*;
            parse::<_, _, _, _, String>(vec![
            Value(11),
            Operator(Add),
            Value(11),
        ])}.collect();
        use Symbol::*;
        assert_eq!(parsed, [Value(11), Value(11), Operator(Add)]);
    }

    #[test]
    fn full() {
        use self::Operator::*;
        let parsed: Vec<_> = {
            use Token::*;
            parse::<_, _, _, _, String>(vec![
            Value(3),
            Operator(Add),
            Value(4),
            Operator(Mul),
            Value(2),
            Operator(Div),
            OpeningBracket,
            Value(1),
            Operator(Sub),
            Value(5),
            ClosingBracket,
            Operator(Pow),
            Value(2),
            Operator(Pow),
            Value(3)
        ])}.collect();
        use Symbol::*;
        assert_eq!(parsed, [
            Value(3),
            Value(4),
            Value(2),
            Operator(Mul),
            Value(1),
            Value(5),
            Operator(Sub),
            Value(2),
            Value(3),
            Operator(Pow),
            Operator(Pow),
            Operator(Div),
            Operator(Add),
        ]);
    }

    #[test]
    fn function() {
        use self::Operator::*;
        let parsed: Vec<_> = {
            use Token::*;
            parse(vec![
            OpeningFunction("sin".to_string()),
            OpeningFunction("max".to_string()),
            Value(2.0),
            ArgumentSeparator,
            Value(3.0),
            ClosingBracket,
            Operator(Div),
            Value(3.0),
            Operator(Mul),
            Value(3.1415),
            ClosingBracket,
        ])}.collect();
        use Symbol::*;
        assert_eq!(parsed, [
            Value(2.0),
            Value(3.0),
            Function("max".to_string()),
            Value(3.0),
            Operator(Div),
            Value(3.1415),
            Operator(Mul),
            Function("sin".to_string()),
        ]);
    }
}