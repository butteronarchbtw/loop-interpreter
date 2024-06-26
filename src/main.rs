use core::fmt;
use std::{collections::HashMap, env, fs, os, path::Path, process::exit};

#[derive(Debug, Eq, PartialEq, Clone)]
enum Token {
    Assignment, // :=
    Plus,
    Minus,
    EndKeyword,
    DoKeyword,
    LoopKeyword,
    Semicolon,
    EOF,
    Constant(u8),  // only 0 and 1
    Variable(u32), // x_i
}

struct Lexer {
    text: Vec<u8>,
    pos: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum LexError {
    InvalidNumber,
    IllegalChar(char),
    IllegalKeyword(String),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::InvalidNumber => write!(f, "invalid number somewhere lol"),
            LexError::IllegalChar(c) => write!(f, "illegal character {}", c),
            LexError::IllegalKeyword(kw) => write!(f, "illegal keyword {}", kw),
        }
    }
}

impl Lexer {
    pub fn new(text: String) -> Lexer {
        return Lexer {
            text: text.into_bytes(),
            pos: 0,
        };
    }

    pub fn skip_whitespace(&mut self) {
        while self.pos < self.text.len() && self.text[self.pos].is_ascii_whitespace()  {
            self.pos += 1;
        }
    }

    pub fn read_number(&mut self) -> Result<u32, LexError> {
        let start = self.pos;
        while self.pos < self.text.len() && self.text[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        let n = String::from_utf8_lossy(&self.text[start..self.pos])
            .to_string()
            .parse::<u32>()
            .map_err(|_| LexError::InvalidNumber)?;
        self.pos -= 1;
        Ok(n)
    }

    pub fn read_keyword(&mut self) -> String {
        let start = self.pos;
        while self.pos < self.text.len() && self.text[self.pos].is_ascii_alphanumeric() {
            self.pos += 1;
        }
        let n = String::from_utf8_lossy(&self.text[start..self.pos]).to_string();
        self.pos -= 1;
        n
    }

    pub fn peek(&self) -> u8 {
        if self.pos < self.text.len() - 1 {
            self.text[self.pos + 1]
        } else {
            0
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        if self.pos > self.text.len() - 1 {
            return Ok(Token::EOF);
        }
        self.skip_whitespace();
        if self.pos > self.text.len() - 1 {
            return Ok(Token::EOF);
        }

        let tok = match self.text[self.pos] {
            b';' => Ok(Token::Semicolon),
            b'+' => Ok(Token::Plus),
            b'-' => Ok(Token::Minus),
            b':' => {
                let next = self.peek();
                if next == b'=' {
                    self.pos += 1;
                    Ok(Token::Assignment)
                } else {
                    Err(LexError::IllegalChar(next as char))
                }
            }
            b'0'..=b'9' => {
                let c = self.read_number()?;
                if c > 1 {
                    Err(LexError::InvalidNumber)
                } else {
                    Ok(Token::Constant(u8::try_from(c).expect("parsing a constant that is for sure between 0 and 1 to u8 should NEVER fail")))
                }
            }
            b'x' => {
                let next = self.peek();
                if next.is_ascii_digit() {
                    self.pos += 1;
                    let i = self.read_number()?;
                    Ok(Token::Variable(i))
                } else {
                    Err(LexError::InvalidNumber)
                }
            }
            b'a'..=b'z' => {
                let kw = self.read_keyword();
                match kw.as_str() {
                    "loop" => Ok(Token::LoopKeyword),
                    "do" => Ok(Token::DoKeyword),
                    "end" => Ok(Token::EndKeyword),
                    &_ => Err(LexError::IllegalKeyword(kw)),
                }
            }
            x => Err(LexError::IllegalChar(x as char)),
        };

        self.pos += 1;

        tok
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let mut token_list = Vec::<Token>::new();
        let mut next = self.next_token()?;
        while next != Token::EOF {
            token_list.push(next);
            next = self.next_token()?;
        }
        Ok(token_list)
    }
}

#[derive(Clone)]
enum Program {
    Sequential(Sequential),
    NonSequential(NonSequential),
}

#[derive(Clone)]
struct Sequential {
    p1: Box<NonSequential>,
    p2: Box<Program>,
}

#[derive(Clone)]
enum NonSequential {
    Assignment(Assignment),
    LoopStatement(LoopStatement),
}

#[derive(Clone)]
enum Assignment {
    ValueAssignment(ValueAssignment),      // x_i := c
    OperatorAssigment(OperatorAssignment), // x_i := x_j (+|-) c
}

#[derive(Clone)]
struct Variable {
    i: u32,
}

#[derive(Clone)]
struct Constant {
    value: u8,
}

#[derive(Clone)]
struct ValueAssignment {
    x: Box<Variable>,
    c: Box<Constant>,
}

#[derive(Clone)]
enum Operator {
    Plus,
    Minus,
}

#[derive(Clone)]
struct OperatorAssignment {
    xi: Box<Variable>,
    xj: Box<Variable>,
    op: Box<Operator>,
    c: Box<Constant>,
}

#[derive(Clone)]
struct LoopStatement {
    count: Box<Variable>,
    body: Box<Program>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseError {
    ExpectedMoreTokens,
    ExpectedDifferentToken(Token, Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::ExpectedMoreTokens => write!(f, "too few tokens provided"),
            Self::ExpectedDifferentToken(t1, t2) => {
                write!(f, "got token {:?}, expected token {:?}", t1, t2)
            }
        }
    }
}

struct Parser {
    tokens: <Vec<Token> as IntoIterator>::IntoIter,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter(),
        }
    }

    /// main program
    pub fn parse(&mut self) -> Result<Box<Program>, ParseError> {
        let prog = self.parse_program()?;
        if self.tokens.len() > 1 {
            return Err(ParseError::ExpectedDifferentToken(Token::Semicolon, Token::EOF));
        }
        Ok(prog)
    }

    /// <program> ::= <non-sequential> | <non-sequential>;<program>
    /// <non-sequential> ::= <assignment> | <loop-statement>
    /// <assignment> ::= <variable>:=<constant> | <variable>:=<variable>+<constant> |
    ///     <variable>:=<varibale>-<constant>
    /// <loop-statement> ::= loop<variable>do<program>end
    pub fn parse_program(&mut self) -> Result<Box<Program>, ParseError> {
        let p1 = self.parse_nonseq()?;
        let mut peekable = self.tokens.clone().peekable();
        let peek = peekable.peek();
        if peek.is_none() || peek.is_some_and(|p| p.to_owned() != Token::Semicolon) {
            Ok(Box::new(Program::NonSequential(*p1)))
        } else {
            let _ = self.tokens.next();
            let p2 = self.parse_program()?;
            let s = Sequential { p1, p2 };
            Ok(Box::new(Program::Sequential(s)))
        }
    }

    pub fn parse_nonseq(&mut self) -> Result<Box<NonSequential>, ParseError> {
        let mut peekable = self.tokens.clone().peekable();
        let peek = peekable.peek().ok_or(ParseError::ExpectedMoreTokens)?;
        match peek {
            Token::Variable(_) => {
                let a = self.parse_assignment()?;
                Ok(Box::new(NonSequential::Assignment(*a)))
            }
            Token::LoopKeyword => {
                let l = self.parse_loop_statement()?;
                Ok(Box::new(NonSequential::LoopStatement(*l)))
            }
            _ => Err(ParseError::ExpectedDifferentToken(
                Token::Variable(0),
                peek.to_owned(),
            )),
        }
    }

    pub fn parse_assignment(&mut self) -> Result<Box<Assignment>, ParseError> {
        let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
        match next {
            Token::Variable(v) => {
                let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                if next != Token::Assignment {
                    Err(ParseError::ExpectedDifferentToken(Token::Assignment, next))
                } else {
                    let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                    match next {
                        Token::Variable(vj) => {
                            let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                            match next {
                                Token::Plus => {
                                    let next =
                                        self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                                    match next {
                                        Token::Constant(c) => {
                                            let xi = Box::new(Variable { i: v });
                                            let xj = Box::new(Variable { i: vj });
                                            let c = Box::new(Constant { value: c });
                                            let op = Box::new(Operator::Plus);
                                            Ok(Box::new(Assignment::OperatorAssigment(
                                                OperatorAssignment { xi, xj, op, c },
                                            )))
                                        }
                                        _ => Err(ParseError::ExpectedDifferentToken(
                                            Token::Constant(0),
                                            next,
                                        )),
                                    }
                                }
                                Token::Minus => {
                                    let next =
                                        self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                                    match next {
                                        Token::Constant(c) => {
                                            let xi = Box::new(Variable { i: v });
                                            let xj = Box::new(Variable { i: vj });
                                            let c = Box::new(Constant { value: c });
                                            let op = Box::new(Operator::Minus);
                                            Ok(Box::new(Assignment::OperatorAssigment(
                                                OperatorAssignment { xi, xj, op, c },
                                            )))
                                        }
                                        _ => Err(ParseError::ExpectedDifferentToken(
                                            Token::Constant(0),
                                            next,
                                        )),
                                    }
                                }
                                _ => Err(ParseError::ExpectedDifferentToken(Token::Plus, next)),
                            }
                        }
                        Token::Constant(c) => {
                            let x = Box::new(Variable { i: v });
                            let c = Box::new(Constant { value: c });
                            Ok(Box::new(Assignment::ValueAssignment(ValueAssignment {
                                x,
                                c,
                            })))
                        }
                        _ => Err(ParseError::ExpectedDifferentToken(Token::Constant(0), next)), //TODO:
                                                                                                //variable
                                                                                                //error:
                                                                                                //token
                                                                                                //this
                                                                                                //or
                                                                                                //that
                    }
                }
            }
            _ => Err(ParseError::ExpectedDifferentToken(
                Token::Variable(69),
                next,
            )),
        }
    }

    pub fn parse_loop_statement(&mut self) -> Result<Box<LoopStatement>, ParseError> {
        let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
        match next {
            Token::LoopKeyword => {
                let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                match next {
                    Token::Variable(v) => {
                        let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                        if next != Token::DoKeyword {
                            Err(ParseError::ExpectedDifferentToken(Token::DoKeyword, next))
                        } else {
                            let body = self.parse_program()?;
                            let next = self.tokens.next().ok_or(ParseError::ExpectedMoreTokens)?;
                            if next != Token::EndKeyword {
                                Err(ParseError::ExpectedDifferentToken(Token::EndKeyword, next))
                            } else {
                                let count = Box::new(Variable { i: v });
                                let l = LoopStatement { count, body };
                                Ok(Box::new(l))
                            }
                        }
                    }
                    _ => Err(ParseError::ExpectedDifferentToken(Token::Variable(0), next)),
                }
            }
            _ => Err(ParseError::ExpectedDifferentToken(Token::LoopKeyword, next)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum EvalError {
    VariableNotDefined(u32),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::VariableNotDefined(i) => write!(f, "variable x{} not defined", i),
        }
    }
}

struct Interpreter {
    ast: Box<Program>,
    variables: HashMap<u32, u32>,
}

impl Interpreter {
    // TODO: there should not be an error if variable is not defined, it should be set to 0
    // there should be $k$ variables which are defined by the user (via command line?)
    pub fn new(ast: Box<Program>) -> Interpreter {
        Interpreter {
            ast,
            variables: HashMap::new(),
        }
    }

    fn set_start_allocation(&mut self, values: Vec<u32>) {
        for (i, v) in values.iter().enumerate() {
            self.variables.insert(i as u32 + 1, v.to_owned());
        }
    }

    fn get_variable_value(&mut self, v: Variable) -> u32 {
        let var = self
            .variables
            .get(&v.i);

        match var {
            Some(v) => v.to_owned(),
            None => {
                self.variables.insert(v.i, 0);
                0
            }
        }

    }

    pub fn evaluate(&mut self) -> Result<(), EvalError> {
        self.eval_program(*self.ast.clone())
    }

    pub fn eval_program(&mut self, program: Program) -> Result<(), EvalError> {
        match program {
            Program::Sequential(s) => self.eval_seq(s), Program::NonSequential(n) => self.eval_nonseq(n),
        }
    }

    pub fn eval_seq(&mut self, seq: Sequential) -> Result<(), EvalError> {
        self.eval_nonseq(*seq.p1)?;
        self.eval_program(*seq.p2)?;
        Ok(())
    }

    pub fn eval_nonseq(&mut self, nonseq: NonSequential) -> Result<(), EvalError> {
        match nonseq {
            NonSequential::Assignment(a) => self.eval_assignment(a),
            NonSequential::LoopStatement(l) => self.eval_loop_statement(l),
        }
    }

    pub fn eval_assignment(&mut self, a: Assignment) -> Result<(), EvalError> {
        match a {
            Assignment::ValueAssignment(va) => {
                let _ = self.variables.insert(va.x.i, va.c.value as u32);
                Ok(())
            }
            Assignment::OperatorAssigment(oa) => {
                let xj_val = self.get_variable_value(*oa.xj);
                let xi_new: u32;
                match *oa.op {
                    Operator::Plus => {
                        xi_new = xj_val + oa.c.value as u32;
                    }
                    Operator::Minus => {
                        if oa.c.value as u32 >= xj_val {
                            xi_new = 0;
                        } else {
                            xi_new = xj_val - oa.c.value as u32;
                        }
                    }
                };

                let _ = self.variables.insert(oa.xi.i, xi_new);
                Ok(())
            }
        }
    }

    pub fn eval_loop_statement(&mut self, l: LoopStatement) -> Result<(), EvalError> {
        let mut count = self.get_variable_value(*l.count);
        while count > 0 {
            self.eval_program(*l.body.clone())?;
            count -= 1;
        }
        Ok(())
    }
}

fn main() {
   let args: Vec<String> = env::args().collect(); 
   let mut values: Vec<u32> = vec![];
   if args.len() < 2 {
        println!("need at least 1 argument to the call");
        exit(1);
   }

   match args[1].as_str() {
        "-h" | "--help" => {
            println!("Usage:");
            println!("-h / --help: show this help");
            println!("-k: set the first k variable assignments (list with spaces)");
            exit(0);
        },
        "-k" => {
            if args.len() < 3 {
                println!("wtf u tryna do here? >:(");
                exit(1);
            }
            values = args.clone()[2..args.len() - 1].iter().map(|v| {
                match v.parse::<u32>() {
                    Ok(v) => v,
                    Err(e) => {
                        println!("{}", e.to_string());
                        exit(1);
                    }
                }
            }).collect();
        },
        _ => {
        }
   }

   let filename = args.last().clone().expect("congrats, you have reached an error that cannot happen");

   let read_res = fs::read_to_string(Path::new(filename));
   let text = match read_res {
     Ok(text) => text,
     Err(e) => {
        println!("file \"{filename}\" not found");
        println!("{}", e.to_string());
        exit(1);
     }
   };
   let mut lexer = Lexer::new(text);
   let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => {
            println!("{}", e.to_string());
            exit(1);
        }
   };
   let mut parser = Parser::new(tokens);
   let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            println!("{}", e.to_string());
            exit(1);
        }
   };
   let mut interpreter = Interpreter::new(ast);
   interpreter.set_start_allocation(values);
    match interpreter.evaluate() {
        Ok(()) => {
            println!("========");
            println!("PROGRAM SUCCESSFUL");
            println!("END CONFIGURATION FOLLOWS");
            println!("========");
            println!("{:?}", interpreter.variables);
        },
        Err(e) => {
            println!("{}", e.to_string());
            exit(1);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_fail() {
        let mut l = Lexer::new("hallo123".to_string());
        let lex_res = l.lex();
        assert!(lex_res.is_err_and(|e| {
            assert_eq!(e, LexError::IllegalKeyword(String::from("hallo123")));
            return true;
        }));
    }

    #[test]
    fn test_lex_good() {
        let prog = "x1:=1;loop x1 do x2 := 0 end; loop x2 do end";
        let mut l = Lexer::new(prog.to_string());
        let lex_res = l.lex().unwrap();
        assert_eq!(
            lex_res,
            vec![
                Token::Variable(1),
                Token::Assignment,
                Token::Constant(1),
                Token::Semicolon,
                Token::LoopKeyword,
                Token::Variable(1),
                Token::DoKeyword,
                Token::Variable(2),
                Token::Assignment,
                Token::Constant(0),
                Token::EndKeyword,
                Token::Semicolon,
                Token::LoopKeyword,
                Token::Variable(2),
                Token::DoKeyword,
                Token::EndKeyword
            ]
        );
    }

    #[test]
    fn test_parse_without_errors() {
        let prog = "loop x1 do loop x2 do x1 := 1 end; x1 := x2 + 1 end";
        let mut l = Lexer::new(prog.to_string());
        let tokens = l.lex().unwrap();
        let mut p = Parser::new(tokens);
        p.parse().unwrap();
    }

    #[test]
    fn test_parse_with_errors() {
        let prog = "loop x1 do end";
        let mut l = Lexer::new(prog.to_string());
        let tokens = l.lex().unwrap();
        let mut p = Parser::new(tokens);
        let parse_res = p.parse();
        assert!(parse_res.is_err());
        assert!(parse_res.is_err_and(|e| {
            match e {
                ParseError::ExpectedDifferentToken(_, Token::EndKeyword) => true,
                _ => false,
            }
        }));
    }

    #[test]
    fn test_evaluate() {
        let prog = "x1:=x1+1;x1:=x1+1;x2:=0;loop x1 do x2 := x2 + 1 end";
        let mut l = Lexer::new(prog.to_string());
        let tokens = l.lex().unwrap();
        let mut p = Parser::new(tokens);
        let ast = p.parse().unwrap();
        let mut i = Interpreter::new(ast);
        i.evaluate().unwrap();
        assert_eq!(i.variables.get(&1).unwrap(), &2);
        assert_eq!(i.variables.get(&1).unwrap(), i.variables.get(&2).unwrap());
    }
}
