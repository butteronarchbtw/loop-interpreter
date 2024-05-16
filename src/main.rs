use core::fmt;

#[derive(Debug, Eq, PartialEq)]
enum Token {
    Assignment, // :=
    Plus,
    Minus,
    EndKeyword,
    DoKeyword,
    LoopKeyword,
    Semicolon,
    EOF,
    Constant(u8), // only 0 and 1
    Variable(u32) // x_i
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
            LexError::IllegalKeyword(kw) => write!(f, "illegal keyword {}", kw)
        }
    }
}

impl Lexer {
    pub fn new(text: String) -> Lexer {
       return Lexer {
            text: text.into_bytes(),
            pos: 0
       } 
    }

    pub fn skip_whitespace(&mut self) {
        while self.text[self.pos].is_ascii_whitespace() && self.pos < self.text.len() {
            self.pos += 1;
        }
    }

    pub fn read_number(&mut self) -> Result<u32, LexError> {
       let start = self.pos; 
       while self.text[self.pos].is_ascii_digit() {
            self.pos += 1;
       }
       let n = String::from_utf8_lossy(&self.text[start..self.pos]).to_string().parse::<u32>().map_err(|_| LexError::InvalidNumber)?;
       self.pos -= 1;
       Ok(n)
    }

    pub fn read_keyword(&mut self) -> String {
       let start = self.pos; 
       while self.pos < self.text.len() && self.text[self.pos].is_ascii_alphabetic() {
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
            },
            b'0'..=b'9' => {
                let c = self.read_number()?;
                if c > 1 {
                    Err(LexError::InvalidNumber)
                } else {
                    Ok(Token::Constant(u8::try_from(c).expect("parsing a constant that is for sure between 0 and 1 to u8 should NEVER fail")))
                }
            },
            b'x' => {
                let next = self.peek();
                if next.is_ascii_digit() {
                    self.pos += 1;
                    let i = self.read_number()?;
                    Ok(Token::Variable(i))
                } else {
                    Err(LexError::InvalidNumber)
                }
            },
            b'a'..=b'z' => {
                println!("reading keyword starting at {}", self.text[self.pos] as char);
                let kw = self.read_keyword();
                match kw.as_str() {
                    "loop" => Ok(Token::LoopKeyword),
                    "do" => Ok(Token::DoKeyword),
                    "end" => Ok(Token::EndKeyword),
                    &_ => Err(LexError::IllegalKeyword(kw))
                }
            },
            x => Err(LexError::IllegalChar(x as char))
        };

        self.pos += 1;

        tok
    }

    pub fn lex(&mut self) -> Result<Vec<Token>,LexError> {
        let mut token_list = Vec::<Token>::new();
        let mut next = self.next_token()?;
        while next != Token::EOF {
            token_list.push(next);
            next = self.next_token()?;
        }
        Ok(token_list)
    }
}

fn main() {
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_fail() {
        let mut l = Lexer::new("hallo123".to_string());    
        let lex_res = l.lex();
        assert!(lex_res.is_err_and(|e| {
            assert_eq!(e, LexError::IllegalKeyword(String::from("hallo")));
            return true;
        }));
    }

    #[test]
    fn test_lex_good() {
        let prog = "x1:=1;loop x1 do x2 := 0 end; loop x2 do end";
        let mut l = Lexer::new(prog.to_string());    
        let lex_res = l.lex().unwrap();
        assert_eq!(lex_res, vec![
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
        ]);
    }
}
