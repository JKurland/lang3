use crate::Result;
use crate::Error;
use crate::Query;
use crate::Program;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Loop,
    If,
    Else,
    Break,
    Fn,
    Struct,
    Return,
    Let,
    ThinArrow,
    U32,
    DoubleEq,
    Ident(String),
    Int(String),
    String(String),
    Comma,
    Colon,
    SemiColon,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Equal,
    Plus,
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub t: TokenType
}

trait Lexer {
    /// Returns the length of the token lexed, if the string
    /// does not start with this type of token then return Ok(0), if the string
    /// is this type of token but there is an error then return Err.
    fn lex(&self, s: &str) -> Result<usize>;

    /// Convert the token str into a TokenType for this lexer.
    fn make_token(&self, token: &str) -> TokenType;
}


struct Keyword {
    keyword: &'static str,
    t: TokenType,
}

impl Lexer for Keyword {
    fn lex(&self, s: &str) -> Result<usize> {
        if s.starts_with(self.keyword) {
            Ok(self.keyword.len())
        } else {
            Ok(0)
        }
    }

    fn make_token(&self, _token: &str) -> TokenType {
        self.t.clone()
    }
}


struct Punctuation {
    c: char,
    t: TokenType,
}

impl Lexer for Punctuation {
    fn lex(&self, s: &str) -> Result<usize> {
        if s.starts_with(self.c) {
            Ok(1)
        } else {
            Ok(0)
        }
    }

    fn make_token(&self, _token: &str) -> TokenType {
        self.t.clone()
    }
}


struct IntLiteral;

impl Lexer for IntLiteral {
    fn lex(&self, s: &str) -> Result<usize> {
        if let Some(token_length) = s.find(|c: char| !c.is_ascii_digit()) {
            Ok(token_length)
        } else {
            Ok(s.len())
        }
    }

    fn make_token(&self, token: &str) -> TokenType {
        TokenType::Int(token.to_string())
    }
}


struct StringLiteral;

impl Lexer for StringLiteral {
    fn lex(&self, s: &str) -> Result<usize> {
        if let Some(content) = s.strip_prefix('"') {
            if let Some(close_idx) = content.find('"') {
                return Ok(close_idx + 2)
            } else {
                return Err(Error::new("String isn't closed"));
            }
        } else {
            return Ok(0);
        }
    }

    fn make_token(&self, token: &str) -> TokenType {
        TokenType::String(token.to_string())
    }
}


struct Ident;

impl Lexer for Ident {
    fn lex(&self, s: &str) -> Result<usize> {
        if let Some(after_first) = s.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_') {
            if let Some(close_idx) = after_first.find(|c: char| !c.is_ascii_alphanumeric() && c != '_') {
                Ok(close_idx + 1)
            } else {
                Ok(s.len())
            }
        } else {
            Ok(0)
        }
    }

    fn make_token(&self, token: &str) -> TokenType {
        TokenType::Ident(token.to_string())
    }
}


pub(crate) fn lex(mut s: &str) -> Result<Vec<Token>> {
    let lexers = [
        Box::new(Keyword{keyword: "loop", t: TokenType::Loop}) as Box<dyn Lexer>,
        Box::new(Keyword{keyword: "if", t: TokenType::If}),
        Box::new(Keyword{keyword: "else", t: TokenType::Else}),
        Box::new(Keyword{keyword: "break", t: TokenType::Break}),
        Box::new(Keyword{keyword: "fn", t: TokenType::Fn}),
        Box::new(Keyword{keyword: "struct", t: TokenType::Struct}),
        Box::new(Keyword{keyword: "return", t: TokenType::Return}),
        Box::new(Keyword{keyword: "let", t: TokenType::Let}),
        Box::new(Keyword{keyword: "->", t: TokenType::ThinArrow}),
        Box::new(Keyword{keyword: "u32", t: TokenType::U32}),
        Box::new(Keyword{keyword: "==", t: TokenType::DoubleEq}),
        Box::new(IntLiteral),
        Box::new(StringLiteral),
        Box::new(Ident),
        Box::new(Punctuation{c: ',', t: TokenType::Comma}),
        Box::new(Punctuation{c: ':', t: TokenType::Colon}),
        Box::new(Punctuation{c: ';', t: TokenType::SemiColon}),
        Box::new(Punctuation{c: '{', t: TokenType::OpenBrace}),
        Box::new(Punctuation{c: '}', t: TokenType::CloseBrace}),
        Box::new(Punctuation{c: '(', t: TokenType::OpenParen}),
        Box::new(Punctuation{c: ')', t: TokenType::CloseParen}),
        Box::new(Punctuation{c: '=', t: TokenType::Equal}),
        Box::new(Punctuation{c: '+', t: TokenType::Plus}),
    ];

    let mut tokens: Vec<Token> = Vec::new();
    loop {
        s = s.trim_start_matches(|c: char| c.is_ascii_whitespace());
        if s.is_empty() {
            break;
        }

        let results = lexers
            .iter()
            .map(|l| l.lex(s))
            .zip(lexers.iter())
            .filter(|r| r.0 != Ok(0));

        let mut max_token_length = 0usize;
        let mut matched_lexer = None;
        for (token_length, lexer) in results {
            let r = token_length?;
            if r > max_token_length {
                max_token_length = r;
                matched_lexer = Some(lexer);
            }
        }

        let (token, remaining) = s.split_at(max_token_length);
        s = remaining;
        match matched_lexer {
            None => return Err(Error::new("no matching lexer")),
            Some(lexer) => tokens.push(Token{t: lexer.make_token(token)})
        }
    }
    return Ok(tokens);
}

#[derive(Hash, PartialEq, Clone)]
pub(crate) struct GetTokenStream;

impl Query for GetTokenStream {
    type Output = Result<Vec<Token>>;
}

impl GetTokenStream {
    pub(crate) async fn make(self, prog: Arc<Program>) -> <Self as Query>::Output {
        lex(prog.get_src())
    }
}

mod test {
    #[cfg(test)]
    use crate::make_query;

    #[cfg(test)]
    use super::*;

    #[test]
    fn test_lex_keyword() {
        assert_eq!(lex("loop"), Ok(vec![Token{t: TokenType::Loop}]));
        assert_eq!(lex("if"), Ok(vec![Token{t: TokenType::If}]));
        assert_eq!(lex("loop if"), Ok(vec![Token{t: TokenType::Loop}, Token{t: TokenType::If}]));
    }

    #[test]
    fn test_lex_ident() {
        assert_eq!(lex("hello"), Ok(vec![Token{t: TokenType::Ident("hello".to_string())}]));
        assert_eq!(lex("hello "), Ok(vec![Token{t: TokenType::Ident("hello".to_string())}]));
        assert_eq!(lex("hello goodbye"), Ok(vec![
            Token{t: TokenType::Ident("hello".to_string())},
            Token{t: TokenType::Ident("goodbye".to_string())}
        ]));
    }

    #[test]
    fn test_lex_mixed() {
        assert_eq!(lex("hello goodbye   123 if loop else 234,; dj23 \"h£ello\"_23\n43\n loophello\t72jsd +"), Ok(vec![
            Token{t: TokenType::Ident("hello".to_string())},
            Token{t: TokenType::Ident("goodbye".to_string())},
            Token{t: TokenType::Int("123".to_string())},
            Token{t: TokenType::If},
            Token{t: TokenType::Loop},
            Token{t: TokenType::Else},
            Token{t: TokenType::Int("234".to_string())},
            Token{t: TokenType::Comma},
            Token{t: TokenType::SemiColon},
            Token{t: TokenType::Ident("dj23".to_string())},
            Token{t: TokenType::String("\"h£ello\"".to_string())},
            Token{t: TokenType::Ident("_23".to_string())},
            Token{t: TokenType::Int("43".to_string())},
            Token{t: TokenType::Ident("loophello".to_string())},
            Token{t: TokenType::Int("72".to_string())},
            Token{t: TokenType::Ident("jsd".to_string())},
            Token{t: TokenType::Plus},
        ]));
    }

    #[cfg(test)]
    use futures::executor::block_on;
    #[test]
    fn test_as_query() {
        let prog = Arc::new(Program::new("if a; b".to_string()));
        let token_stream = block_on(make_query!(&prog, GetTokenStream));
        assert_eq!(*token_stream, Ok(vec![
            Token{t: TokenType::If},
            Token{t: TokenType::Ident("a".to_string())},
            Token{t: TokenType::SemiColon},
            Token{t: TokenType::Ident("b".to_string())},
        ]));
    }
}