use crate::{Result, SourceRef};
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
    Dot,
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Token {
    pub t: TokenType,
    pub source_ref: SourceRef,
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
                return Err(Error::StringNotClosed);
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
        Box::new(Punctuation{c: '.', t: TokenType::Dot}),
    ];

    let mut tokens: Vec<Token> = Vec::new();
    let full_len = s.len();
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
        let source_ref = SourceRef::new(full_len - s.len(), token.len());

        s = remaining;
        match matched_lexer {
            None => return Err(Error::UnknownToken),
            Some(lexer) => tokens.push(Token{
                t: lexer.make_token(token),
                source_ref,
            })
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
    use super::*;
    
    #[cfg(test)]
    fn lex_types(s: &str) -> Result<Vec<TokenType>> {
        let mut rtn = Vec::new();
        for token in lex(s)? {
            rtn.push(token.t)
        }
        Ok(rtn)
    }


    #[test]
    fn test_lex_keyword() {
        assert_eq!(lex_types("loop"), Ok(vec![TokenType::Loop]));
        assert_eq!(lex_types("if"), Ok(vec![TokenType::If]));
        assert_eq!(lex_types("loop if"), Ok(vec![TokenType::Loop, TokenType::If]));
    }

    #[test]
    fn test_lex_ident() {
        assert_eq!(lex_types("hello"), Ok(vec![TokenType::Ident("hello".to_string())]));
        assert_eq!(lex_types("hello "), Ok(vec![TokenType::Ident("hello".to_string())]));
        assert_eq!(lex_types("hello goodbye"), Ok(vec![
            TokenType::Ident("hello".to_string()),
            TokenType::Ident("goodbye".to_string())
        ]));
    }

    #[test]
    fn test_lex_mixed() {
        assert_eq!(lex_types("hello goodbye   123 if loop else 234,; dj23 \"h£ello\"_23\n43\n loophello\t72jsd +"), Ok(vec![
            TokenType::Ident("hello".to_string()),
            TokenType::Ident("goodbye".to_string()),
            TokenType::Int("123".to_string()),
            TokenType::If,
            TokenType::Loop,
            TokenType::Else,
            TokenType::Int("234".to_string()),
            TokenType::Comma,
            TokenType::SemiColon,
            TokenType::Ident("dj23".to_string()),
            TokenType::String("\"h£ello\"".to_string()),
            TokenType::Ident("_23".to_string()),
            TokenType::Int("43".to_string()),
            TokenType::Ident("loophello".to_string()),
            TokenType::Int("72".to_string()),
            TokenType::Ident("jsd".to_string()),
            TokenType::Plus,
        ]));
    }

    #[test]
    fn test_lex_source_ref() {
        assert_eq!(lex("hello 1     wo"), Ok(vec![
            Token{t: TokenType::Ident("hello".to_string()), source_ref: SourceRef::new(0, 5)},
            Token{t: TokenType::Int("1".to_string()), source_ref: SourceRef::new(6, 1)},
            Token{t: TokenType::Ident("wo".to_string()), source_ref: SourceRef::new(12, 2)},
        ]));
    }

}