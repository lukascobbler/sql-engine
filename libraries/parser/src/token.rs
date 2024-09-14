use std::fmt::{Debug, Display, Formatter};
use strum_macros::{EnumString, IntoStaticStr};

#[derive(PartialEq, Clone)]
pub(crate) enum Token {
    Keyword(Keyword),
    Identifier(String),
    String(String),
    Number(u64),
    Float(f64),
    Invalid(char),
    RightParentheses,
    LeftParentheses,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
    Star,
    Divide,
    Minus,
    Plus,
    Comma,
    Dot,
    Semicolon,
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{:?}", keyword),
            Token::Identifier(iden) => write!(f, "{:?}", iden),
            Token::String(str) => write!(f, "{:?}", str),
            Token::Number(num) => write!(f, "{:?}", num),
            Token::Float(num) => write!(f, "{:?}", num),
            Token::RightParentheses => write!(f, "("),
            Token::LeftParentheses => write!(f, ")"),
            Token::GreaterThan => write!(f, ">"),
            Token::GreaterThanOrEqual => write!(f, ">="),
            Token::LessThan => write!(f, "<"),
            Token::LessThanOrEqual => write!(f, "<="),
            Token::Equal => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::Star => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Semicolon => write!(f, ";"),
            Token::Eof => write!(f, "Eof"),
            Token::Invalid(c) => write!(f, "{}", c),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(PartialEq, Clone, EnumString, IntoStaticStr)]
#[strum(ascii_case_insensitive)]
pub(crate) enum Keyword {
    Select,
    Update,
    Delete,
    Insert,
    Into,
    Create,
    Drop,
    Table,
    Database,
    Values,
    Set,
    Where,
    Order,
    By,
    Asc,
    Desc,
    From,
    Join,
    Inner,
    Outer,
    Left,
    Right,
    Natural,
    On,
    Is,
    And,
    Or,
    Not,
    True,
    False,
    Unique,
    Index,
    Primary,
    Foreign,
    References,
    Key,
    Check,
    Default,
    Int,
    BigInt,
    Decimal,
    Bool,
    Varchar,
    Null,
    Start,
    Transaction,
    Rollback,
    Commit,
    Explain,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", <&Keyword as Into<&str>>::into(self).to_uppercase())
    }
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}