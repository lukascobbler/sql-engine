use crate::token::{Keyword, Token};
use std::cmp::PartialEq;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::str::{Chars, FromStr};

#[derive(PartialEq)]
pub(crate) struct TokenizerError(pub(crate) String);

impl Debug for TokenizerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for TokenizerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for TokenizerError {}

impl From<&str> for TokenizerError {
    fn from(value: &str) -> Self {
        TokenizerError("tokenizer error: ".to_string() + value)
    }
}

pub(crate) type TokenizerResult<T> = Result<T, TokenizerError>;

fn is_char_keyword_or_identifier(char: char) -> bool {
    char.is_lowercase() || char.is_uppercase() || char.is_ascii_digit() || char == '_'
}

pub(crate) struct TokenIterator<'a> {
    input_string_iter: Peekable<Chars<'a>>,
    is_end: bool,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = TokenizerResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next_char = ' ';

        while next_char.is_whitespace() {
            next_char = match self.input_string_iter.next() {
                Some(char) => char,
                None => {
                    if self.is_end { return None; };
                    self.is_end = true;
                    return Some(Ok(Token::Eof));
                }
            };
        }

        Some(Ok(match next_char {
            '=' => Token::Equal,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '/' => Token::Divide,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '(' => Token::LeftParentheses,
            ')' => Token::RightParentheses,
            ';' => Token::Semicolon,
            '!' => {
                if self.check_consume_next('=') {
                    Token::NotEqual
                } else {
                    return Some(Err("! not followed by =".into()));
                }
            }
            '>' => {
                if self.check_consume_next('=') {
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '<' => {
                if self.check_consume_next('=') {
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            }
            '0'..='9' => {
                let first_num = self.build_number(next_char);

                if self.check_consume_next('.') {
                    let next_char = self.input_string_iter.next().unwrap();
                    if !next_char.is_ascii_digit() {
                        return Some(Err("number followed by a dot not followed by another number".into()));
                    }
                    let second_num = self.build_number(next_char);

                    Token::Float(first_num as f64 + 10f64.powi(-((second_num.checked_ilog10().unwrap_or(0) + 1) as i32)) * second_num as f64)
                } else {
                    Token::Number(first_num)
                }
            }
            '*' => Token::Star,
            '"' | '\'' => {
                match self.build_string(next_char) {
                    Ok(str) => Token::String(str),
                    Err(e) => return Some(Err(e))
                }
            }
            _ if is_char_keyword_or_identifier(next_char) => self.build_keyword_or_identifier(next_char),
            _ => Token::Invalid(next_char)
        }))
    }
}

impl<'a> TokenIterator<'a> {
    pub fn new(input_string: &'a str) -> Self {
        TokenIterator {
            input_string_iter: input_string.chars().peekable(),
            is_end: false,
        }
    }

    pub fn build_keyword_or_identifier(&mut self, starting_char: char) -> Token {
        let mut final_string = String::new();
        final_string.push(starting_char);

        while let Some(next_char) = self.input_string_iter.peek() {
            if !is_char_keyword_or_identifier(*next_char) {
                break;
            }

            final_string.push(*next_char);
            self.input_string_iter.next();
        }

        match Keyword::from_str(&final_string) {
            Ok(e) => Token::Keyword(e),
            Err(_) => Token::Identifier(final_string)
        }
    }

    pub fn build_string(&mut self, starting_quote: char) -> TokenizerResult<String> {
        let mut final_string = String::new();

        while let Some(next_char) = self.input_string_iter.peek() {
            if *next_char == '\'' || *next_char == '"' {
                break;
            }

            final_string.push(*next_char);
            self.input_string_iter.next();
        }

        if let Some(next_quote) = self.input_string_iter.next() {
            if next_quote == starting_quote {
                Ok(final_string)
            } else {
                Err("quotes do not match".into())
            }
        } else {
            Err("no ending quote".into())
        }
    }

    pub fn build_number(&mut self, starting_char: char) -> u64 {
        let mut final_string = String::new();
        final_string.push(starting_char);

        while let Some(next_char) = self.input_string_iter.peek() {
            if !next_char.is_ascii_digit() {
                break;
            }

            final_string.push(*next_char);
            self.input_string_iter.next();
        }

        final_string.parse::<u64>().unwrap()
    }

    fn check_consume_next(&mut self, next_char: char) -> bool {
        if let Some(peeked_char) = self.input_string_iter.peek() {
            if *peeked_char == next_char {
                self.input_string_iter.next();
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{Keyword, Token, TokenIterator};

    #[test]
    fn test_string_building_single_quotes() {
        let query = "'aa123'";
        let token_iterator = TokenIterator::new(query);

        assert_eq!(vec![
            Token::String(String::from("aa123")),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>()
        )
    }

    #[test]
    fn test_string_building_double_quotes() {
        let query = "\"aa1234\"";
        let token_iterator = TokenIterator::new(query);

        assert_eq!(vec![
            Token::String(String::from("aa1234")),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>()
        )
    }

    #[should_panic]
    #[test]
    fn test_string_building_wrong_closed_1() {
        let query = "\"aa1234'";
        let token_iterator = TokenIterator::new(query);

        let _ = token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>();
    }

    #[should_panic]
    #[test]
    fn test_string_building_wrong_closed_2() {
        let query = "'aa1234\"";
        let token_iterator = TokenIterator::new(query);

        let _ = token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>();
    }

    #[should_panic]
    #[test]
    fn test_string_building_not_closed() {
        let query = "'aa1234";
        let token_iterator = TokenIterator::new(query);

        let _ = token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>();
    }

    #[test]
    fn test_select_star() {
        let example = "SELECT * FROM users";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::Star,
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_select_comma() {
        let example = "SELECT one, two FROM users";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::Identifier("one".to_string()),
            Token::Comma,
            Token::Identifier("two".to_string()),
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_select_greater_less_than() {
        let example = "SELECT one, two FROM users WHERE one>1 AND two<1";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::Identifier("one".to_string()),
            Token::Comma,
            Token::Identifier("two".to_string()),
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Keyword(Keyword::Where),
            Token::Identifier("one".to_string()),
            Token::GreaterThan,
            Token::Number(1),
            Token::Keyword(Keyword::And),
            Token::Identifier("two".to_string()),
            Token::LessThan,
            Token::Number(1),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_join() {
        let example = "SELECT * FROM users INNER JOIN hours ON users.id = hours.id";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::Star,
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Keyword(Keyword::Inner),
            Token::Keyword(Keyword::Join),
            Token::Identifier("hours".to_string()),
            Token::Keyword(Keyword::On),
            Token::Identifier("users".to_string()),
            Token::Dot,
            Token::Identifier("id".to_string()),
            Token::Equal,
            Token::Identifier("hours".to_string()),
            Token::Dot,
            Token::Identifier("id".to_string()),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_select_big_selection() {
        let example = "SELECT one, two, three \
                              FROM users \
                              WHERE one=23 AND two>=55 AND three<=40";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::Identifier("one".to_string()),
            Token::Comma,
            Token::Identifier("two".to_string()),
            Token::Comma,
            Token::Identifier("three".to_string()),
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Keyword(Keyword::Where),
            Token::Identifier("one".to_string()),
            Token::Equal,
            Token::Number(23),
            Token::Keyword(Keyword::And),
            Token::Identifier("two".to_string()),
            Token::GreaterThanOrEqual,
            Token::Number(55),
            Token::Keyword(Keyword::And),
            Token::Identifier("three".to_string()),
            Token::LessThanOrEqual,
            Token::Number(40),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_select_same_keyword_identifier() {
        let example = "SELECT \"from\" FROM users";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::String("from".to_string()),
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_insert() {
        let example = "INSERT INTO users (one, two, three) VALUES (1, 2, 3)";
        let token_iterator = TokenIterator::new(example);

        assert_eq!(vec![
            Token::Keyword(Keyword::Insert),
            Token::Keyword(Keyword::Into),
            Token::Identifier("users".to_string()),
            Token::LeftParentheses,
            Token::Identifier("one".to_string()),
            Token::Comma,
            Token::Identifier("two".to_string()),
            Token::Comma,
            Token::Identifier("three".to_string()),
            Token::RightParentheses,
            Token::Keyword(Keyword::Values),
            Token::LeftParentheses,
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::Comma,
            Token::Number(3),
            Token::RightParentheses,
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_float() {
        let example = "123.456";
        let token_iterator = TokenIterator::new(example);
        assert_eq!(vec![
            Token::Float(123.456),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }

    #[test]
    fn test_float_big() {
        let example = "SELECT one, two, three \
                              FROM users \
                              WHERE one=23.9871 AND two>=41.184561 AND three<=40.8749841";
        let token_iterator = TokenIterator::new(example);
        assert_eq!(vec![
            Token::Keyword(Keyword::Select),
            Token::Identifier("one".to_string()),
            Token::Comma,
            Token::Identifier("two".to_string()),
            Token::Comma,
            Token::Identifier("three".to_string()),
            Token::Keyword(Keyword::From),
            Token::Identifier("users".to_string()),
            Token::Keyword(Keyword::Where),
            Token::Identifier("one".to_string()),
            Token::Equal,
            Token::Float(23.9871),
            Token::Keyword(Keyword::And),
            Token::Identifier("two".to_string()),
            Token::GreaterThanOrEqual,
            Token::Float(41.184561),
            Token::Keyword(Keyword::And),
            Token::Identifier("three".to_string()),
            Token::LessThanOrEqual,
            Token::Float(40.8749841),
            Token::Eof
        ],
                   token_iterator.map(|res| res.unwrap()).collect::<Vec<_>>());
    }
}