use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use crate::statement::{BinaryOperator, Constraint, DBType, Expression, Statement, TableColumn};
use crate::token::{Keyword, Token};
use crate::tokenizer::{TokenIterator, TokenizerError};

fn is_token_expression_ending(token: &Token) -> bool {
    match token {
        // tokens that mean an end of an arithmetic-logic expression
        Token::Keyword(Keyword::From) | Token::Keyword(Keyword::Where) |
        Token::Keyword(Keyword::Order) | Token::Keyword(Keyword::Values)
        | Token::Comma | Token::Semicolon | Token::Eof => true,
        _ => false
    }
}

#[derive(PartialEq)]
pub struct ParserError(String);

impl Debug for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for ParserError {}

impl From<&str> for ParserError {
    fn from(value: &str) -> Self {
        ParserError("parser error: ".to_string() + value)
    }
}

impl From<String> for ParserError {
    fn from(value: String) -> Self {
        ParserError("parser error: ".to_owned() + &*value)
    }
}

impl From<TokenizerError> for ParserError {
    fn from(value: TokenizerError) -> Self {
        ParserError(value.0)
    }
}

impl From<&TokenizerError> for ParserError {
    fn from(value: &TokenizerError) -> Self {
        ParserError(value.0.clone())
    }
}

pub(crate) type ParserResult<T> = Result<T, ParserError>;

pub struct Parser<'t> {
    tokenizer: Peekable<TokenIterator<'t>>
}

impl<'t> Parser<'t> {
    pub fn new(query: &'t str) -> Self {
        Parser {
            tokenizer: TokenIterator::new(query).peekable(),
        }
    }

    pub fn build_ast(&mut self) -> ParserResult<Statement> {
        let first_token = self.tokenizer.next().unwrap()?;

        let ret_val = match first_token {
            Token::Keyword(Keyword::Select) => {
                let column_expressions = self.parse_comma_separated(Self::parse_single_expression, false)?;

                if column_expressions.is_empty() {
                    return Err("at least one column must be selected".into());
                }

                if !self.check_consume_next(Token::Keyword(Keyword::From))? {
                    return Err("keyword FROM not found".into());
                }

                // this can, in theory, be expanded to support joins, as joins are just expressions with an
                // added condition on the ON keyword
                let from_expression = self.parse_single_expression()?;

                let where_expression = if !self.check_consume_next(Token::Keyword(Keyword::Where))? {
                    None
                } else {
                    Some(self.parse_single_expression()?)
                };

                let orderby_expression = if self.check_consume_next(Token::Keyword(Keyword::Order))? {
                    if !self.check_consume_next(Token::Keyword(Keyword::By))? {
                        return Err("DELETE not followed by FROM".into());
                    }

                    self.parse_comma_separated(Self::parse_single_expression, false)?
                } else {
                    Vec::new()
                };

                Statement::Select {
                    columns: column_expressions,
                    from: from_expression,
                    r#where: where_expression,
                    orderby: orderby_expression,
                }
            }
            Token::Keyword(Keyword::Update) => {
                let table = self.parse_single_identifier()?;

                if !self.check_consume_next(Token::Keyword(Keyword::Set))? {
                    return Err("keyword SET not found".into());
                }

                let new_value_expressions = self.parse_comma_separated(Self::parse_single_equal_expression, false)?;

                let where_expression = if !self.check_consume_next(Token::Keyword(Keyword::Where))? {
                    None
                } else {
                    Some(self.parse_single_expression()?)
                };

                Statement::Update {
                    table,
                    new_values: new_value_expressions,
                    r#where: where_expression,
                }
            }
            Token::Keyword(Keyword::Delete) => {
                if !self.check_consume_next(Token::Keyword(Keyword::From))? {
                    return Err("DELETE not followed by FROM".into())
                }

                let table = self.parse_single_identifier()?;

                let where_expression = if !self.check_consume_next(Token::Keyword(Keyword::Where))? {
                    None
                } else {
                    Some(self.parse_single_expression()?)
                };

                Statement::Delete {
                    table,
                    r#where: where_expression,
                }
            }
            Token::Keyword(Keyword::Insert) => {
                if !self.check_consume_next(Token::Keyword(Keyword::Into))? {
                    return Err("INSERT not followed by INTO".into())
                }

                let table = self.parse_single_identifier()?;

                let column_order = if let Token::LeftParentheses = self.tokenizer.peek().unwrap().as_ref()? {
                    self.parse_comma_separated(Self::parse_single_identifier, true)?
                } else {
                    Vec::new()
                };

                if !self.check_consume_next(Token::Keyword(Keyword::Values))? {
                    return Err("keyword VALUES not found".into());
                }

                let values = self.parse_comma_separated(Self::parse_single_value, true)?;

                Statement::Insert {
                    table,
                    column_order,
                    values
                }
            }
            Token::Keyword(Keyword::Drop) => {
                let is_database;
                match self.tokenizer.peek().unwrap().as_ref()? {
                    token @ (Token::Keyword(Keyword::Table) | Token::Keyword(Keyword::Database)) => {
                        is_database = *token == Token::Keyword(Keyword::Database);
                        self.tokenizer.next();
                    }
                    _ => return Err("DROP not followed by TABLE or DATABASE".into())
                }

                let table_or_database = self.parse_single_identifier()?;

                Statement::Drop {
                    table_or_database,
                    is_database
                }
            }
            Token::Keyword(Keyword::Create) => {
                match self.tokenizer.peek().unwrap().as_ref()? {
                    Token::Keyword(Keyword::Unique) | Token::Keyword(Keyword::Index) => {
                        let is_unique = self.check_consume_next(Token::Keyword(Keyword::Unique))?;

                        if is_unique {
                            if !self.check_consume_next(Token::Keyword(Keyword::Index))? {
                                return Err("UNIQUE not followed by INDEX".into());
                            }
                        } else {
                            self.tokenizer.next();
                        }

                        let index_name = self.parse_single_identifier()?;

                        if !self.check_consume_next(Token::Keyword(Keyword::On))? {
                            return Err("ON keyword expected".into());
                        }

                        let table_name = self.parse_single_identifier()?;

                        let column_list = self.parse_comma_separated(Self::parse_single_identifier, true)?;

                        Statement::CreateIndex {
                            index_name,
                            table_name,
                            column_list,
                            is_unique
                        }
                    }
                    Token::Keyword(Keyword::Database) => {
                        self.tokenizer.next();

                        let database_name = self.parse_single_identifier()?;

                        Statement::CreateDatabase {
                            database_name
                        }
                    }
                    Token::Keyword(Keyword::Table) => {
                        self.tokenizer.next();

                        let table_name = self.parse_single_identifier()?;

                        let column_list = self.parse_comma_separated(Self::parse_single_column, true)?;

                        Statement::CreateTable {
                            table_name,
                            column_list
                        }
                    }
                    _ => return Err("CREATE not followed by TABLE, INDEX or UNIQUE INDEX".into())
                }
            }
            Token::Keyword(Keyword::Start) => {
                if !self.check_consume_next(Token::Keyword(Keyword::Transaction))? {
                    return Err("START not followed by TRANSACTION".into());
                }

                Statement::StartTransaction
            }
            Token::Keyword(Keyword::Rollback) => Statement::RollbackTransaction,
            Token::Keyword(Keyword::Commit) => Statement::CommitTransaction,
            Token::Keyword(Keyword::Explain) => {
                Statement::Explain {
                    statement: Box::new(self.build_ast()?)
                }
            }
            Token::Eof => return Err("empty statement query".into()),
            _ => return Err("the first keyword must be either: select, update, delete, insert, create, drop".into())
        };

        if !self.check_consume_next(Token::Semicolon)? {
            return Err("statement not ended with a semicolon".into());
        }

        Ok(ret_val)
    }

    fn check_consume_next(&mut self, next_token: Token) -> ParserResult<bool> {
        if *self.tokenizer.peek().unwrap().as_ref()? == next_token {
            self.tokenizer.next();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn parse_comma_separated<P>(&mut self, mut parser: impl FnMut(&mut Self) -> ParserResult<P>, check_parentheses: bool) -> ParserResult<Vec<P>> {
        if !self.check_consume_next(Token::LeftParentheses)? && check_parentheses {
            return Err("left parentheses not found".into());
        }

        let mut comma_separated_parsables = vec![parser(self)?];
        while self.check_consume_next(Token::Comma)? {
            comma_separated_parsables.push(parser(self)?);
        }

        if !self.check_consume_next(Token::RightParentheses)? && check_parentheses {
            return Err("right parentheses not found".into());
        }

        Ok(comma_separated_parsables)
    }

    fn parse_single_expression(&mut self) -> ParserResult<Expression> {
        ExpressionBuilder::new(&mut self.tokenizer).build_expression(0)
    }

    fn parse_single_identifier(&mut self) -> ParserResult<Expression> {
        match self.parse_single_expression()? {
            expr @ Expression::Identifier(_) => Ok(expr),
            _ => Err("not an identifier".into())
        }
    }

    fn parse_single_value(&mut self) -> ParserResult<Expression> {
        match self.parse_single_expression()? {
            expr @ (Expression::Number(_) | Expression::Float(_) | Expression::String(_)) => Ok(expr),
            _ => Err("not a value".into())
        }
    }

    fn parse_single_equal_expression(&mut self) -> ParserResult<Expression> {
        match self.parse_single_expression()? {
            Expression::BinaryOperation { left_operand, operator, right_operand } => {
                match (&*left_operand, &operator, &*right_operand) {
                    (Expression::Identifier(_), BinaryOperator::Equal, Expression::String(_)) |
                    (Expression::Identifier(_), BinaryOperator::Equal, Expression::Float(_)) |
                    (Expression::Identifier(_), BinaryOperator::Equal, Expression::Number(_)) =>
                        Ok(Expression::BinaryOperation { left_operand, operator, right_operand }),
                    _ => Err("not an equals expression".into())
                }
            },
            _ => Err("not an equals expression".into())
        }
    }

    fn parse_single_column(&mut self) -> ParserResult<TableColumn> {
        let column_name = self.parse_single_identifier()?;

        let column_type = match self.tokenizer.next().unwrap()? {
            Token::Keyword(Keyword::Int) => DBType::Int,
            Token::Keyword(Keyword::BigInt) => DBType::BigInt,
            Token::Keyword(Keyword::Decimal) => {
                if !self.check_consume_next(Token::LeftParentheses)? {
                    return Err("missing left parentheses on decimal type".into());
                }

                let integral_part_len = if let Token::Number(len) = self.tokenizer.next().unwrap()? {
                    len as usize
                } else {
                    return Err("expected integral part on decimal type".into());
                };

                if !self.check_consume_next(Token::Comma)? {
                    return Err("missing comma on decimal type".into());
                }

                let fractional_part_len = if let Token::Number(len) = self.tokenizer.next().unwrap()? {
                    len as usize
                } else {
                    return Err("expected fractional part on decimal type".into());
                };

                if !self.check_consume_next(Token::RightParentheses)? {
                    return Err("missing right parentheses on decimal type".into());
                }

                DBType::Decimal(integral_part_len, fractional_part_len)
            }
            Token::Keyword(Keyword::Varchar) => {
                if !self.check_consume_next(Token::LeftParentheses)? {
                    return Err("missing left parentheses on varchar type".into());
                }

                let length = if let Token::Number(len) = self.tokenizer.next().unwrap()? {
                    len as usize
                } else {
                    return Err("expected number on varchar type".into());
                };

                if !self.check_consume_next(Token::RightParentheses)? {
                    return Err("missing right parentheses on varchar type".into());
                }

                DBType::Varchar(length)
            }
            Token::Keyword(Keyword::Bool) => DBType::Bool,
            _ => return Err("expected column type".into())
        };

        let mut constraints = vec![];

        let constraint_tokens = &[
            Token::Keyword(Keyword::Not), Token::Keyword(Keyword::Primary), Token::Keyword(Keyword::Foreign),
            Token::Keyword(Keyword::Unique), Token::Keyword(Keyword::Check), Token::Keyword(Keyword::Default)
        ];

        while constraint_tokens.contains(self.tokenizer.peek().unwrap().as_ref()?) {
            constraints.push(match self.tokenizer.next().unwrap()? {
                Token::Keyword(Keyword::Not) => {
                    if !self.check_consume_next(Token::Keyword(Keyword::Null))? {
                        return Err("NOT not followed by NULL for the constraint".into());
                    }

                    Constraint::NotNull
                }
                Token::Keyword(Keyword::Primary) => {
                    if !self.check_consume_next(Token::Keyword(Keyword::Key))? {
                        return Err("PRIMARY not followed by KEY for the constraint".into());
                    }

                    Constraint::PrimaryKey
                }
                Token::Keyword(Keyword::Foreign) => {
                    if !self.check_consume_next(Token::Keyword(Keyword::Key))? {
                        return Err("FOREIGN not followed by KEY for the constraint".into());
                    }

                    if !self.check_consume_next(Token::Keyword(Keyword::References))? {
                        return Err("REFERENCES not followed by FOREIGN KEY".into());
                    }

                    let referenced_table = self.parse_single_identifier()?;

                    if !self.check_consume_next(Token::LeftParentheses)? {
                        return Err("missing left parentheses on FOREIGN KEY constraint".into());
                    }

                    let referenced_column = self.parse_single_identifier()?;

                    if !self.check_consume_next(Token::RightParentheses)? {
                        return Err("missing right parentheses on FOREIGN KEY constraint".into());
                    }

                    Constraint::ForeignKey { referenced_column, referenced_table }
                }
                Token::Keyword(Keyword::Unique) => Constraint::Unique,
                Token::Keyword(Keyword::Check) => {
                    if !self.check_consume_next(Token::LeftParentheses)? {
                        return Err("missing left parentheses on CHECK constraint".into());
                    }

                    let check_expr = self.parse_single_expression()?;

                    if !self.check_consume_next(Token::RightParentheses)? {
                        return Err("missing right parentheses on CHECK constraint".into());
                    }

                    Constraint::Check(check_expr)
                }
                Token::Keyword(Keyword::Default) => {
                    if !self.check_consume_next(Token::LeftParentheses)? {
                        return Err("missing left parentheses on DEFAULT constraint".into());
                    }

                    let default_expt = self.parse_single_expression()?;

                    if !self.check_consume_next(Token::RightParentheses)? {
                        return Err("missing right parentheses on DEFAULT constraint".into());
                    }

                    Constraint::Default(default_expt)
                }
                _ => unreachable!()
            })
        }

        Ok(TableColumn { column_name, column_type, constraints })
    }
}

struct ExpressionBuilder<'t, 'i> {
    tokenizer: &'i mut Peekable<TokenIterator<'t>>
}

impl<'t, 'i> ExpressionBuilder<'t, 'i> {
    const PREFIX_PRECEDENCE: u8 = 100;

    fn new(tokenizer: &'i mut Peekable<TokenIterator<'t>>) -> Self {
        ExpressionBuilder {
            tokenizer
        }
    }

    fn build_expression(&mut self, current_precedence: u8) -> ParserResult<Expression> {
        let mut expression = self.get_prefix_expr()?;
        let mut next_precedence = self.get_precedence_infix()?;

        while current_precedence < next_precedence {
            expression = self.get_infix_expr(expression, next_precedence)?;
            next_precedence = self.get_precedence_infix()?;
        }

        Ok(expression)
    }

    fn get_prefix_expr(&mut self) -> ParserResult<Expression> {
        Ok(match self.tokenizer.next().unwrap()? {
            Token::Identifier(str) => Expression::Identifier(str),
            Token::String(str) => Expression::String(str),
            Token::Number(num) => Expression::Number(num),
            Token::Float(num) => Expression::Float(num),
            Token::Keyword(Keyword::True) => Expression::Bool(true),
            Token::Keyword(Keyword::False) => Expression::Bool(false),
            token @ (Token::Minus | Token::Plus | Token::Keyword(Keyword::Not)) => {
                Expression::UnaryOperation {
                    operand: Box::new(self.build_expression(Self::PREFIX_PRECEDENCE)?),
                    operator: token.into(),
                }
            },
            Token::Star => {
                match self.tokenizer.peek().unwrap() {
                    Ok(next_token) if is_token_expression_ending(next_token) => Expression::Identifier("*".to_string()),
                    Ok(_) => return Err("wildcard followed by other operators".into()),
                    Err(e) => return Err(e.into())
                }
            },
            Token::LeftParentheses => {
                let ret_expr = self.build_expression(0)?;

                match self.tokenizer.peek().unwrap() {
                    Ok(next_token) if *next_token == Token::RightParentheses => {
                        self.tokenizer.next();
                        ret_expr
                    },
                    Ok(_) => return Err("right parentheses not found".into()),
                    Err(e) => return Err(e.into())
                }
            },
            Token::Eof => return Err("expression ended early".into()),
            invalid_token => return Err(format!("invalid token: {}", invalid_token).into())
        })
    }

    fn get_infix_expr(&mut self, left: Expression, current_precedence: u8) -> ParserResult<Expression> {
        Ok(match self.tokenizer.next().unwrap()? {
            // ASC and DESC operators are 'postfix' operators,
            // a special variation of the 'infix' operators
            // where there is no right side of the operation
            token @ (Token::Keyword(Keyword::Asc) | Token::Keyword(Keyword::Desc)) => {
                Expression::UnaryOperation {
                    operand: Box::new(left),
                    operator: token.into(),
                }
            }
            // all other tokens are 'infix' operators,
            // for whom the right side is calculated as a subexpression
            token => {
                Expression::BinaryOperation {
                    left_operand: Box::new(left),
                    operator: token.into(),
                    right_operand: Box::new(self.build_expression(current_precedence)?),
                }
            }
        })
    }

    fn get_precedence_infix(&mut self) -> ParserResult<u8> {
        Ok(match self.tokenizer.peek().unwrap().as_ref()? {
            Token::Divide | Token::Star => 30,
            Token::Minus | Token::Plus => 25,
            Token::Equal | Token::LessThanOrEqual | Token::LessThan |
            Token::GreaterThanOrEqual | Token::GreaterThan | Token::NotEqual => 20,
            Token::Keyword(Keyword::And) => 15,
            Token::Keyword(Keyword::Or) => 10,
            Token::Keyword(Keyword::Asc) | Token::Keyword(Keyword::Desc) => 5,
            _ => 0
        })
    }
}

#[cfg(test)]
mod tests_expression_builder {
    use crate::parser::{ExpressionBuilder, ParserError};
    use crate::statement::{BinaryOperator, Expression, UnaryOperator};
    use crate::tokenizer::TokenIterator;

    #[test]
    fn test_precedence_simple() {
        let query = "a / b - 13";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("a".to_string())),
                    operator: BinaryOperator::Divide,
                    right_operand: Box::new(Expression::Identifier("b".to_string()))
                }),
                operator: BinaryOperator::Minus,
                right_operand: Box::new(Expression::Number(13))
            }
        )
    }

    #[test]
    fn test_precedence_unary() {
        let query = "-c / d - 13";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::UnaryOperation {
                        operand: Box::new(Expression::Identifier("c".to_string())),
                        operator: UnaryOperator::Minus,
                    }),
                    operator: BinaryOperator::Divide,
                    right_operand: Box::new(Expression::Identifier("d".to_string()))
                }),
                operator: BinaryOperator::Minus,
                right_operand: Box::new(Expression::Number(13))
            }
        )
    }

    #[test]
    fn test_precedence_unary_all() {
        let query = "-a AND NOT b ";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::UnaryOperation {
                    operand: Box::new(Expression::Identifier("a".to_string())),
                    operator: UnaryOperator::Minus,
                }),
                operator: BinaryOperator::And,
                right_operand: Box::new(Expression::UnaryOperation {
                    operand: Box::new(Expression::Identifier("b".to_string())),
                    operator: UnaryOperator::Not,
                })
            }
        )
    }

    #[test]
    fn test_precedence_complex() {
        let query = "a * (b - 17) / 4 + 22 * (c + 50 / 25)";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("a".to_string())),
                        operator: BinaryOperator::Multiply,
                        right_operand: Box::new(Expression::BinaryOperation {
                            left_operand: Box::new(Expression::Identifier("b".to_string())),
                            operator: BinaryOperator::Minus,
                            right_operand: Box::new(Expression::Number(17)),
                        }),
                    }),
                    operator: BinaryOperator::Divide,
                    right_operand: Box::new(Expression::Number(4)),
                }),
                operator: BinaryOperator::Plus,
                right_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Number(22)),
                    operator: BinaryOperator::Multiply,
                    right_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("c".to_string())),
                        operator: BinaryOperator::Plus,
                        right_operand: Box::new(Expression::BinaryOperation {
                            left_operand: Box::new(Expression::Number(50)),
                            operator: BinaryOperator::Divide,
                            right_operand: Box::new(Expression::Number(25)),
                        }),
                    }),
                }),
            }
        )
    }

    #[test]
    fn test_floating_number() {
        let query = "a * -1.1";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::Identifier("a".to_string())),
                operator: BinaryOperator::Multiply,
                right_operand: Box::new(Expression::UnaryOperation {
                    operand: Box::new(Expression::Float(1.1)),
                    operator: UnaryOperator::Minus,
                })
            }
        )
    }

    #[test]
    fn test_identifier_solo() {
        let query = "identifier";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::Identifier("identifier".to_string())
        )
    }

    #[test]
    fn test_and_or_precedence_simple() {
        let query = "a AND b OR c AND d";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("a".to_string())),
                    operator: BinaryOperator::And,
                    right_operand: Box::new(Expression::Identifier("b".to_string()))
                }),
                operator: BinaryOperator::Or,
                right_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("c".to_string())),
                    operator: BinaryOperator::And,
                    right_operand: Box::new(Expression::Identifier("d".to_string()))
                })
            }
        )
    }

    #[test]
    fn test_precedence_not() {
        let query = "NOT a AND NOT b";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::UnaryOperation {
                    operand: Box::new(Expression::Identifier("a".to_string())),
                    operator: UnaryOperator::Not
                }),
                operator: BinaryOperator::And,
                right_operand: Box::new(Expression::UnaryOperation {
                    operand: Box::new(Expression::Identifier("b".to_string())),
                    operator: UnaryOperator::Not
                })
            }
        )
    }

    #[test]
    fn test_where_precedence_complex() {
        let query = "(a * 13) > 15 AND NOT b OR c / 17 < (d - 44)";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::BinaryOperation {
                            left_operand: Box::new(Expression::Identifier("a".to_string())),
                            operator: BinaryOperator::Multiply,
                            right_operand: Box::new(Expression::Number(13))
                        }),
                        operator: BinaryOperator::GreaterThan,
                        right_operand: Box::new(Expression::Number(15))
                    }),
                    operator: BinaryOperator::And,
                    right_operand: Box::new(Expression::UnaryOperation {
                        operand: Box::new(Expression::Identifier("b".to_string())),
                        operator: UnaryOperator::Not
                    })
                }),
                operator: BinaryOperator::Or,
                right_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("c".to_string())),
                        operator: BinaryOperator::Divide,
                        right_operand: Box::new(Expression::Number(17))
                    }),
                    operator: BinaryOperator::LessThan,
                    right_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("d".to_string())),
                        operator: BinaryOperator::Minus,
                        right_operand: Box::new(Expression::Number(44))
                    })
                })
            }
        )
    }

    #[test]
    fn test_end_with_comma() {
        let query = "y + n - c, not_important";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("y".to_string())),
                    operator: BinaryOperator::Plus,
                    right_operand: Box::new(Expression::Identifier("n".to_string()))
                }),
                operator: BinaryOperator::Minus,
                right_operand: Box::new(Expression::Identifier("c".to_string()))
            }
        )
    }

    #[test]
    fn test_orderby_precedence_simple() {
        let query = "salary ASC";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::UnaryOperation {
                operand: Box::new(Expression::Identifier("salary".to_string())),
                operator: UnaryOperator::Asc
            }
        )
    }

    #[test]
    fn test_orderby_precedence_complex() {
        let query = "(salary * (1 + inflation)) DESC";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::UnaryOperation {
                operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("salary".to_string())),
                    operator: BinaryOperator::Multiply,
                    right_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Number(1)),
                        operator: BinaryOperator::Plus,
                        right_operand: Box::new(Expression::Identifier("inflation".to_string()))
                    })
                }),
                operator: UnaryOperator::Desc
            }
        )
    }

    #[test]
    fn test_true_false_keywords() {
        let query = "a = TRUE OR b != FALSE";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0).unwrap(),
            Expression::BinaryOperation {
                left_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("a".to_string())),
                    operator: BinaryOperator::Equal,
                    right_operand: Box::new(Expression::Bool(true))
                }),
                operator: BinaryOperator::Or,
                right_operand: Box::new(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("b".to_string())),
                    operator: BinaryOperator::NotEqual,
                    right_operand: Box::new(Expression::Bool(false))
                })
            }
        )
    }

    #[test]
    fn test_incorrect_syntax_1() {
        let query = "a *";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0),
            Err(ParserError("parser error: expression ended early".into()))
        )
    }

    #[test]
    fn test_incorrect_syntax_2() {
        let query = "* 3";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0),
            Err(ParserError("parser error: wildcard followed by other operators".into()))
        )
    }

    #[test]
    fn test_incorrect_syntax_3() {
        let query = "/ 3";
        let tokenizer = TokenIterator::new(query);
        let mut token_stream = tokenizer.peekable();

        let mut expression_builder = ExpressionBuilder::new(&mut token_stream);

        assert_eq!(
            expression_builder.build_expression(0),
            Err(ParserError("parser error: invalid token: /".into()))
        )
    }
}

#[cfg(test)]
mod tests_parser {
    use crate::parser::Parser;
    use crate::statement::{BinaryOperator, Constraint, DBType, Expression, Statement, TableColumn, UnaryOperator};

    #[test]
    fn test_select_columns() {
        let query = "SELECT one, two FROM users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![Expression::Identifier("one".to_owned()), Expression::Identifier("two".to_owned())],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_string_as_column() {
        let query = "SELECT one, 'this is a string' FROM users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![Expression::Identifier("one".to_owned()), Expression::String("this is a string".to_owned())],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_columns_star() {
        let query = "SELECT * FROM users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![Expression::Identifier("*".to_string())],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_columns_star_multiple() {
        let query = "SELECT *, *, star * 2 FROM users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![
                    Expression::Identifier("*".to_string()),
                    Expression::Identifier("*".to_string()),
                    Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("star".to_string())),
                        operator: BinaryOperator::Multiply,
                        right_operand: Box::new(Expression::Number(2))
                    }
                ],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_columns_expressions() {
        let query = "SELECT one * 12 FROM users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![Expression::BinaryOperation {
                    left_operand: Box::new(Expression::Identifier("one".to_string())),
                    operator: BinaryOperator::Multiply,
                    right_operand: Box::new(Expression::Number(12))
                }],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_columns_expressions_multiple() {
        let query = "SELECT one * 12, two, three - 14 * 22 - 7 FROM users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![
                    Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("one".to_string())),
                        operator: BinaryOperator::Multiply,
                        right_operand: Box::new(Expression::Number(12))
                    },
                    Expression::Identifier("two".to_string()),
                    Expression::BinaryOperation {
                        left_operand: Box::new(Expression::BinaryOperation {
                            left_operand: Box::new(Expression::Identifier("three".to_string())),
                            operator: BinaryOperator::Minus,
                            right_operand: Box::new(Expression::BinaryOperation {
                                left_operand: Box::new(Expression::Number(14)),
                                operator: BinaryOperator::Multiply,
                                right_operand: Box::new(Expression::Number(22))
                            })
                        }),
                        operator: BinaryOperator::Minus,
                        right_operand: Box::new(Expression::Number(7))
                    },
                ],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_with_where() {
        let query = "SELECT name, surname FROM users WHERE name = \"Voldemort\" AND surname = 'Riddle';";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![
                    Expression::Identifier("name".to_string()),
                    Expression::Identifier("surname".to_string())
                ],
                from: Expression::Identifier("users".to_string()),
                r#where: Some(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("name".to_string())),
                        operator: BinaryOperator::Equal,
                        right_operand: Box::new(Expression::String("Voldemort".to_string()))
                    }),
                    operator: BinaryOperator::And,
                    right_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("surname".to_string())),
                        operator: BinaryOperator::Equal,
                        right_operand: Box::new(Expression::String("Riddle".to_string()))
                    }),
                }),
                orderby: vec![],
            }
        )
    }

    #[test]
    fn test_select_with_order_by() {
        let query = "SELECT id, salary FROM users ORDER BY salary - 2.3 * 10 ASC, id DESC;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Select {
                columns: vec![
                    Expression::Identifier("id".to_string()),
                    Expression::Identifier("salary".to_string())
                ],
                from: Expression::Identifier("users".to_string()),
                r#where: None,
                orderby: vec![
                    Expression::UnaryOperation {
                        operand: Box::new(Expression::BinaryOperation {
                            left_operand: Box::new(Expression::Identifier("salary".to_string())),
                            operator: BinaryOperator::Minus,
                            right_operand: Box::new(Expression::BinaryOperation {
                                left_operand: Box::new(Expression::Float(2.3)),
                                operator: BinaryOperator::Multiply,
                                right_operand: Box::new(Expression::Number(10))
                            })
                        }),
                        operator: UnaryOperator::Asc
                    },
                    Expression::UnaryOperation {
                        operand: Box::new(Expression::Identifier("id".to_string())),
                        operator: UnaryOperator::Desc
                    },
                ],
            }
        )
    }

    #[test]
    fn test_update_with_where() {
        let query = "UPDATE users SET name = \"Tom\", age = 15 WHERE name = \"Voldemort\" AND surname = 'Riddle';";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Update {
                new_values: vec![
                    Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("name".to_string())),
                        operator: BinaryOperator::Equal,
                        right_operand: Box::new(Expression::String("Tom".to_string()))
                    },
                    Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("age".to_string())),
                        operator: BinaryOperator::Equal,
                        right_operand: Box::new(Expression::Number(15))
                    },
                ],
                table: Expression::Identifier("users".to_string()),
                r#where: Some(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("name".to_string())),
                        operator: BinaryOperator::Equal,
                        right_operand: Box::new(Expression::String("Voldemort".to_string()))
                    }),
                    operator: BinaryOperator::And,
                    right_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("surname".to_string())),
                        operator: BinaryOperator::Equal,
                        right_operand: Box::new(Expression::String("Riddle".to_string()))
                    }),
                }),
            }
        )
    }

    #[test]
    fn test_update_with_bad_new_values() {
        let query = "UPDATE users SET name != \"Tom\", age = 15 * test;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast(),
            Err("not an equals expression".into())
        )
    }

    #[test]
    fn test_delete_with_where() {
        let query = "DELETE FROM users WHERE salary < 35000 OR work_week_hours <= 40;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Delete {
                table: Expression::Identifier("users".to_string()),
                r#where: Some(Expression::BinaryOperation {
                    left_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("salary".to_string())),
                        operator: BinaryOperator::LessThan,
                        right_operand: Box::new(Expression::Number(35000))
                    }),
                    operator: BinaryOperator::Or,
                    right_operand: Box::new(Expression::BinaryOperation {
                        left_operand: Box::new(Expression::Identifier("work_week_hours".to_string())),
                        operator: BinaryOperator::LessThanOrEqual,
                        right_operand: Box::new(Expression::Number(40))
                    }),
                }),
            }
        )
    }

    #[test]
    fn test_drop() {
        let query = "DROP TABLE users;";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Drop {
                table_or_database: Expression::Identifier("users".to_string()),
                is_database: false
            }
        )
    }

    #[test]
    fn test_insert_into_all() {
        let query = "INSERT INTO users VALUES (1, 'Test', 'Testing', 22750);";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Insert {
                table: Expression::Identifier("users".to_string()),
                column_order: vec![],
                values: vec![
                    Expression::Number(1),
                    Expression::String("Test".to_string()),
                    Expression::String("Testing".to_string()),
                    Expression::Number(22750),
                ],
            }
        )
    }

    #[test]
    fn test_insert_into_custom() {
        let query = "INSERT INTO users (id, name, surname) VALUES (1, 'Test', 'Testing');";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::Insert {
                table: Expression::Identifier("users".to_string()),
                column_order: vec![
                    Expression::Identifier("id".to_string()),
                    Expression::Identifier("name".to_string()),
                    Expression::Identifier("surname".to_string()),
                ],
                values: vec![
                    Expression::Number(1),
                    Expression::String("Test".to_string()),
                    Expression::String("Testing".to_string()),
                ],
            }
        )
    }

    #[test]
    fn test_insert_with_more_than_identifiers() {
        let query = "INSERT INTO users (id = 5, name = 'Me', surname = 'Mine') VALUES (1, 'Test', 'Testing');";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast(),
            Err("not an identifier".into())
        )
    }

    #[test]
    fn test_insert_with_more_than_values() {
        let query = "INSERT INTO users (id, name, surname) VALUES (1 > 5, 'Test' + 'Test2', 'Testing' + 'Testing2');";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast(),
            Err("not a value".into())
        )
    }

    #[test]
    fn test_create_table_simple() {
        let query = "CREATE TABLE simple_table(\
            first_col INT,\
            second_col DECIMAL(1, 2),\
            third_col VARCHAR(255),\
            fourth_col BOOL,\
            fifth_col BIGINT\
        );";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::CreateTable {
                table_name: Expression::Identifier("simple_table".to_string()),
                column_list: vec![
                    TableColumn {
                        column_name: Expression::Identifier("first_col".to_string()),
                        column_type: DBType::Int,
                        constraints: vec![],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("second_col".to_string()),
                        column_type: DBType::Decimal(1, 2),
                        constraints: vec![],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("third_col".to_string()),
                        column_type: DBType::Varchar(255),
                        constraints: vec![],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("fourth_col".to_string()),
                        column_type: DBType::Bool,
                        constraints: vec![],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("fifth_col".to_string()),
                        column_type: DBType::BigInt,
                        constraints: vec![],
                    },
                ],
            }
        )
    }

    #[test]
    fn test_create_table_complex() {
        let query = "CREATE TABLE complex_table(\
            id INT PRIMARY KEY,\
            email VARCHAR(255) NOT NULL UNIQUE,\
            is_junior BOOL DEFAULT(TRUE),\
            age INT CHECK(age >= 18),\
            social_security_number INT FOREIGN KEY REFERENCES ss_nums(social_security_number)
        );";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::CreateTable {
                table_name: Expression::Identifier("complex_table".to_string()),
                column_list: vec![
                    TableColumn {
                        column_name: Expression::Identifier("id".to_string()),
                        column_type: DBType::Int,
                        constraints: vec![Constraint::PrimaryKey],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("email".to_string()),
                        column_type: DBType::Varchar(255),
                        constraints: vec![Constraint::NotNull, Constraint::Unique],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("is_junior".to_string()),
                        column_type: DBType::Bool,
                        constraints: vec![Constraint::Default(Expression::Bool(true))],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("age".to_string()),
                        column_type: DBType::Int,
                        constraints: vec![Constraint::Check(
                            Expression::BinaryOperation {
                                left_operand: Box::new(Expression::Identifier("age".to_string())),
                                operator: BinaryOperator::GreaterThanOrEqual,
                                right_operand: Box::new(Expression::Number(18))
                            })],
                    },
                    TableColumn {
                        column_name: Expression::Identifier("social_security_number".to_string()),
                        column_type: DBType::Int,
                        constraints: vec![Constraint::ForeignKey {
                            referenced_table: Expression::Identifier("ss_nums".to_string()),
                            referenced_column: Expression::Identifier("social_security_number".to_string()),
                        }],
                    },
                ],
            }
        )
    }

    #[test]
    fn test_create_index() {
        let query = "CREATE INDEX test_index ON test_table (name, surname);";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::CreateIndex {
                index_name: Expression::Identifier("test_index".to_string()),
                table_name: Expression::Identifier("test_table".to_string()),
                is_unique: false,
                column_list: vec![
                    Expression::Identifier("name".to_string()),
                    Expression::Identifier("surname".to_string())
                ],
            }
        )
    }

    #[test]
    fn test_create_unique_index() {
        let query = "CREATE UNIQUE INDEX test_unique_index ON test_table (id);";

        let mut parser = Parser::new(query);
        assert_eq!(
            parser.build_ast().unwrap(),
            Statement::CreateIndex {
                index_name: Expression::Identifier("test_unique_index".to_string()),
                table_name: Expression::Identifier("test_table".to_string()),
                is_unique: true,
                column_list: vec![
                    Expression::Identifier("id".to_string()),
                ],
            }
        )
    }
}
