use crate::token::{Keyword, Token};
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Select {
        columns: Vec<Expression>,
        from: Expression,
        r#where: Option<Expression>,
        orderby: Vec<Expression>,
    },
    Update {
        table: Expression,
        new_values: Vec<Expression>,
        r#where: Option<Expression>,
    },
    Delete {
        table: Expression,
        r#where: Option<Expression>,
    },
    Drop {
        table_or_database: Expression,
        is_database: bool,
    },
    Insert {
        table: Expression,
        column_order: Vec<Expression>,
        values: Vec<Expression>,
    },
    CreateTable {
        table_name: Expression,
        column_list: Vec<TableColumn>,
    },
    CreateIndex {
        index_name: Expression,
        table_name: Expression,
        is_unique: bool,
        column_list: Vec<Expression>,
    },
    CreateDatabase {
        database_name: Expression
    },
    StartTransaction,
    CommitTransaction,
    RollbackTransaction,
    Explain {
        statement: Box<Statement>
    },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum DBType {
    Int,
    BigInt,
    Decimal(usize, usize),
    Varchar(usize),
    Bool,
}

#[derive(Debug, PartialEq)]
pub struct TableColumn {
    pub(crate) column_name: Expression,
    pub(crate) column_type: DBType,
    pub(crate) constraints: Vec<Constraint>,
}

#[derive(Debug, PartialEq)]
pub enum Constraint {
    NotNull,
    Unique,
    PrimaryKey,
    ForeignKey {
        referenced_table: Expression,
        referenced_column: Expression,
    },
    Check(Expression),
    Default(Expression),
}

#[derive(PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(PartialEq)]
pub enum UnaryOperator {
    Not,
    Plus,
    Minus,
    Asc,
    Desc,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Plus => write!(f, "+"),
            UnaryOperator::Desc => write!(f, "DESC"),
            UnaryOperator::Asc => write!(f, "ASC"),
            UnaryOperator::Not => write!(f, "NOT"),
        }
    }
}

impl Debug for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanOrEqual => write!(f, ">="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanOrEqual => write!(f, "<="),
            BinaryOperator::Equal => write!(f, "="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::And => write!(f, "AND"),
            BinaryOperator::Or => write!(f, "OR"),
        }
    }
}

impl Debug for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<Token> for BinaryOperator {
    fn from(value: Token) -> Self {
        match value {
            Token::GreaterThan => BinaryOperator::GreaterThan,
            Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            Token::LessThan => BinaryOperator::LessThan,
            Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            Token::Equal => BinaryOperator::Equal,
            Token::NotEqual => BinaryOperator::NotEqual,
            Token::Star => BinaryOperator::Multiply,
            Token::Divide => BinaryOperator::Divide,
            Token::Minus => BinaryOperator::Minus,
            Token::Plus => BinaryOperator::Plus,
            Token::Keyword(Keyword::And) => BinaryOperator::And,
            Token::Keyword(Keyword::Or) => BinaryOperator::Or,
            _ => unreachable!()
        }
    }
}

impl From<Token> for UnaryOperator {
    fn from(value: Token) -> Self {
        match value {
            Token::Minus => UnaryOperator::Minus,
            Token::Plus => UnaryOperator::Plus,
            Token::Keyword(Keyword::Not) => UnaryOperator::Not,
            Token::Keyword(Keyword::Asc) => UnaryOperator::Asc,
            Token::Keyword(Keyword::Desc) => UnaryOperator::Desc,
            _ => unreachable!()
        }
    }
}

#[derive(PartialEq)]
pub enum Expression {
    BinaryOperation {
        left_operand: Box<Expression>,
        operator: BinaryOperator,
        right_operand: Box<Expression>,
    },
    UnaryOperation {
        operand: Box<Expression>,
        operator: UnaryOperator,
    },
    Number(u64),
    Float(f64),
    Bool(bool),
    Identifier(String),
    String(String),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::BinaryOperation { left_operand, operator, right_operand } => {
                write!(f, "({:?} {:?} {:?})", left_operand, operator, right_operand)
            }
            Expression::UnaryOperation { operand, operator } => {
                write!(f, "({:?} {:?})", operator, operand)
            }
            Expression::Number(num) => write!(f, "{num}"),
            Expression::Float(num) => write!(f, "{num}"),
            Expression::Identifier(iden) => write!(f, "{}", iden),
            Expression::String(str) => write!(f, "\"{}\"", str),
            Expression::Bool(b) => write!(f, "{}", b)
        }
    }
}
