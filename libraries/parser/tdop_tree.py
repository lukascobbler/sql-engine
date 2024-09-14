import re

token_pat = re.compile(r"\s*(?:(\d+)|(.))")
PREFIX_PRECEDENCE = 100

class BinaryOperation:
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

    def __str__(self):
        return f"({self.left} {self.op} {self.right})"

class UnaryOperation:
    def __init__(self, operand, op):
        self.operand = operand
        self.op = op

    def __str__(self):
        return f"({self.op} {self.operand})"

def tokenize(program):
    for number, operator_or_identifier in token_pat.findall(program):
        if number:
            yield NumberToken(number)
        elif operator_or_identifier == "+":
            yield AddToken()
        elif operator_or_identifier == "-":
            yield SubToken()
        elif operator_or_identifier == "*":
            yield MulToken()
        elif operator_or_identifier == "/":
            yield DivToken()
        elif operator_or_identifier == "^":
            yield PowToken()
        elif operator_or_identifier == '(':
            yield LeftParenthesesToken()
        elif operator_or_identifier == ')':
            yield RightParenthesesToken()
        elif operator_or_identifier == "&":
            yield AndToken()
        elif operator_or_identifier == "|":
            yield OrToken()
        elif operator_or_identifier == "!":
            yield NotToken()
        elif operator_or_identifier == ">":
            yield GreaterThanToken()
        elif operator_or_identifier == "<":
            yield LessThanToken()
        elif operator_or_identifier == "A":
            yield AscToken()
        elif operator_or_identifier == "D":
            yield DescToken()
        else:
            yield IdentifierToken(operator_or_identifier)
    yield EofToken()

def parse(program):
    global token, token_iterator
    token_iterator = tokenize(program)
    token = next(token_iterator)
    return build_expression()

def build_expression(right_binding_power=0):
    global token

    current_token = token
    token = next(token_iterator)
    left = current_token.prefix_expression()
    while right_binding_power < token.infix_precedence:
        current_token = token
        token = next(token_iterator)
        left = current_token.infix_expression(left)
    return left

def check_next_token(next_token=None):
    global token
    if next_token and next_token != type(token):
        raise SyntaxError('Expected %s' % next_token)
    # consume the token if it passed the check
    token = next(token_iterator)

class NumberToken:
    def __init__(self, value):
        self.value = int(value)
    def prefix_expression(self):
        return self.value

    def __str__(self):
        return str(self.value)

class IdentifierToken:
    def __init__(self, value):
        self.value = value
    def prefix_expression(self):
        return self.value

    def __str__(self):
        return str(self.value)

class AddToken:
    infix_precedence = 25
    def prefix_expression(self):
        return UnaryOperation(build_expression(PREFIX_PRECEDENCE), "+")

    def infix_expression(self, left):
        return BinaryOperation(left, "+", build_expression(self.infix_precedence))

class SubToken:
    infix_precedence = 25
    def prefix_expression(self):
        return UnaryOperation(build_expression(PREFIX_PRECEDENCE), "-")

    def infix_expression(self, left):
        return BinaryOperation(left, "-", build_expression(self.infix_precedence))

class MulToken:
    infix_precedence = 30
    def infix_expression(self, left):
        return BinaryOperation(left, "*", build_expression(self.infix_precedence))

class DivToken:
    infix_precedence = 30
    def infix_expression(self, left):
        return BinaryOperation(left, "/", build_expression(self.infix_precedence))

class PowToken:
    infix_precedence = 35
    def infix_expression(self, left):
        return BinaryOperation(left, "^", build_expression(self.infix_precedence - 1))

class AndToken:
    infix_precedence = 15
    def infix_expression(self, left):
        return BinaryOperation(left, "AND", build_expression(self.infix_precedence))

class AscToken:
    infix_precedence = 5
    def infix_expression(self, left):
        return UnaryOperation(left, "ASC")

class DescToken:
    infix_precedence = 5
    def infix_expression(self, left):
        return UnaryOperation(left, "DESC")

class OrToken:
    infix_precedence = 10
    def infix_expression(self, left):
        return BinaryOperation(left, "OR", build_expression(self.infix_precedence))

class NotToken:
    def prefix_expression(self):
        return UnaryOperation(build_expression(PREFIX_PRECEDENCE), "NOT")

class GreaterThanToken:
    infix_precedence = 20
    def infix_expression(self, left):
        return BinaryOperation(left, ">", build_expression(self.infix_precedence))

class LessThanToken:
    infix_precedence = 15
    def infix_expression(self, left):
        return BinaryOperation(left, "<", build_expression(self.infix_precedence))

class LeftParenthesesToken:
    infix_precedence = 0
    def prefix_expression(self):
        expr = build_expression()
        check_next_token(RightParenthesesToken)
        return expr

class RightParenthesesToken:
    infix_precedence = 0

class EofToken:
    infix_precedence = 0

if __name__ == "__main__":
    print(parse("(-i * 2) / 15^i^2 - ((44 * i) - 15 * 2)"))
    print(parse("!i & i | (i & i)"))
    print(parse("i > 15 * (44 - i / 7) | i < 0"))
    print(parse("i * 2 D"))
    
    """
    This is a simplified Python tokenizer and parser 
    used for easier learning of the PRATT parsing method.
    
    Adapted to Python3 and reformated for better code structure:
    https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
    
    This parser builds the operation tree instead of calculating the value. Added SQL statements.
    """