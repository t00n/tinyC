module Tokens where
	
data TokenWrapper = TokenWrapper Token (Int, Int, Int)
    deriving (Eq, Show)


data Token = INT | IF | ELSE | NEQUAL
           | RETURN | LPAR | RPAR | LBRACE
           | RBRACE | LBRACK | RBRACK | ASSIGN
           | SEMICOLON | COMMA | PLUS | MINUS
           | TIMES_OR_PTR | DIVIDE | EQUAL | CHAR
           | WRITE | READ | GREATER | LESS
           | NOT | LENGTH | WHILE
           | NAME String | NUMBER Int | QCHAR Char | QSTRING String | ADDR
           deriving (Eq)

instance Show Token where
    show INT = "int"
    show IF = "if"
    show ELSE = "else"
    show NEQUAL = "!="
    show RETURN = "return"
    show LPAR = "("
    show RPAR = ")"
    show LBRACE = "{"
    show RBRACE = "}"
    show LBRACK = "["
    show RBRACK = "]"
    show ASSIGN = "="
    show SEMICOLON = ";"
    show COMMA = ","
    show PLUS = "+"
    show MINUS = "-"
    show TIMES_OR_PTR = "*"
    show DIVIDE = "/"
    show EQUAL = "=="
    show CHAR = "char"
    show WRITE = "write"
    show READ = "read"
    show GREATER = ">"
    show LESS = "<"
    show NOT = "!"
    show LENGTH = "length"
    show WHILE = "while"
    show (NAME s) = s
    show (NUMBER i) = show i
    show (QCHAR c) = show c
    show (QSTRING s) = s
    show ADDR = "&"