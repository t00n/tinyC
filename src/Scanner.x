{
    module Scanner where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
$word = [$alpha$digit\_]

tokens :- 
    $white+         ; -- spaces etc
    \/\/.*          ; -- comments
    int             { tokenWrapper (\s -> INT) }
    if              { tokenWrapper (\s -> IF) }
    else            { tokenWrapper (\s -> ELSE) }
    !=              { tokenWrapper (\s -> NEQUAL) }
    return          { tokenWrapper (\s -> RETURN) }
    \(              { tokenWrapper (\s -> LPAR) }
    \)              { tokenWrapper (\s -> RPAR) }
    \{              { tokenWrapper (\s -> LBRACE) }
    \}              { tokenWrapper (\s -> RBRACE) }
    \[              { tokenWrapper (\s -> LBRACK) }
    \]              { tokenWrapper (\s -> RBRACK) }
    =               { tokenWrapper (\s -> ASSIGN) }
    \;              { tokenWrapper (\s -> SEMICOLON) }
    \,              { tokenWrapper (\s -> COMMA) }
    \+              { tokenWrapper (\s -> PLUS) }
    \-              { tokenWrapper (\s -> MINUS) }
    \*              { tokenWrapper (\s -> TIMES_OR_PTR) }
    \/              { tokenWrapper (\s -> DIVIDE) }
    ==              { tokenWrapper (\s -> EQUAL) }
    char            { tokenWrapper (\s -> CHAR) }
    write           { tokenWrapper (\s -> WRITE) }
    read            { tokenWrapper (\s -> READ) }
    >               { tokenWrapper (\s -> GREATER) }
    \<              { tokenWrapper (\s -> LESS) }
    !               { tokenWrapper (\s -> NOT) }
    length          { tokenWrapper (\s -> LENGTH) }
    while           { tokenWrapper (\s -> WHILE) }
    $digit+         { tokenWrapper (\s -> NUMBER (read s)) }
    $alpha[$word]*  { tokenWrapper (\s -> NAME s) }
    \'.\'           { tokenWrapper (\s -> QCHAR (s !! 1)) }
    \".*"           { tokenWrapper (\s -> QSTRING $ (init . tail) s) }

{
tokenWrapper :: (String -> Token) -> (AlexPosn -> String -> TokenWrapper)
tokenWrapper f = \(AlexPn p1 p2 p3) s -> TokenWrapper (f s) (p1, p2, p3)

data TokenWrapper = TokenWrapper Token (Int, Int, Int)
    deriving (Eq, Show)


data Token = INT | IF | ELSE | NEQUAL
           | RETURN | LPAR | RPAR | LBRACE
           | RBRACE | LBRACK | RBRACK | ASSIGN
           | SEMICOLON | COMMA | PLUS | MINUS
           | TIMES_OR_PTR | DIVIDE | EQUAL | CHAR
           | WRITE | READ | GREATER | LESS
           | NOT | LENGTH | WHILE
           | NAME String | NUMBER Int | QCHAR Char | QSTRING String
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
}