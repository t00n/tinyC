{
module Scanner (scan) where
import Tokens
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
$word = [$alpha$digit\_]

tokens :- 
    $white+         ; -- spaces etc
    \/\/.*          ; -- comments
    int             { tokenWrapper (\s -> INT) }
    &               { tokenWrapper (\s -> ADDR) }
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

scan = alexScanTokens

}