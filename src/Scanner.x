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
    int             { \p s -> INT }
    if              { \p s -> IF }
    else            { \p s -> ELSE }
    !=              { \p s -> NEQUAL }
    return          { \p s -> RETURN }
    \(              { \p s -> LPAR }
    \)              { \p s -> RPAR }
    \{              { \p s -> LBRACE }
    \}              { \p s -> RBRACE }
    \[              { \p s -> LBRACK }
    \]              { \p s -> RBRACK }
    =               { \p s -> ASSIGN }
    \;              { \p s -> SEMICOLON }
    \,              { \p s -> COMMA }
    \+              { \p s -> PLUS }
    \-              { \p s -> MINUS }
    \*              { \p s -> TIMES }
    \/              { \p s -> DIVIDE }
    ==              { \p s -> EQUAL }
    char            { \p s -> CHAR }
    write           { \p s -> WRITE }
    read            { \p s -> READ }
    >               { \p s -> GREATER }
    \<              { \p s -> LESS }
    !               { \p s -> NOT }
    length          { \p s -> LENGTH }
    while           { \p s -> WHILE }
    $digit+         { \p s -> NUMBER (read s) }
    $alpha[$word]*  { \p s -> NAME s }
    \'.\'           { \p s -> QCHAR (s !! 1) }
    \".*"           { \p s -> QSTRING $ take ((length s) - 2) (drop 1 s) }

{
data Token = INT | IF | ELSE | NEQUAL
           | RETURN | LPAR | RPAR | LBRACE
           | RBRACE | LBRACK | RBRACK | ASSIGN
           | SEMICOLON | COMMA | PLUS | MINUS
           | TIMES | DIVIDE | EQUAL | CHAR
           | WRITE | READ | GREATER | LESS
           | NOT | LENGTH | WHILE
           | NAME String | NUMBER Int | QCHAR Char | QSTRING String
           deriving (Eq, Show)
}