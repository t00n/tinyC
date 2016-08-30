module SymbolTable where

import Data.Tree
import Data.Tree.Zipper as Z
import qualified Data.Map as M
import Data.Char(ord)
import Control.Monad (foldM)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (traceShow)

import Utility
import SemanticError
import Parser

-- Tree
addChild :: Tree a -> Tree a -> Tree a
addChild child tree = Node (rootLabel tree) (child:subForest tree)

addChildren :: [Tree a] -> Tree a -> Tree a
addChildren = flip (foldr addChild)

nextDF :: Eq a => TreePos Full a -> Maybe (TreePos Full a)
nextDF treePos = 
    let left = Z.firstChild treePos
        right p = 
            if Z.next p /= Nothing 
                then Z.next p
            else if Z.parent p /= Nothing
                then right ((fromJust . Z.parent) p)
            else Nothing
    in
    if left /= Nothing
        then left
    else right treePos

-- Symbols
data SymbolKind = Value | Pointer | Array
    deriving (Eq, Show)

data SymbolSize = VarSize Int
                | ArraySize [Int]
    deriving (Eq, Show)

data SymbolInfo = VarInfo {
    infoType :: Type,
    infoKind :: SymbolKind,
    infoSize :: SymbolSize
}         | FuncInfo {
    infoType :: Type,
    infoParams :: Symbols
} deriving (Eq, Show)

type Symbols = M.Map String SymbolInfo

getSymbolInfo :: String -> SymbolTable -> Maybe SymbolInfo
getSymbolInfo s st = let res = M.lookup s ((rootLabel . Z.tree) st) in 
                         if res == Nothing then Z.parent st >>= getSymbolInfo s
                         else res

unsafeGetSymbolInfo :: String -> SymbolTable -> SymbolInfo
unsafeGetSymbolInfo s st = let info = getSymbolInfo s st in
    fromMaybe (error ("Symbol " ++ s ++ " is not in symbol table " ++ show st)) info

unsafeSymbolType :: String -> SymbolTable -> Type
unsafeSymbolType = infoType ... unsafeGetSymbolInfo

unsafeSymbolKind :: String -> SymbolTable -> SymbolKind
unsafeSymbolKind = infoKind ... unsafeGetSymbolInfo

unsafeSymbolSize :: String -> SymbolTable -> SymbolSize
unsafeSymbolSize = infoSize ... unsafeGetSymbolInfo

-- Symbol Table
type SymbolTable = TreePos Full Symbols

emptyST :: Tree Symbols
emptyST = Node M.empty []

root :: SymbolTable -> SymbolTable
root = Z.root

zipper :: Tree Symbols -> SymbolTable
zipper = Z.fromTree

unzip :: SymbolTable -> Tree Symbols
unzip = Z.toTree

symbols :: SymbolTable -> Symbols
symbols = Z.label

memberST :: String -> SymbolTable -> Bool
memberST s stz = (M.member s . rootLabel . Z.tree) stz

nameInScope :: String -> SymbolTable -> Bool
nameInScope s = (||) <$> memberST s <*> nameInParent s

nameInParent :: String -> SymbolTable -> Bool
nameInParent s st = 
    let p = parent st
    in
    if p /= Nothing
        then
            if memberST s (fromJust p)
                then True
            else nameInParent s (fromJust p)
    else False

symbolIsFunction :: String -> SymbolTable -> Bool
symbolIsFunction s st = 
    let info = unsafeGetSymbolInfo s st
    in
    case info of
        FuncInfo _ _ -> True
        _ -> False

-- Declaration data
nameToKind :: Name -> SymbolKind
nameToKind (Name _) = Value
nameToKind (NameSubscription _ _) = Array
nameToKind (NamePointer _) = Pointer

expressionSize :: Expression -> Int
expressionSize (Int i) = i
expressionSize (Char c) = ord c

nameToSize :: Name -> Either SemanticError SymbolSize
nameToSize (Name _) = Right $ VarSize 1
nameToSize (NameSubscription _ sizes) = Right $ ArraySize $ map expressionSize sizes
nameToSize (NamePointer _) = Right $ VarSize 1
nameToSize x = Left (SemanticError NotAConstantError (show x))

declName :: Declaration -> String
declName (VarDeclaration _ n _) = nameToString n
declName (FuncDeclaration _ n _ _) = nameToString n

paramToDecl :: Parameter -> Declaration
paramToDecl (Parameter t n) = VarDeclaration t n Nothing

-- Parameter data
paramToInfo :: Parameter -> Either SemanticError SymbolInfo
paramToInfo (Parameter t n) = do
    size <- nameToSize n
    Right $ VarInfo t (nameToKind n) size

paramToSymbol :: Parameter -> Either SemanticError (String, SymbolInfo)
paramToSymbol p@(Parameter t n) = do
    info <- paramToInfo p
    Right (nameToString n, info)

-- Declarations
addSymbol :: Declaration -> Tree Symbols -> Either SemanticError (Tree Symbols)
addSymbol d st = 
    let declInfo (VarDeclaration t n _) = 
            do
                size <- nameToSize n
                Right $ VarInfo t (nameToKind n) size
        declInfo (FuncDeclaration t n ps _) = 
            do
                infos <- mapM paramToSymbol ps
                Right $ FuncInfo t (M.fromList infos)
    in
    do
    info <- declInfo d
    Right $ Node (M.insert (declName d) info (rootLabel st)) (subForest st)

declare :: Declaration -> Tree Symbols -> Either SemanticError (Tree Symbols)
declare decl st = do 
    if M.member (declName decl) (rootLabel st)
        then Left $ SemanticError NameExistsError (declName decl)
    else
        addSymbol decl st

constructBlock :: Statement -> Tree Symbols -> Either SemanticError (Tree Symbols)
constructBlock (Block ds stmts) st = 
    let isBlock (Block _ _) = True
        isBlock (If _ _) = True
        isBlock (IfElse _ _ _) = True
        isBlock (While _ _) = True
        isBlock _ = False
    in
    do
    constructDeclarations ds st >>= flip (foldM (flip constructBlock)) stmts
constructBlock (If _ stmt) st = constructBlock stmt st
constructBlock (IfElse _ stmt1 stmt2) st = constructBlock stmt1 st >>= constructBlock stmt2
constructBlock (While _ stmt) st = constructBlock stmt st
constructBlock _ st = return st

constructDeclarations :: [Declaration] -> Tree Symbols -> Either SemanticError (Tree Symbols)
constructDeclarations ds st = 
    let isFuncDecl (FuncDeclaration _ _ _ _) = True
        isFuncDecl _ = False
        subdecl (FuncDeclaration _ _ ps stmt) = do
            block <- constructBlock stmt emptyST
            foldM (flip addSymbol) block (map paramToDecl ps)
    in
    do
    decl <- foldM (flip declare) st ds
    subds <- ((mapM subdecl) . (filter isFuncDecl)) ds
    Right (addChildren subds decl)

-- SymbolTable construction
constructFunction :: [Declaration] -> Either SemanticError (Tree Symbols)
constructFunction ds = constructDeclarations ds emptyST

constructST :: [Declaration] -> Either SemanticError SymbolTable
constructST ds = constructFunction ds >>= (return . fromTree)