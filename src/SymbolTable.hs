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
data SymbolScalarity = Scalar | Array
    deriving (Eq, Show)

type SymbolSize = Int

data SymbolInfo = VarInfo {
    infoType :: Type,
    infoScalarity :: SymbolScalarity,
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

unsafeSymbolScalarity :: String -> SymbolTable -> SymbolScalarity
unsafeSymbolScalarity = infoScalarity ... unsafeGetSymbolInfo

unsafeSymbolIsScalarity :: String -> SymbolScalarity -> SymbolTable -> Bool
unsafeSymbolIsScalarity s k st = (unsafeSymbolScalarity s st) == k

unsafeSymbolIsType :: String -> Type -> SymbolTable -> Bool
unsafeSymbolIsType s t st = (unsafeSymbolType s st) == t

nameScalarity :: Name -> SymbolScalarity
nameScalarity (Name _) = Scalar
nameScalarity (NameSubscription _ _) = Array

-- Errors data types
scalarityError :: SymbolScalarity -> ErrorType
scalarityError Scalar = NotAScalarError
scalarityError Array = NotAnArrayError

-- Symbol Table
type SymbolTable = TreePos Full Symbols

emptyST :: Tree Symbols
emptyST = Node M.empty []

root :: SymbolTable -> SymbolTable
root = Z.root

zipper :: Tree Symbols -> SymbolTable
zipper = Z.fromTree

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
nameToScalarity :: Name -> SymbolScalarity
nameToScalarity (Name _) = Scalar
nameToScalarity (NameSubscription _ _) = Array

nameToSize :: Name -> Either SemanticError SymbolSize
nameToSize (Name _) = Right 1
nameToSize (NameSubscription _ (Int i)) = Right i
nameToSize (NameSubscription _ (Char c)) = Right $ ord c
nameToSize x = Left (SemanticError NotConstantError (show x))

declName :: Declaration -> String
declName (VarDeclaration _ n _) = nameToString n
declName (FuncDeclaration _ n _ _) = nameToString n

paramToDecl :: Parameter -> Declaration
paramToDecl (Parameter t n) = VarDeclaration t n Nothing

-- Parameter data
paramToInfo :: Parameter -> Either SemanticError SymbolInfo
paramToInfo (Parameter t n) = do
    size <- nameToSize n
    Right $ VarInfo t (nameToScalarity n) size

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
                Right $ VarInfo t (nameToScalarity n) size
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

constructBlock :: Statement -> Either SemanticError (Tree Symbols)
constructBlock (Block ds stmts) = 
    let isBlock (Block _ _) = True
        isBlock (If _ _) = True
        isBlock (IfElse _ _ _) = True
        isBlock (While _ _) = True
        isBlock _ = False
    in
    do
    st <- constructSubST ds
    subst <- mapM constructBlock (filter isBlock stmts)
    return $ addChildren subst st
constructBlock (If _ stmt) = constructBlock stmt
constructBlock (IfElse _ stmt1 stmt2) = do
    st1 <- constructBlock stmt1
    st2 <- constructBlock stmt2
    return $ addChildren [st1, st2] emptyST
constructBlock (While _ stmt) = constructBlock stmt
constructBlock _ = return emptyST

-- SymbolTable construction
constructSubST :: [Declaration] -> Either SemanticError (Tree Symbols)
constructSubST ds = 
    let isFuncDecl (FuncDeclaration _ _ _ _) = True
        isFuncDecl _ = False
        subdecl (FuncDeclaration _ _ ps stmt) = do
            st <- constructBlock stmt
            foldM (flip addSymbol) st (map paramToDecl ps)
    in
    do
    st <- foldM (flip declare) emptyST ds
    subds <- ((mapM subdecl) . (filter isFuncDecl)) ds
    Right (addChildren subds st)

constructST :: [Declaration] -> Either SemanticError SymbolTable
constructST ds = constructSubST ds >>= (return . fromTree)