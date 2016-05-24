module SymbolTable where

import qualified Data.Tree as T
import qualified Data.Map as M
import Data.Char(ord)
import Control.Monad (foldM)

import SemanticError
import Parser

-- Tree
addChild :: T.Tree a -> T.Tree a -> T.Tree a
addChild child tree = T.Node (T.rootLabel tree) (child:T.subForest tree)

addChildren :: [T.Tree a] -> T.Tree a -> T.Tree a
addChildren = flip (foldr addChild)

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
    infoParams :: [SymbolInfo]
} deriving (Eq, Show)

type Symbols = M.Map String SymbolInfo

-- Symbol Table
type SymbolTable = T.Tree Symbols

emptyST :: SymbolTable
emptyST = T.Node M.empty []

symbols :: SymbolTable -> Symbols
symbols = T.rootLabel

children :: SymbolTable -> [SymbolTable]
children = T.subForest

-- Declaration data
nameToString :: Name -> String
nameToString (Name s) = s
nameToString (NameSubscription s _) = s

nameToScalarity :: Name -> SymbolScalarity
nameToScalarity (Name _) = Scalar
nameToScalarity (NameSubscription _ _) = Array

nameToSize :: Name -> SymbolSize
nameToSize (Name _) = 1
nameToSize (NameSubscription _ (Int i)) = i
nameToSize (NameSubscription _ (Char c)) = ord c

declName :: Declaration -> String
declName (VarDeclaration _ n _) = nameToString n
declName (FuncDeclaration _ n _ _) = nameToString n

-- Parameter data
paramToInfo :: Parameter -> SymbolInfo
paramToInfo (Parameter t n) = VarInfo t (nameToScalarity n) (nameToSize n)

paramToSymbol :: Parameter -> (String, SymbolInfo)
paramToSymbol p@(Parameter t n) = (nameToString n, paramToInfo p)

-- Declarations
addSymbol :: Declaration -> SymbolTable -> SymbolTable
addSymbol d st = 
    let declInfo (VarDeclaration t n _) = VarInfo t (nameToScalarity n) (nameToSize n)
        declInfo (FuncDeclaration t n ps _) = FuncInfo t (map paramToInfo ps)
    in
    T.Node (M.insert (declName d) (declInfo d) (symbols st)) (children st)

declare :: Declaration -> SymbolTable -> Either SemanticError SymbolTable
declare decl st = do 
    if M.member (declName decl) (symbols st)
        then Left $ SemanticError NameExistsError (declName decl)
    else
        Right $ addSymbol decl st

-- SymbolTable construction
constructSubST :: [Declaration] -> SymbolTable -> Either SemanticError SymbolTable
constructSubST ds st = 
    let funcdecl (FuncDeclaration _ _ _ _) = True
        funcdecl _ = False
        paramsToST ps = T.Node (M.fromList (map paramToSymbol ps)) []
        subdecl (FuncDeclaration _ _ ps (Block subds _)) = constructSubST subds (paramsToST ps)
    in
    do
    st <- foldM (flip declare) st ds
    subds <- ((mapM subdecl) . (filter funcdecl)) ds
    Right (addChildren subds st)

constructST :: [Declaration] -> Either SemanticError SymbolTable
constructST = flip constructSubST emptyST