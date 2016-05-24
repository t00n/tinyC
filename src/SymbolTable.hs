module SymbolTable where

import qualified Data.Tree as T
import qualified Data.Map as M
import Data.Char(ord)
import Control.Monad (foldM)

import SemanticError
import Parser

addChild :: a -> T.Tree a -> T.Tree a
addChild e tree = 
    let child = T.Node e []
    in T.Node (T.rootLabel tree) (child:T.subForest tree)

emptyST :: SymbolTable
emptyST = T.Node (M.empty) []

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

type SymbolTable = T.Tree Symbols

symbols :: SymbolTable -> Symbols
symbols = T.rootLabel

children :: SymbolTable -> [SymbolTable]
children = T.subForest

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

paramToInfo :: Parameter -> SymbolInfo
paramToInfo (Parameter t n) = VarInfo t (nameToScalarity n) (nameToSize n)

paramToSymbol :: Parameter -> (String, SymbolInfo)
paramToSymbol p@(Parameter t n) = (nameToString n, paramToInfo p)

declare :: Declaration -> Symbols -> Either SemanticError Symbols
declare decl st = 
    let declToInfo (VarDeclaration t n _) = VarInfo t (nameToScalarity n) (nameToSize n)
        declToInfo (FuncDeclaration t n ps _) = FuncInfo t (map paramToInfo ps)
        declName (VarDeclaration _ n _) = nameToString n
        declName (FuncDeclaration _ n _ _) = nameToString n
    in
    do 
        if M.member (declName decl) st
            then Left $ SemanticError NameExistsError (declName decl)
        else
            Right $ M.insert (declName decl) (declToInfo decl) st

constructSubST :: [Declaration] -> SymbolTable -> Either SemanticError SymbolTable
constructSubST ds st = 
    let funcdecl (FuncDeclaration _ _ _ _) = True
        funcdecl _ = False
        subdecl (FuncDeclaration _ _ ps (Block subds _)) = constructSubST subds (T.Node (M.fromList $ map paramToSymbol ps) [])
        ss = symbols st
    in
    do
    symbols <- foldM (flip declare) ss ds
    subds <- ((mapM subdecl) . (filter funcdecl)) ds
    Right (T.Node symbols subds)

constructST :: [Declaration] -> Either SemanticError SymbolTable
constructST = flip constructSubST (T.Node M.empty [])