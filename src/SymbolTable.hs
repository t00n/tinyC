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

declare :: Declaration -> Symbols -> Either SemanticError Symbols
declare decl st = 
    let name (Name s) = s
        name (NameSubscription s _) = s
        scalarity (Name _) = Scalar
        scalarity (NameSubscription _ _) = Array
        size (Name _) = 1
        size (NameSubscription _ (Int i)) = i
        size (NameSubscription _ (Char c)) = ord c
        paramToInfo (Parameter t n) = VarInfo t (scalarity n) (size n)
        declToInfo (VarDeclaration t n _) = VarInfo t (scalarity n) (size n)
        declToInfo (FuncDeclaration t n ps _) = FuncInfo t (map paramToInfo ps)
        declName (VarDeclaration _ n _) = name n
        declName (FuncDeclaration _ n _ _) = name n
    in
    do 
        if M.member (declName decl) st
            then Left $ SemanticError NameExistsError (declName decl)
        else
            Right $ M.insert (declName decl) (declToInfo decl) st

constructST :: [Declaration] -> Either SemanticError SymbolTable
constructST ds = do
    symbols <- foldM (flip declare) M.empty ds
    Right (T.Node symbols [])