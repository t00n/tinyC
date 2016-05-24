module SymbolTable where

import qualified Data.Tree as T
import qualified Data.Map as M
import Data.Char(ord)

import SemanticError
import Parser

addChild :: a -> T.Tree a -> T.Tree a
addChild e tree = 
    let child = T.Node e []
    in T.Node (T.rootLabel tree) (child:T.subForest tree)

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

constructST :: [Declaration] -> Either SemanticError (T.Tree Symbols)
constructST ds = 
    let 
        name (Name s) = s
        name (NameSubscription s _) = s
        scalarity (Name _) = Scalar
        scalarity (NameSubscription _ _) = Array
        size (Name _) = 1
        size (NameSubscription _ (Int i)) = i
        size (NameSubscription _ (Char c)) = ord c
        paramToInfo (Parameter t n) = VarInfo t (scalarity n) (size n)
        declToInfo (VarDeclaration t n _) = (name n, VarInfo t (scalarity n) (size n))
        declToInfo (FuncDeclaration t n ps _) = (name n, FuncInfo t (map paramToInfo ps))
        symbols = M.fromList (map declToInfo ds)
    in Right (T.Node symbols [])