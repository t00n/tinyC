module SymbolTable where

import Data.Tree
import Data.Tree.Zipper
import qualified Data.Map as M
import Data.Char(ord)
import Control.Monad (foldM)
import Data.Maybe (fromJust)

import SemanticError
import Parser

-- Tree
addChild :: Tree a -> Tree a -> Tree a
addChild child tree = Node (rootLabel tree) (child:subForest tree)

addChildren :: [Tree a] -> Tree a -> Tree a
addChildren = flip (foldr addChild)

nextDF :: Eq a => TreePos Full a -> Maybe (TreePos Full a)
nextDF treePos = 
    let left = firstChild treePos
        right p = 
            if next p /= Nothing 
                then next p
            else if parent p /= Nothing
                then right ((fromJust . parent) p)
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
    infoParams :: [SymbolInfo]
} deriving (Eq, Show)

type Symbols = M.Map String SymbolInfo

-- Symbol Table
type SymbolTable = Tree Symbols

emptyST :: SymbolTable
emptyST = Node M.empty []

symbols :: SymbolTable -> Symbols
symbols = rootLabel

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

paramToDecl :: Parameter -> Declaration
paramToDecl (Parameter t n) = VarDeclaration t n Nothing

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
    Node (M.insert (declName d) (declInfo d) (symbols st)) (subForest st)

declare :: Declaration -> SymbolTable -> Either SemanticError SymbolTable
declare decl st = do 
    if M.member (declName decl) (symbols st)
        then Left $ SemanticError NameExistsError (declName decl)
    else
        Right $ addSymbol decl st

-- SymbolTable construction
constructSubST :: [Declaration] -> Either SemanticError SymbolTable
constructSubST ds = 
    let isFuncDecl (FuncDeclaration _ _ _ _) = True
        isFuncDecl _ = False
        subdecl (FuncDeclaration _ _ ps (Block subds _)) = constructSubST (map paramToDecl ps ++ subds)
    in
    do
    st <- foldM (flip declare) emptyST ds
    subds <- ((mapM subdecl) . (filter isFuncDecl)) ds
    Right (addChildren subds st)

constructST :: [Declaration] -> Either SemanticError SymbolTable
constructST = constructSubST