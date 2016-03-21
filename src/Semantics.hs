module Semantics (checkSemantics) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map

import Parser (Program, Declaration(..), Parameter(..), Statement(..), Expr(..), Type(..), BinaryOperator(..), UnaryOperator(..))

data Info = Info {
    infoType :: Type,
    infoArray :: Bool
} deriving (Eq, Show)

type Symbols = Map.Map String Info

data SymbolTable = SymbolTable {
    symbols :: Symbols,
    parent :: Maybe SymbolTable
} deriving (Eq, Show)

emptySymbolTable :: Maybe SymbolTable -> SymbolTable
emptySymbolTable parent = SymbolTable Map.empty parent

insertSymbol :: String -> Info -> SymbolTable -> SymbolTable
insertSymbol s i st = SymbolTable (Map.insert s i (symbols st)) (parent st)

variableName :: Expr -> String
variableName e = case e of Variable s -> s
                           Array s _ -> s
                           _ -> error "Not a variable declaration"

variableArray :: Expr -> Bool
variableArray e = case e of Array _ _ -> True
                            Variable _ -> False
                            _ -> error "Not a variable declaration"

variableInScope :: String -> SymbolTable -> Bool
variableInScope n st = if Map.member n (symbols st) then True else variableInParent n st
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = variableInScope n p

walkProgram :: Program -> SymbolTable -> Either String SymbolTable
walkProgram [] symbolTable = Right symbolTable
walkProgram (x:xs) symbolTable = do
    case x of
        VarDeclaration t e _ -> walkProgram xs 
                                    (insertSymbol (variableName e) (Info t (variableArray e)) symbolTable)
        FuncDeclaration t e params stmt -> (walkProgram xs
                                    (insertSymbol (variableName e) (Info t False) symbolTable)) >>= checkStatement stmt 

checkStatement :: Statement -> SymbolTable -> Either String SymbolTable
checkStatement stmt st = 
    case stmt of
        Assignment e1 e2 -> if variableInScope (variableName e1) st then Right st else Left "lolol"
        Block _ [] -> Right st
        Block decl (x:xs) -> (walkProgram decl st) >>= checkStatement x


checkSemantics :: Program -> Either String SymbolTable
checkSemantics program = walkProgram program (emptySymbolTable Nothing)
