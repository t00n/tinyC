module NASMGenerator (nasmGenerate, nasmGenerateData, nasmGenerateText, NASMData(..), NASMInstruction(..), RegisterName(..), RegisterSize(..), Register(..), Address(..), AddressSize(..)) where

import Data.Set (Set, member, empty, insert, delete)
import Control.Monad.State
import Data.Char (ord)


import TACGenerator
import SymbolTable

type SRSS = StateT RegisterState (State SymbolTable)

nasmGenerate :: TACProgram -> SymbolTable -> ([NASMData], [NASMInstruction])
nasmGenerate p = (,) <$> nasmGenerateData p <*> nasmGenerateText p

nasmGenerateText :: TACProgram -> SymbolTable -> [NASMInstruction]
nasmGenerateText p = evalState (evalStateT (nasmCodeGenerate p) empty)

nasmGenerateData :: TACProgram -> SymbolTable -> [NASMData]
nasmGenerateData ds st = ((map decl) . (filter datadecl)) ds
    where datadecl (TACCopy _ _) = True
          datadecl (TACArrayDecl _ _) = True
          datadecl _ = False
          decl (TACCopy s (TACInt i)) = NASMData s DWORDADDRESS [i]
          decl (TACCopy s (TACChar c)) = NASMData s BYTEADDRESS [ord c]
          decl (TACArrayDecl s xs) = NASMData s DWORDADDRESS (map (\(TACInt i) -> i) xs)

class NASMCodeGenerator a where
    nasmCodeGenerate :: a -> SRSS [NASMInstruction]

instance NASMCodeGenerator a => NASMCodeGenerator [a] where
    nasmCodeGenerate [] = return []
    nasmCodeGenerate (x:xs) = do
        first <- nasmCodeGenerate x
        rest <- nasmCodeGenerate xs
        return $ first ++ rest

instance NASMCodeGenerator TACInstruction where
    nasmCodeGenerate inst = return $ []

type RegisterState = Set RegisterName

data NASMData = NASMData Label AddressSize [Int]
    deriving(Eq, Show)

data NASMInstruction = MOV1 RegisterSize RegisterName RegisterName
                     | MOV2 Register Address
                     | MOV3 Address Register
                     | MOV4 Register Int
                     | MOV5 AddressSize Address Constant
                     | PUSH1 RegisterName -- Only 32-bit register
                     | PUSH2 Address
                     | PUSH3 Constant -- 32-bit constant
                     | POP1 RegisterName -- Only 32-bit register
                     | POP2 Address
                     | LEA RegisterName Address -- Only 32-bit register
                     | ADD1 RegisterSize RegisterName RegisterName
                     | ADD2 Register Address
                     | ADD3 Address Register
                     | ADD4 Register Constant
                     | ADD5 Address Constant
                     | SUB1 RegisterSize RegisterName RegisterName
                     | SUB2 Register Address
                     | SUB3 Address Register
                     | SUB4 Register Constant
                     | SUB5 Address Constant
                     | INC1 Register
                     | INC2 Address
                     | DEC1 Register
                     | DEC2 Address
                     | IMUL1 RegisterName RegisterName -- Only 32-bit register
                     | IMUL2 RegisterName Address -- Only 32-bit register
                     | IMUL3 RegisterName RegisterName Constant -- Only 32-bit register
                     | IMUL4 RegisterName Address Constant -- Only 32-bit register
                     -- divide the contents of EDX:EAX by the contents of the parameter. Place the quotient in EAX and the remainder in EDX.
                     | IDIV1 RegisterName -- Only 32-bit register
                     | IDIV2 Address
                     | AND1 RegisterSize RegisterName RegisterName
                     | AND2 Register Address
                     | AND3 Address Register
                     | AND4 Register Constant
                     | AND5 Address Constant
                     | OR1 RegisterSize RegisterName RegisterName
                     | OR2 Register Address
                     | OR3 Address Register
                     | OR4 Register Constant
                     | OR5 Address Constant
                     | XOR1 RegisterSize RegisterName RegisterName
                     | XOR2 Register Address
                     | XOR3 Address Register
                     | XOR4 Register Constant
                     | XOR5 Address Constant
                     | NOT1 Register
                     | NOT2 Address
                     | SHL1 Register Constant -- Constant must be 8-bit value
                     | SHL2 Address Constant -- Constant must be 8-bit value
                     | SHL3 Register Register -- Second Register must be CL
                     | SHL4 Address Register -- Second Register must be CL
                     | SHR1 Register Constant -- Constant must be 8-bit value
                     | SHR2 Address Constant -- Constant must be 8-bit value
                     | SHR3 Register Register -- Second Register must be CL
                     | SHR4 Address Register -- Second Register must be CL
                     | JMP Label
                     | JE Label
                     | JNE Label
                     | JZ Label
                     | JG Label
                     | JGE Label
                     | JL Label
                     | JLE Label
                     | CMP1 Register Register
                     | CMP2 Register Address
                     | CMP3 Address Register
                     | CMP4 Register Constant
                     | CALL Label
                     | RET
    deriving (Eq, Show)

data RegisterSize = LSB | MSB | WORD | DWORD
    deriving (Eq, Show)

data RegisterName = A | B | C | D | SI | DI | SP | BP
    deriving (Eq, Show, Ord)

data Register = Register RegisterName RegisterSize
    deriving (Eq, Show)

data AddressSize = BYTEADDRESS | WORDADDRESS | DWORDADDRESS
    deriving (Eq, Show)

data Address = Address Register Multiplier Offset
             | AddressBase Register Register Multiplier Offset
             | AddressLabel String Offset
    deriving (Eq, Show)

type Constant = Int

type Multiplier = Int

type Offset = Int

type Label = String