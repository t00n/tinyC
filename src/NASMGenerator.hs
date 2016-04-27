module NASMGenerator (gasGenerate, NASMInstruction(..)) where

import TACGenerator

class NASMGenerator a where
    gasGenerate :: a -> NASMInstruction

data NASMInstruction = MOV1 RegisterSize RegisterName RegisterName
                     | MOV2 Register Address
                     | MOV3 Address Register
                     | MOV4 Register Int
                     | MOV5 AddressSize Address Constant
                     | PUSH1 RegisterName
                     | PUSH2 Address
                     | PUSH3 Constant
                     | POP1 RegisterName
                     | POP2 Address
                     | LEA RegisterName Address
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
                     | IMUL1 RegisterName RegisterName
                     | IMUL2 RegisterName Address
                     | IMUL3 RegisterName RegisterName Constant
                     | IMUL4 RegisterName Address Constant
                     | IDIV1 RegisterName
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

data RegisterSize = LSB | MSB | WORD | DWORD

data RegisterName = A | B | C | D | SI | DI | SP | BP

data Register = Register RegisterName RegisterSize

type Constant = Int

type Multiplier = Int

type Offset = Int

data AddressSize = BYTEADDRESS | WORDADDRESS | DWORDADDRESS

data Address = Address Register Multiplier Offset
             | AddressBase Register Register Multiplier Offset