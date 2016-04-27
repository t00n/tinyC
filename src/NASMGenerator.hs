module NASMGenerator (gasGenerate, NASMInstruction(..)) where

import TACGenerator

class NASMGenerator a where
    gasGenerate :: a -> NASMInstruction

data NASMInstruction = MOV1 RegisterSize RegisterName RegisterName
                     | MOV2 Register Address
                     | MOV3 Address Register
                     | MOV4 Register Int
                     | MOV5 AddressSize Address Int

data RegisterSize = LSB | MSB | WORD | DWORD

data RegisterName = A | B | C | D | SI | DI | SP | BP

data Register = Register RegisterName RegisterSize

type Multiplier = Int

type Offset = Int

data AddressSize = BYTEADDRESS | WORDADDRESS | DWORDADDRESS

data Address = Address Register Multiplier Offset
             | AddressBase Register Register Multiplier Offset