{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module NASMProgram where

import Numeric (showHex)
import Data.String.Builder
import Text.Printf
import Data.Char (toLower)
import Data.List

import TACProgram

data NASMProgram = NASMProgram [NASMData] [NASMInstruction]
    deriving (Eq, Show)

data NASMData = NASMData Label DataSize [Int]
    deriving (Eq, Show)

data DataSize = DB | DW | DD
    deriving (Eq, Show)

data RegisterSize = LSB | MSB | WORD | DWORD
    deriving (Eq, Show)

data RegisterName = A | B | C | D | SI | DI | SP | BP
    deriving (Eq, Show, Ord)

registers :: [RegisterName]
registers = [A, C, D, SI, DI, B]

data Register = Register RegisterName RegisterSize
    deriving (Eq, Show)

data AddressSize = BYTEADDRESS | WORDADDRESS | DWORDADDRESS
    deriving (Eq, Show)

data Address = AddressRegisterOffset Register Offset Multiplier
             | AddressRegisterRegister Register Offset Register Multiplier
             | AddressLabelOffset Label Offset Multiplier
             | AddressLabelRegister Label Register Multiplier
    deriving (Eq, Show)

type Constant = Int

type Multiplier = Int

type Offset = Int

data NASMInstruction = LABEL Label
                     | MOV1 RegisterSize RegisterName RegisterName
                     | MOV2 Register Address
                     | MOV3 Address Register
                     | MOV4 Register Constant
                     | MOV5 AddressSize Address Constant
                     | XCHG1 RegisterName -- Exchange register with eax
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
                     | NEG1 Register
                     | NEG2 Address
                     | IMUL1 Register -- Multiplied by A (depending on size) and stored in A
                     | IMUL2 AddressSize Address
                     | IMUL3 RegisterName Constant -- Only 32-bit register
                     | IMUL4 RegisterName RegisterName -- Only 32-bit register
                     | IMUL5 RegisterName Address -- Only 32-bit register
                     | IMUL6 RegisterName RegisterName Constant -- Only 32-bit register
                     | IMUL7 RegisterName Address Constant -- Only 32-bit register
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
                     | SHL3 Register -- Shift by the value in CL
                     | SHL4 Address -- Shift by the value in CL
                     | SHR1 Register Constant -- Constant must be 8-bit value
                     | SHR2 Address Constant -- Constant must be 8-bit value
                     | SHR3 Register -- Shift by the value in CL
                     | SHR4 Address -- Shift by the value in CL
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
                     | INTERRUPT Constant
    deriving (Eq, Show)

class NASMShow a where
    nasmShow :: a -> String

instance {-# OVERLAPPABLE #-} NASMShow a => NASMShow [a] where 
    nasmShow = concatMap (\x -> nasmShow x ++ "\n")

instance NASMShow String where
    nasmShow = (tail . init . show)

instance NASMShow NASMProgram where
    nasmShow (NASMProgram ds is) = build $ do
        "global tiny"
        "section .data"
        "\talign 2"
        literal $ nasmShow ds
        "section .bss"
        "section .text"
        literal $ nasmShow is
        "_exit:"
        "mov eax, 1"
        "mov ebx, 0"
        "int 0x80"
        "_writechar:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "mov edx,1     ; arg3, length of string to print"
        "lea ecx,[ebp+8]     ; arg2, pointer to string"
        "mov ebx,1       ; arg1, where to write, screen"
        "mov eax,4       ; write sysout command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"
        "_writeint:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "mov eax, [ebp+8] ; arg 1 : the int"
        "mov ecx, 0      ; byte counter"
        "mov ebx, 10      ; base 10"
        "; check if positive or negative"
        "test eax, eax"
        "jns _writeint_beg"
        "neg eax"
        "_writeint_beg:"
        "xor edx, edx"
        "idiv ebx"
        "add edx, 48"
        "sub esp, 1"
        "mov [esp], dl"
        "add ecx, 1"
        "cmp eax, 0"
        "jne _writeint_beg"
        "; add - if negative"
        "mov eax, [ebp+8]"
        "test eax, eax"
        "jns _write_not_signed"
        "mov bl, 45"
        "sub esp, 1"
        "mov [esp], bl"
        "add ecx, 1"
        "_write_not_signed:"
        "mov esi, ecx"
        "mov edx,ecx     ; arg3, length of string to print"
        "mov ecx,esp     ; arg2, pointer to string"
        "mov ebx,1       ; arg1, where to write, screen"
        "mov eax,4       ; write sysout command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "add esp, esi"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"
        "_read_char:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "sub esp, 1"
        "mov edx,1     ; arg3, length of string to read"
        "lea ecx,[esp]     ; arg2, pointer to string"
        "mov ebx,0       ; arg1, where to read, keyboard"
        "mov eax,3      ; write sysin command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "movzx eax, byte [esp]"
        "add esp, 1"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"
        "_read_int:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "sub esp, 100"
        "mov edx,100     ; arg3, length of string to read"
        "mov ecx,esp     ; arg2, pointer to string"
        "mov ebx,0       ; arg1, where to read, keyboard"
        "mov eax,3      ; write sysin command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "sub eax, 1"
        "mov ecx, 0" -- counter
        "mov ebx, 0" -- result
        "cmp byte [esp], 45" -- if first char is - ignore it for the moment
        "jne _read_int_beg"
        "add ecx, 1"
        "_read_int_beg:" -- start of loop
        "cmp ecx, eax" -- end if counter is higher than total number of chars
        "jge _read_int_end"
        "imul ebx, 10"
        "movzx esi, byte [esp+ecx]"
        "sub esi, 48"
        "add ebx, esi"
        "add ecx, 1"
        "jmp _read_int_beg"
        "_read_int_end:"
        "cmp byte [esp], 45"
        "jne _read_int_not_signed"
        "neg ebx"
        "_read_int_not_signed:"
        "mov eax, ebx"
        "add esp, 100"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"

 
instance NASMShow NASMData where
    nasmShow (NASMData l size vs) = printf "\t%s: %s\t" l (nasmShow size) ++ intercalate "," [printf "%d" v | v <- vs]

instShow :: String -> [String] -> String
instShow inst ops = inst ++ " " ++ intercalate ", " ops

instance NASMShow NASMInstruction where
    nasmShow (LABEL l) = printf "%s:" l
    nasmShow (MOV1 size r1 r2) = instShow "mov" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (MOV2 reg addr) = instShow "mov" [nasmShow reg, nasmShow addr]
    nasmShow (MOV3 addr reg) = instShow "mov" [nasmShow addr, nasmShow reg]
    nasmShow (MOV4 reg cst) = instShow "mov" [nasmShow reg, nasmShow cst]
    nasmShow (MOV5 size addr cst) = "mov " ++ nasmShow size ++ " " ++ nasmShow addr ++ "," ++ nasmShow cst
    nasmShow (XCHG1 reg) = instShow "xchg" [nasmShow reg, nasmShow (Register A DWORD)]
    nasmShow (PUSH1 rname ) = instShow "push" [nasmShow (Register rname DWORD)]
    nasmShow (PUSH2 addr) = instShow "push" [nasmShow addr]
    nasmShow (PUSH3 cst) = instShow "push" [nasmShow cst]
    nasmShow (POP1 rname ) = instShow "pop" [nasmShow (Register rname DWORD)]
    nasmShow (POP2 addr) = instShow "pop" [nasmShow addr]
    nasmShow (LEA rname addr ) = instShow "lea" [nasmShow (Register rname DWORD), nasmShow addr]
    nasmShow (ADD1 size r1 r2) = instShow "add" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (ADD2 reg addr) = instShow "add" [nasmShow reg, nasmShow addr]
    nasmShow (ADD3 addr reg) = instShow "add" [nasmShow addr, nasmShow reg]
    nasmShow (ADD4 reg cst) = instShow "add" [nasmShow reg, nasmShow cst]
    nasmShow (ADD5 addr cst) = instShow "add" [nasmShow addr, nasmShow cst]
    nasmShow (SUB1 size r1 r2) = instShow "sub" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (SUB2 reg addr) = instShow "sub" [nasmShow reg, nasmShow addr]
    nasmShow (SUB3 addr reg) = instShow "sub" [nasmShow addr, nasmShow reg]
    nasmShow (SUB4 reg cst) = instShow "sub" [nasmShow reg, nasmShow cst]
    nasmShow (SUB5 addr cst) = instShow "sub" [nasmShow addr, nasmShow cst]
    nasmShow (INC1 reg) = instShow "inc" [nasmShow reg]
    nasmShow (INC2 addr) = instShow "inc" [nasmShow addr]
    nasmShow (DEC1 reg) = instShow "dec" [nasmShow reg]
    nasmShow (DEC2 addr) = instShow "dec" [nasmShow addr]
    nasmShow (NEG1 reg) = instShow "neg" [nasmShow reg]
    nasmShow (NEG2 addr) = instShow "neg" [nasmShow addr]
    nasmShow (IMUL1 reg) = instShow "imul" [nasmShow reg]
    nasmShow (IMUL2 size addr) = "imul " ++ nasmShow size ++ " " ++ nasmShow addr
    nasmShow (IMUL3 r1 cst) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow cst]
    nasmShow (IMUL4 r1 r2) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow (Register r2 DWORD)]
    nasmShow (IMUL5 rname addr) = instShow "imul" [nasmShow (Register rname DWORD), nasmShow addr]
    nasmShow (IMUL6 r1 r2 cst) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow (Register r2 DWORD), nasmShow cst]
    nasmShow (IMUL7 rname addr cst) = instShow "imul" [nasmShow (Register rname DWORD), nasmShow addr, nasmShow cst]
    nasmShow (IDIV1 rname ) = instShow "idiv" [nasmShow (Register rname DWORD)]
    nasmShow (IDIV2 addr) = instShow "idiv" [nasmShow addr]
    nasmShow (AND1 size r1 r2) = instShow "and" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (AND2 reg addr) = instShow "and" [nasmShow reg, nasmShow addr]
    nasmShow (AND3 addr reg) = instShow "and" [nasmShow addr, nasmShow reg]
    nasmShow (AND4 reg cst) = instShow "and" [nasmShow reg, nasmShow cst]
    nasmShow (AND5 addr cst) = instShow "and" [nasmShow addr, nasmShow cst]
    nasmShow (OR1 size r1 r2) = instShow "or" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (OR2 reg addr) = instShow "or" [nasmShow reg, nasmShow addr]
    nasmShow (OR3 addr reg) = instShow "or" [nasmShow addr, nasmShow reg]
    nasmShow (OR4 reg cst) = instShow "or" [nasmShow reg, nasmShow cst]
    nasmShow (OR5 addr cst) = instShow "or" [nasmShow addr, nasmShow cst]
    nasmShow (XOR1 size r1 r2) = instShow "xor" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (XOR2 reg addr) = instShow "xor" [nasmShow reg, nasmShow addr]
    nasmShow (XOR3 addr reg) = instShow "xor" [nasmShow addr, nasmShow reg]
    nasmShow (XOR4 reg cst) = instShow "xor" [nasmShow reg, nasmShow cst]
    nasmShow (XOR5 addr cst) = instShow "xor" [nasmShow addr, nasmShow cst]
    nasmShow (NOT1 reg) = instShow "not" [nasmShow reg]
    nasmShow (NOT2 addr) = instShow "not" [nasmShow addr]
    nasmShow (SHL1 reg cst) = instShow "shl" [nasmShow reg, nasmShow cst]
    nasmShow (SHL2 addr cst) = instShow "shl" [nasmShow addr, nasmShow cst]
    nasmShow (SHL3 reg) = instShow "shl" [nasmShow reg, "cl"]
    nasmShow (SHL4 addr) = instShow "shl" [nasmShow addr, "cl"]
    nasmShow (SHR1 reg cst) = instShow "shr" [nasmShow reg, nasmShow cst]
    nasmShow (SHR2 addr cst) = instShow "shr" [nasmShow addr, nasmShow cst]
    nasmShow (SHR3 reg) = instShow "shr" [nasmShow reg, "cl"]
    nasmShow (SHR4 addr) = instShow "shr" [nasmShow addr, "cl"]
    nasmShow (JMP label) = instShow "jmp" [nasmShow label]
    nasmShow (JE label) = instShow "je" [nasmShow label]
    nasmShow (JNE label) = instShow "jne" [nasmShow label]
    nasmShow (JZ label) = instShow "jz" [nasmShow label]
    nasmShow (JG label) = instShow "jg" [nasmShow label]
    nasmShow (JGE label) = instShow "jge" [nasmShow label]
    nasmShow (JL label) = instShow "jl" [nasmShow label]
    nasmShow (JLE label) = instShow "jle" [nasmShow label]
    nasmShow (CMP1 r1 r2) = instShow "cmp" [nasmShow r1, nasmShow r2]
    nasmShow (CMP2 reg addr) = instShow "cmp" [nasmShow reg, nasmShow addr]
    nasmShow (CMP3 addr reg) = instShow "cmp" [nasmShow addr, nasmShow reg]
    nasmShow (CMP4 reg cst) = instShow "cmp" [nasmShow reg, nasmShow cst]
    nasmShow (CALL l) = printf "call %s" l
    nasmShow (RET) = "ret"
    nasmShow (INTERRUPT cst) = instShow "int" [showHex cst ""]

instance NASMShow AddressSize where
    nasmShow BYTEADDRESS = "byte"
    nasmShow WORDADDRESS = "word"
    nasmShow DWORDADDRESS = "dword"

showOffsetMult :: Offset -> Multiplier -> String
showOffsetMult offset mult = if offset /= 0 && mult /= 0 then 
                                 "+" ++ if mult /= 1 then nasmShow offset ++ "*" ++ nasmShow mult
                                        else nasmShow offset
                             else ""

showOffsetRegisterMult :: Offset -> Register -> Multiplier -> String
showOffsetRegisterMult offset reg mult = (if offset /= 0 then "+" ++ nasmShow offset else "")
                                      ++ (if mult /= 0 then "+" ++ nasmShow reg ++ 
                                              (if mult /= 1 then "*" ++ show mult else "")
                                          else "")

instance NASMShow Address where
    nasmShow (AddressRegisterOffset reg offset mult) = "[" ++ nasmShow reg ++ showOffsetMult offset mult ++ "]"
    nasmShow (AddressRegisterRegister r1 offset r2 mult) = "[" ++ nasmShow r1 ++ showOffsetRegisterMult offset r2 mult ++ "]"
    nasmShow (AddressLabelOffset label offset mult) = "[" ++ nasmShow label ++ showOffsetMult offset mult ++ "]"
    nasmShow (AddressLabelRegister label register mult) = "[" ++ nasmShow label ++ showOffsetRegisterMult 0 register mult ++ "]"

instance NASMShow Multiplier where
    nasmShow = show

instance NASMShow DataSize where
    nasmShow = (map toLower) . show

instance NASMShow RegisterName where
    nasmShow = (map toLower) . show

instance NASMShow Register where
    nasmShow (Register name size) = 
        let (prev, next) = case size of
                                DWORD -> ("e", "x")
                                WORD -> ("", "x")
                                MSB -> ("", "h")
                                LSB -> ("", "l")
        in  if name `elem` [SI, DI, BP, SP]
                then prev ++ nasmShow name
            else prev ++ nasmShow name ++ next
