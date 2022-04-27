module Instructions

type t =
    | ADC
    | AND
    | ASL
    | BCC
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BRK
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CPX
    | CPY
    | CMP
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | JMP
    | JSR
    | LDA
    | LDX
    | LDY
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | ROR
    | ROL
    | RTI
    | RTS
    | SBC
    | SEC
    | SED
    | SEI
    | STA
    | STX
    | STY
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA
    // undocumented opcodes
    | LAX
    | SAX
    | DCP
    | ISB
    | SLO
    | RLA
    | SRE
    | RRA

module AddressingMode =
    type t =
        | Implicit
        | Absolute
        | AbsoluteX
        | AbsoluteY
        | ZeroPage
        | ZeroPageX
        | ZeroPageY
        | Accumulator
        | Relative
        | IndirectX
        | IndirectY
        | Indirect
        | Immediate

let decode opcode =
    match opcode with
    | 0x1A
    | 0x3A
    | 0x5A
    | 0x7A
    | 0xDA
    | 0xFA -> (NOP, AddressingMode.Implicit, 2, 0)
    | 0x80
    | 0x82
    | 0x89
    | 0xC2
    | 0xE2 -> (NOP, AddressingMode.Immediate, 2, 0)
    | 0x04
    | 0x44
    | 0x64 -> (NOP, AddressingMode.ZeroPage, 3, 0)
    | 0x0C -> (NOP, AddressingMode.Absolute, 4, 0)
    | 0x14
    | 0x34
    | 0x54
    | 0x74
    | 0xD4
    | 0xF4 -> (NOP, AddressingMode.ZeroPageX, 4, 0)
    | 0x1C
    | 0x3C
    | 0x5C
    | 0x7C
    | 0xDC
    | 0xFC -> (NOP, AddressingMode.AbsoluteX, 4, 1)
    | 0x00 -> (BRK, AddressingMode.Implicit, 7, 0)
    | 0x01 -> (ORA, AddressingMode.IndirectX, 6, 0)
    | 0x03 -> (SLO, AddressingMode.IndirectX, 8, 0)
    | 0x05 -> (ORA, AddressingMode.ZeroPage, 3, 0)
    | 0x06 -> (ASL, AddressingMode.ZeroPage, 5, 0)
    | 0x07 -> (SLO, AddressingMode.ZeroPage, 5, 0)
    | 0x08 -> (PHP, AddressingMode.Implicit, 3, 0)
    | 0x09 -> (ORA, AddressingMode.Immediate, 2, 0)
    | 0x0A -> (ASL, AddressingMode.Accumulator, 2, 0)
    | 0x0D -> (ORA, AddressingMode.Absolute, 4, 0)
    | 0x0E -> (ASL, AddressingMode.Absolute, 6, 0)
    | 0x0F -> (SLO, AddressingMode.Absolute, 6, 0)
    | 0x10 -> (BPL, AddressingMode.Relative, 2, 0)
    | 0x11 -> (ORA, AddressingMode.IndirectY, 5, 0)
    | 0x13 -> (SLO, AddressingMode.IndirectY, 8, 0)
    | 0x15 -> (ORA, AddressingMode.ZeroPageX, 4, 0)
    | 0x16 -> (ASL, AddressingMode.ZeroPageX, 6, 0)
    | 0x17 -> (SLO, AddressingMode.ZeroPageX, 6, 0)
    | 0x18 -> (CLC, AddressingMode.Implicit, 2, 0)
    | 0x19 -> (ORA, AddressingMode.AbsoluteY, 4, 0)
    | 0x1B -> (SLO, AddressingMode.AbsoluteY, 7, 0)
    | 0x1D -> (ORA, AddressingMode.AbsoluteX, 4, 0)
    | 0x1E -> (ASL, AddressingMode.AbsoluteX, 7, 0)
    | 0x1F -> (SLO, AddressingMode.AbsoluteX, 7, 0)
    | 0x20 -> (JSR, AddressingMode.Absolute, 6, 0)
    | 0x21 -> (AND, AddressingMode.IndirectX, 6, 0)
    | 0x23 -> (RLA, AddressingMode.IndirectX, 8, 0)
    | 0x24 -> (BIT, AddressingMode.ZeroPage, 3, 0)
    | 0x25 -> (AND, AddressingMode.ZeroPage, 3, 0)
    | 0x26 -> (ROL, AddressingMode.ZeroPage, 5, 0)
    | 0x27 -> (RLA, AddressingMode.ZeroPage, 5, 0)
    | 0x28 -> (PLP, AddressingMode.Implicit, 4, 0)
    | 0x29 -> (AND, AddressingMode.Immediate, 2, 0)
    | 0x2A -> (ROL, AddressingMode.Accumulator, 2, 0)
    | 0x2D -> (AND, AddressingMode.Absolute, 4, 0)
    | 0x2C -> (BIT, AddressingMode.Absolute, 4, 0)
    | 0x2E -> (ROL, AddressingMode.Absolute, 6, 0)
    | 0x2F -> (RLA, AddressingMode.Absolute, 6, 0)
    | 0x30 -> (BMI, AddressingMode.Relative, 2, 1)
    | 0x31 -> (AND, AddressingMode.IndirectY, 5, 0)
    | 0x33 -> (RLA, AddressingMode.IndirectY, 8, 0)
    | 0x35 -> (AND, AddressingMode.ZeroPageX, 4, 0)
    | 0x36 -> (ROL, AddressingMode.ZeroPageX, 6, 0)
    | 0x37 -> (RLA, AddressingMode.ZeroPageX, 6, 0)
    | 0x38 -> (SEC, AddressingMode.Implicit, 2, 0)
    | 0x39 -> (AND, AddressingMode.AbsoluteY, 4, 0)
    | 0x3B -> (RLA, AddressingMode.AbsoluteY, 7, 0)
    | 0x3D -> (AND, AddressingMode.AbsoluteX, 4, 0)
    | 0x3E -> (ROL, AddressingMode.AbsoluteX, 7, 0)
    | 0x3F -> (RLA, AddressingMode.AbsoluteX, 7, 0)
    | 0x40 -> (RTI, AddressingMode.Implicit, 6, 0)
    | 0x41 -> (EOR, AddressingMode.IndirectX, 6, 0)
    | 0x43 -> (SRE, AddressingMode.IndirectX, 8, 0)
    | 0x45 -> (EOR, AddressingMode.ZeroPage, 3, 0)
    | 0x46 -> (LSR, AddressingMode.ZeroPage, 5, 0)
    | 0x47 -> (SRE, AddressingMode.ZeroPage, 5, 0)
    | 0x48 -> (PHA, AddressingMode.Implicit, 3, 0)
    | 0x49 -> (EOR, AddressingMode.Immediate, 2, 0)
    | 0x4A -> (LSR, AddressingMode.Accumulator, 2, 0)
    | 0x4C -> (JMP, AddressingMode.Absolute, 3, 0)
    | 0x4D -> (EOR, AddressingMode.Absolute, 4, 0)
    | 0x4E -> (LSR, AddressingMode.Absolute, 6, 0)
    | 0x4F -> (SRE, AddressingMode.Absolute, 6, 0)
    | 0x50 -> (BVC, AddressingMode.Relative, 2, 1)
    | 0x51 -> (EOR, AddressingMode.IndirectY, 5, 0)
    | 0x53 -> (SRE, AddressingMode.IndirectY, 8, 0)
    | 0x55 -> (EOR, AddressingMode.ZeroPageX, 4, 0)
    | 0x56 -> (LSR, AddressingMode.ZeroPageX, 6, 0)
    | 0x57 -> (SRE, AddressingMode.ZeroPageX, 6, 0)
    | 0x58 -> (CLI, AddressingMode.Implicit, 2, 0)
    | 0x59 -> (EOR, AddressingMode.AbsoluteY, 4, 0)
    | 0x5B -> (SRE, AddressingMode.AbsoluteY, 7, 0)
    | 0x5D -> (EOR, AddressingMode.AbsoluteX, 4, 0)
    | 0x5E -> (LSR, AddressingMode.AbsoluteX, 7, 0)
    | 0x5F -> (SRE, AddressingMode.AbsoluteX, 7, 0)
    | 0x60 -> (RTS, AddressingMode.Implicit, 6, 0)
    | 0x61 -> (ADC, AddressingMode.IndirectX, 6, 0)
    | 0x63 -> (RRA, AddressingMode.IndirectX, 8, 0)
    | 0x65 -> (ADC, AddressingMode.ZeroPage, 3, 0)
    | 0x66 -> (ROR, AddressingMode.ZeroPage, 5, 0)
    | 0x67 -> (RRA, AddressingMode.ZeroPage, 5, 0)
    | 0x68 -> (PLA, AddressingMode.Implicit, 4, 0)
    | 0x69 -> (ADC, AddressingMode.Immediate, 2, 0)
    | 0x6A -> (ROR, AddressingMode.Accumulator, 2, 0)
    | 0x6C -> (JMP, AddressingMode.Indirect, 5, 0)
    | 0x6D -> (ADC, AddressingMode.Absolute, 4, 0)
    | 0x6E -> (ROR, AddressingMode.Absolute, 6, 0)
    | 0x6F -> (RRA, AddressingMode.Absolute, 6, 0)
    | 0x70 -> (BVS, AddressingMode.Relative, 2, 1)
    | 0x71 -> (ADC, AddressingMode.IndirectY, 5, 0)
    | 0x73 -> (RRA, AddressingMode.IndirectY, 8, 0)
    | 0x75 -> (ADC, AddressingMode.ZeroPageX, 4, 0)
    | 0x76 -> (ROR, AddressingMode.ZeroPageX, 6, 0)
    | 0x77 -> (RRA, AddressingMode.ZeroPageX, 6, 0)
    | 0x78 -> (SEI, AddressingMode.Implicit, 2, 0)
    | 0x79 -> (ADC, AddressingMode.AbsoluteY, 4, 0)
    | 0x7B -> (RRA, AddressingMode.AbsoluteY, 7, 0)
    | 0x7D -> (ADC, AddressingMode.AbsoluteX, 4, 0)
    | 0x7E -> (ROR, AddressingMode.AbsoluteX, 7, 0)
    | 0x7F -> (RRA, AddressingMode.AbsoluteX, 7, 0)
    | 0x81 -> (STA, AddressingMode.IndirectX, 6, 0)
    | 0x83 -> (SAX, AddressingMode.IndirectX, 6, 0)
    | 0x84 -> (STY, AddressingMode.ZeroPage, 3, 0)
    | 0x85 -> (STA, AddressingMode.ZeroPage, 3, 0)
    | 0x86 -> (STX, AddressingMode.ZeroPage, 3, 0)
    | 0x87 -> (SAX, AddressingMode.ZeroPage, 3, 0)
    | 0x88 -> (DEY, AddressingMode.Implicit, 2, 0)
    | 0x8A -> (TXA, AddressingMode.Implicit, 2, 0)
    | 0x8C -> (STY, AddressingMode.Absolute, 4, 0)
    | 0x8D -> (STA, AddressingMode.Absolute, 4, 0)
    | 0x8E -> (STX, AddressingMode.Absolute, 4, 0)
    | 0x8F -> (SAX, AddressingMode.Absolute, 4, 0)
    | 0x90 -> (BCC, AddressingMode.Relative, 2, 0)
    | 0x91 -> (STA, AddressingMode.IndirectY, 6, 0)
    | 0x94 -> (STY, AddressingMode.ZeroPageX, 4, 0)
    | 0x95 -> (STA, AddressingMode.ZeroPageX, 4, 0)
    | 0x9A -> (TXS, AddressingMode.Implicit, 2, 0)
    | 0x96 -> (STX, AddressingMode.ZeroPageY, 4, 0)
    | 0x97 -> (SAX, AddressingMode.ZeroPageY, 4, 0)
    | 0x98 -> (TYA, AddressingMode.Implicit, 2, 0)
    | 0x99 -> (STA, AddressingMode.AbsoluteY, 5, 0)
    | 0x9D -> (STA, AddressingMode.AbsoluteX, 5, 0)
    | 0xA0 -> (LDY, AddressingMode.Immediate, 2, 0)
    | 0xA1 -> (LDA, AddressingMode.IndirectX, 6, 0)
    | 0xA2 -> (LDX, AddressingMode.Immediate, 2, 0)
    | 0xA3 -> (LAX, AddressingMode.IndirectX, 6, 0)
    | 0xA4 -> (LDY, AddressingMode.ZeroPage, 3, 0)
    | 0xA5 -> (LDA, AddressingMode.ZeroPage, 3, 0)
    | 0xA6 -> (LDX, AddressingMode.ZeroPage, 3, 0)
    | 0xA7 -> (LAX, AddressingMode.ZeroPage, 3, 0)
    | 0xA9 -> (LDA, AddressingMode.Immediate, 2, 0)
    | 0xA8 -> (TAY, AddressingMode.Implicit, 2, 0)
    | 0xAA -> (TAX, AddressingMode.Implicit, 2, 0)
    | 0xAC -> (LDY, AddressingMode.Absolute, 4, 0)
    | 0xAD -> (LDA, AddressingMode.Absolute, 4, 0)
    | 0xAE -> (LDX, AddressingMode.Absolute, 4, 0)
    | 0xAF -> (LAX, AddressingMode.Absolute, 4, 0)
    | 0xB0 -> (BCS, AddressingMode.Relative, 2, 1)
    | 0xB1 -> (LDA, AddressingMode.IndirectY, 5, 1)
    | 0xB3 -> (LAX, AddressingMode.IndirectY, 5, 0)
    | 0xB4 -> (LDY, AddressingMode.ZeroPageX, 4, 0)
    | 0xB5 -> (LDA, AddressingMode.ZeroPageX, 4, 0)
    | 0xB6 -> (LDX, AddressingMode.ZeroPageY, 4, 0)
    | 0xB7 -> (LAX, AddressingMode.ZeroPageY, 4, 0)
    | 0xB8 -> (CLV, AddressingMode.Implicit, 2, 0)
    | 0xB9 -> (LDA, AddressingMode.AbsoluteY, 4, 1)
    | 0xBA -> (TSX, AddressingMode.Implicit, 2, 0)
    | 0xBD -> (LDA, AddressingMode.AbsoluteX, 4, 1)
    | 0xBC -> (LDY, AddressingMode.AbsoluteX, 4, 1)
    | 0xBE -> (LDX, AddressingMode.AbsoluteY, 4, 1)
    | 0xBF -> (LAX, AddressingMode.AbsoluteY, 4, 1)
    | 0xC0 -> (CPY, AddressingMode.Immediate, 2, 0)
    | 0xC1 -> (CMP, AddressingMode.IndirectX, 6, 0)
    | 0xC3 -> (DCP, AddressingMode.IndirectX, 8, 0)
    | 0xC4 -> (CPY, AddressingMode.ZeroPage, 3, 0)
    | 0xC5 -> (CMP, AddressingMode.ZeroPage, 3, 0)
    | 0xC6 -> (DEC, AddressingMode.ZeroPage, 5, 0)
    | 0xC7 -> (DCP, AddressingMode.ZeroPage, 5, 0)
    | 0xC8 -> (INY, AddressingMode.Implicit, 2, 0)
    | 0xC9 -> (CMP, AddressingMode.Immediate, 2, 0)
    | 0xCA -> (DEX, AddressingMode.Implicit, 2, 0)
    | 0xCC -> (CPY, AddressingMode.Absolute, 4, 0)
    | 0xCD -> (CMP, AddressingMode.Absolute, 4, 0)
    | 0xCE -> (DEC, AddressingMode.Absolute, 6, 0)
    | 0xCF -> (DCP, AddressingMode.Absolute, 6, 0)
    | 0xD0 -> (BNE, AddressingMode.Relative, 2, 0)
    | 0xD1 -> (CMP, AddressingMode.IndirectY, 5, 0)
    | 0xD3 -> (DCP, AddressingMode.IndirectY, 5, 0)
    | 0xD5 -> (CMP, AddressingMode.ZeroPageX, 4, 0)
    | 0xD6 -> (DEC, AddressingMode.ZeroPageX, 6, 0)
    | 0xD7 -> (DCP, AddressingMode.ZeroPageX, 6, 0)
    | 0xD8 -> (CLD, AddressingMode.Implicit, 2, 0)
    | 0xD9 -> (CMP, AddressingMode.AbsoluteY, 4, 0)
    | 0xDB -> (DCP, AddressingMode.AbsoluteY, 4, 0)
    | 0xDD -> (CMP, AddressingMode.AbsoluteX, 4, 0)
    | 0xDE -> (DEC, AddressingMode.AbsoluteX, 7, 0)
    | 0xDF -> (DCP, AddressingMode.AbsoluteX, 7, 0)
    | 0xE0 -> (CPX, AddressingMode.Immediate, 2, 0)
    | 0xE1 -> (SBC, AddressingMode.IndirectX, 6, 0)
    | 0xE3 -> (ISB, AddressingMode.IndirectX, 8, 0)
    | 0xE4 -> (CPX, AddressingMode.ZeroPage, 3, 0)
    | 0xE5 -> (SBC, AddressingMode.ZeroPage, 3, 0)
    | 0xE6 -> (INC, AddressingMode.ZeroPage, 5, 0)
    | 0xE7 -> (ISB, AddressingMode.ZeroPage, 5, 0)
    | 0xE8 -> (INX, AddressingMode.Implicit, 2, 0)
    | 0xE9 -> (SBC, AddressingMode.Immediate, 2, 0)
    | 0xEA -> (NOP, AddressingMode.Implicit, 2, 0)
    | 0xEB -> (SBC, AddressingMode.Immediate, 2, 0)
    | 0xEC -> (CPX, AddressingMode.Absolute, 4, 0)
    | 0xED -> (SBC, AddressingMode.Absolute, 4, 0)
    | 0xEE -> (INC, AddressingMode.Absolute, 6, 0)
    | 0xEF -> (ISB, AddressingMode.Absolute, 6, 0)
    | 0xF0 -> (BEQ, AddressingMode.Relative, 2, 1)
    | 0xF1 -> (SBC, AddressingMode.IndirectY, 5, 0)
    | 0xF3 -> (ISB, AddressingMode.IndirectY, 8, 0)
    | 0xF5 -> (SBC, AddressingMode.ZeroPageX, 4, 0)
    | 0xF6 -> (INC, AddressingMode.ZeroPageX, 6, 0)
    | 0xF7 -> (ISB, AddressingMode.ZeroPageX, 6, 0)
    | 0xF8 -> (SED, AddressingMode.Implicit, 2, 0)
    | 0xF9 -> (SBC, AddressingMode.AbsoluteY, 4, 0)
    | 0xFB -> (ISB, AddressingMode.AbsoluteY, 7, 0)
    | 0xFD -> (SBC, AddressingMode.AbsoluteX, 4, 0)
    | 0xFE -> (INC, AddressingMode.AbsoluteX, 7, 0)
    | 0xFF -> (ISB, AddressingMode.AbsoluteX, 7, 0)
    | _ -> failwith (sprintf "Unknown opcode %02X" opcode)
