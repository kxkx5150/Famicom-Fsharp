module Mapper0

open MapperBase
open ROM

type t = { rom: Rom }

type Mapper0(rom': Rom) =
    inherit MapperBase()
    member this.nesrom = rom'

    member this.setRom(path: string) =
        printfn "Mapper0 set_rom"
        this.nesrom.setRom path
        let _ = this.nesrom.set_prgrom_page(0, 0)
        let _ = this.nesrom.set_prgrom_page(1, this.nesrom.prg_rom_page_count-1)
        ()

    member this.init(rom: byte array) =
        printfn "mapper init"
        

// let load (rom: Rom) address =
//     if address < 0x2000 then
//         rom.chr.[address]
//     else if address >= 0x6000 && address < 0x8000 then
//         rom.ram.[address]
//     else
//         rom.prg.[address &&& (rom.headers.prg_size - 1)]

// let store (rom: ROM.RomComponentsom) address value =
//     if address < 0x2000 then
//         rom.chr.[address] <- value
//     else if address >= 0x6000 && address < 0x8000 then
//         rom.ram.[address - 0x6000] <- value
//     else
//         failwith (sprintf "Can't store to PRG @ %04X = %02X" address value)



