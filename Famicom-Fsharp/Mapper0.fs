module Mapper0

open MapperBase
open ROM
open PPU

type t = { rom: Rom }

type Mapper0(rom': Rom, ppu': PPU) =
    inherit MapperBase()

    member this.ppu = ppu'
    member this.nesrom = rom'

    member this.setRom(path: string) =
        printfn "Mapper0 set_rom"
        this.nesrom.setRom path
        let _ = this.nesrom.set_prgrom_page (0, 0)
        let _ = this.nesrom.set_prgrom_page (1, this.nesrom.prg_rom_page_count - 1)
        ppu'.set_chr_rom_page(0, this.nesrom);
        ppu'.start (this.nesrom)

    member this.init(rom: byte array) = 
        printfn "mapper init"
        ppu'.init()
