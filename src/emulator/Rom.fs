module ROM

open System.IO

type Mirroring =
    | VERTICAL
    | HORIZONTAL
    | FOUR_SCREEN

type RomData =
    { rom: byte array
      prg_rom_page_count: int
      chr_rom_page_count: int
      screen_mirroring: Mirroring
      sram_enable: bool
      trainer_Enable: bool
      four_screen: bool
      mapper_number: int

      srams: byte array
      roms: byte array
      prgrom_state: byte array
      chrrom_state: byte array
      prgrom_pages: byte array
      chrrom_pages: byte array }

type Rom() =

    member this.init(rom: byte array) =
        printfn "rom init"
        let hlen = 0x0010
        let prg_psize = 0x4000
        let chr_psize = 0x2000
        ()

    member this.setRom(path: string) =
        let rom = path |> File.ReadAllBytes
        let prg_rom_page_count = int rom[4]
        let chr_rom_page_count = int rom[5]
        let four_screen = (int rom[6] &&& 0b1000) <> 0
        let vertical_mirroring = (int rom[6] &&& 0b1) <> 0

        let screen_mirroring =
            match (four_screen, vertical_mirroring) with
            | (true, _) -> Mirroring.FOUR_SCREEN
            | (false, true) -> Mirroring.VERTICAL
            | (false, false) -> Mirroring.HORIZONTAL

        let sram_enable = (int rom[6] &&& 0x02) <> 0
        let trainer_Enable = (int rom[6] &&& 0x04) <> 0
        let mapper_number = (int rom[6] >>> 4) ||| (int rom[7] &&& 0xf0)
        let prg_rom_size = int rom[4] * 16384
        let chr_rom_size = int rom[5] * 8192

        let hlen = 0x0010
        let prg_psize = 0x4000
        let chr_psize = 0x2000
        let prgrom_pages = Array2D.zeroCreate<byte> (prg_rom_page_count*2) 2
        let chrrom_pages = Array2D.zeroCreate<byte> (chr_rom_page_count*8) 2



        // let chr =
        //     romData[(0x10 + headers.prg_size) .. (0x10 + headers.prg_size + headers.chr_size)]

        // let prg = romData[0x10 .. (0x10 + headers.prg_size)]
        // let ram: byte array = Array.zeroCreate 0x2000
        // printfn "Loaded %s\n" path
        // { headers = headers; prg = prg; chr = chr; ram = ram}

        ()