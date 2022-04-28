module ROM

open System.IO

type Mirroring =
    | VERTICAL
    | HORIZONTAL
    | FOUR_SCREEN

type Rom() =
    let mutable rom: byte array = [||]
    let mutable prg_rom_pcount: int = 0
    let mutable chr_rom_page_count: int = 0
    let mutable screen_mirroring: Mirroring = Mirroring.VERTICAL
    let mutable sram_enable: bool = false
    let mutable trainer_Enable: bool = false
    let mutable four_screen: bool = false
    let mutable mapper_number: int = 0

    let mutable srams: byte array = Array.zeroCreate<byte> 0x2000
    let mutable prgrom_state: int array = Array.zeroCreate<int> 4
    let mutable chrrom_state: int array = Array.zeroCreate<int> 16
    let mutable prgrom_pages = Array2D.zeroCreate<byte> 1 1
    let mutable chrrom_pages = Array2D.zeroCreate<byte> 1 1
    let mutable roms' = Array2D.zeroCreate<byte> 4 0x4000
    member this.roms = roms'

    member this.init(rom: byte array) =
        printfn "rom init"
        let hlen = 0x0010
        let prg_psize = 0x4000
        let chr_psize = 0x2000
        ()

    member this.prg_rom_page_count = prg_rom_pcount

    member this.setRom(path: string) =
        rom <- path |> File.ReadAllBytes
        prg_rom_pcount <- int rom[4]
        chr_rom_page_count <- int rom[5]
        four_screen <- (int rom[6] &&& 0b1000) <> 0
        let vertical_mirroring = (int rom[6] &&& 0b1) <> 0

        screen_mirroring <-
            match (four_screen, vertical_mirroring) with
            | (true, _) -> Mirroring.FOUR_SCREEN
            | (false, true) -> Mirroring.VERTICAL
            | (false, false) -> Mirroring.HORIZONTAL

        sram_enable <- (int rom[6] &&& 0x02) <> 0
        trainer_Enable <- (int rom[6] &&& 0x04) <> 0
        mapper_number <- (int rom[6] >>> 4) ||| (int rom[7] &&& 0xf0)

        let prg_rom_size = int rom[4] * 16384
        let chr_rom_size = int rom[5] * 8192
        let hlen = 0x0010
        let prg_psize = 0x4000
        let chr_psize = 0x2000

        prgrom_pages <- Array2D.zeroCreate<byte> (prg_rom_pcount * 2) prg_psize
        chrrom_pages <- Array2D.zeroCreate<byte> (chr_rom_page_count * 8) chr_psize

        if (0 < prg_rom_pcount) then
            for i in 0 .. ((prg_rom_pcount * 2) - 1) do
                let offset = hlen + (prg_psize / 2) * i
                let v = rom[offset .. (offset + prg_psize / 2)]

                for j in 0 .. (v.Length - 1) do
                    prgrom_pages[i, j] <- v[j]

        if (0 < chr_rom_page_count) then
            let romlen = rom.Length

            for i in 0 .. (chr_rom_page_count - 1) do
                let chrrom_offset =
                    hlen
                    + prg_psize * prg_rom_pcount
                    + (chr_psize / 8) * i

                let mutable h = chrrom_offset + chr_psize / 2
                if h > romlen then h <- romlen
                let chr = rom[chrrom_offset..h]

                for j in 0 .. (chr.Length - 1) do
                    chrrom_pages[i, j] <- chr[j]

    member this.clear_roms() = printfn "clear_roms"


    member this.set_prgrom_page_8k(page: int, rompage: int) =
        if rompage < 0 then
            prgrom_state[page] <- rompage
        else
            let pidx = rompage % (this.prg_rom_page_count * 2)
            prgrom_state[page] <- pidx
            let idx = prgrom_state[page]
            let vv = prgrom_pages[idx, *]

            for j in 0 .. (vv.Length - 1) do
                this.roms[ page, j ] <- vv[j]

    member this.set_prgrom_page(no: int, num: int) =
        this.set_prgrom_page_8k ((no * 2), (num * 2))
        this.set_prgrom_page_8k ((no * 2 + 1), (num * 2 + 1))
