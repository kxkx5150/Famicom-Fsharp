module PPU

open ROM

type PPU() =
    let mutable ppux: int = 341
    let mutable line: int = 0
    let mutable regs: byte array = Array.zeroCreate<byte> 8
    let mutable imgdata: byte array = Array.zeroCreate<byte> (256 * 240 * 3)
    let mutable imgok: bool = false
    let mutable imgidx: int = 0
    let mutable rcount: int = 0

    let mutable sprite_zero: bool = false
    let mutable scroll_reg_flg: bool = false
    let mutable ppu_addr_buffer: int = 0
    let mutable h_scroll_val: int = 0
    let mutable ppu_addr_reg_flg: bool = false
    let mutable ppu_addr: int = 0
    let mutable ppu_read_buffer: int = 0
    let mutable screen_mirroring: Mirroring = Mirroring.HORIZONTAL

    let mutable vram = Array2D.zeroCreate<byte> 16 4096
    let mutable vrams = Array2D.zeroCreate<byte> 16 1024

    let mutable bg_line_buffer: byte array = Array.zeroCreate<byte> 264
    let mutable sp_line_buffer: int array = Array.zeroCreate<int> 264

    let mutable palette: byte array = Array.create 33 (byte 0x0f)
    let mutable sprite_ram: byte array = Array.create 0x100 (byte 0)
    let mutable spbit_pattern = Array3D.zeroCreate<byte> 256 256 8

    member this.init() = this.reset

    member this.start(rom': Rom) =
        printfn "ppu start"
        this.crate_spbit_array ()
        screen_mirroring <- rom'.getscreen_mirroring

        let _ =
            match screen_mirroring with
            | Mirroring.VERTICAL -> this.set_mode_mirror (false, rom')
            | Mirroring.HORIZONTAL -> this.set_mode_mirror (true, rom')
            | Mirroring.FOUR_SCREEN -> printfn ""

        ppux <- 341
        line <- 0
        sprite_zero <- false
        imgok <- false

    member this.reset() =
        imgidx <- 0
        scroll_reg_flg <- false
        ppu_addr_reg_flg <- false
        ppu_addr_buffer <- 0
        ppu_read_buffer <- 0
        ppu_addr <- 0
        h_scroll_val <- 0
        ppux <- 341
        line <- 0
        sprite_zero <- false
        imgok <- false
        this.clear_arryas ()

    member this.clear_arryas() = printfn ""

    member this.crate_spbit_array() =
        for i in 0 .. (256 - 1) do
            for j in 0 .. (256 - 1) do
                for k in 0 .. (8 - 1) do
                    let lval = (((i <<< k) &&& 0x80) >>> 7)
                    let rval = (((j <<< k) &&& 0x80) >>> 6)
                    let sval = (lval ||| rval)
                    spbit_pattern[i, j, k] <- byte sval


    member this.set_mode_mirror(value: bool, rom: Rom) =
        if value then
            this.init_mirrors (0, 0, 1, 1, rom)
        else
            this.init_mirrors (0, 1, 0, 1, rom)

    member this.init_mirrors(value0: int, value1: int, value2: int, value3: int, rom: Rom) =
        this.set_chr_rom_data1k (8, value0 + 8 + 0x0100, rom)
        this.set_chr_rom_data1k (9, value1 + 8 + 0x0100, rom)
        this.set_chr_rom_data1k (10, value2 + 8 + 0x0100, rom)
        this.set_chr_rom_data1k (11, value3 + 8 + 0x0100, rom)

    member this.set_chr_rom_data1k(page: int, romPage: int, rom: Rom) =
        if 0x0100 <= romPage then
            rom.setchrrom_state[ page ] <- romPage
            let ridx = romPage &&& 0xff
            let vv = vrams[ridx, *]

            for j in 0 .. (vv.Length - 1) do
                vram[page, j] <- vv[j]
        else if 0 < rom.chr_rom_page_count then
            let tmp = romPage % (rom.chr_rom_page_count * 8)
            rom.setchrrom_state[ page ] <- tmp
            let ridx = rom.setchrrom_state[page]
            let vv = vrams[ridx, *]

            for j in 0 .. (vv.Length - 1) do
                vram[page, j] <- vv[j]

    member this.set_chrrom_pages1k
        (
            rompage0: int,
            rompage1: int,
            rompage2: int,
            rompage3: int,
            rompage4: int,
            rompage5: int,
            rompage6: int,
            rompage7: int,
            rom: Rom
        ) =
        this.set_chr_rom_data1k (0, rompage0, rom)
        this.set_chr_rom_data1k (1, rompage1, rom)
        this.set_chr_rom_data1k (2, rompage2, rom)
        this.set_chr_rom_data1k (3, rompage3, rom)
        this.set_chr_rom_data1k (4, rompage4, rom)
        this.set_chr_rom_data1k (5, rompage5, rom)
        this.set_chr_rom_data1k (6, rompage6, rom)
        this.set_chr_rom_data1k (7, rompage7, rom)

    member this.set_chr_rom_page(num: int, rom: Rom) =
        let numval = num <<< 3

        for i in 0..7 do
            this.set_chr_rom_data1k (i, num + i, rom)

    member this.run(cpuclock: int) =
        let mutable tmpx = ppux
        ppux <- ppux + cpuclock * 3

        while 341 <= ppux do
            ppux <- ppux - 341
            line <- line + 1
            tmpx <- 0
            sprite_zero <- false

            if line < 240 then printfn ""
            elif line = 240 then printfn ""
            elif line = 262 then printfn ""

    // if (self.sprite_zero && (self.regs[0x02] & 0x40) != 0x40) {
    //     let i = if self.ppux > 255 { 255 } else { self.ppux };
    //     while tmpx <= i {
    //         if (self.sp_line_buffer[tmpx] == 0) {
    //             self.regs[0x02] |= 0x40;
    //             break;
    //         }
    //         tmpx += 1;
    //     }
    // }

    member this.render_frame() = printfn ""

    member this.build_bg() = printfn ""

    member this.build_bg_line() = printfn ""

    member this.build_sp_line() = printfn ""




    member this.in_vblank() = printfn ""

    member this.post_render() = printfn ""

    member this.set_img_data() = printfn ""

    member this.clear_img() = printfn ""

    member this.get_img_status() = printfn ""

    member this.is_screen_enable() = printfn ""

    member this.is_sprite_enable() = printfn ""

    member this.is_bigsize() = printfn ""





    member this.write_scroll_reg(value: byte) = 
        regs[0x05] <- value

        if scroll_reg_flg then 
            ppu_addr_buffer <- ((ppu_addr_buffer &&& 0x8c1f)
                ||| ((int value &&& 0xf8) <<< 2)
                ||| ((int value &&& 0x07) <<< 12))
        else
            ppu_addr_buffer <- (ppu_addr_buffer &&& 0xffe0) 
            ||| ((int value &&& 0xf8) >>> 3)
            h_scroll_val <- (int value &&& 7)
        scroll_reg_flg <- not scroll_reg_flg

    member this.write_ppu_ctrl0_reg(value: byte) =
        regs[0x00] <- value
        ppu_addr_buffer <- ((ppu_addr_buffer &&& 0xf3ff) ||| ((int value &&& 0x03) <<< 10))
    
    member this.write_ppu_ctrl1_reg(value: byte) =
        regs[0x01] <- value

    member this.read_ppu_status_reg() =
        let result = regs[0x02]
        regs[0x02] <- (regs[0x02] &&& byte 0x1f)
        scroll_reg_flg <- false
        ppu_addr_reg_flg <- false
        result

    member this.write_ppu_addr_reg() = printfn ""

    member this.read_ppu_data_reg() = printfn ""

    member this.write_ppu_data_reg() = printfn ""

    member this.write_sprite_data() = printfn ""

    member this.write_sprite_addr_reg() = printfn ""


let PALLETE =
    [| 0x10
       0x01
       0x02
       0x03
       0x10
       0x05
       0x06
       0x07
       0x10
       0x09
       0x0a
       0x0b
       0x10
       0x0d
       0x0e
       0x0f |]


let PALLETE_TABLE =
    [ (101, 101, 101)
      (0, 45, 105)
      (19, 31, 127)
      (60, 19, 124)
      (96, 11, 98)
      (115, 10, 55)
      (113, 15, 7)
      (90, 26, 0)
      (52, 40, 0)
      (11, 52, 0)
      (0, 60, 0)
      (0, 61, 16)
      (0, 56, 64)
      (0, 0, 0)
      (0, 0, 0)
      (0, 0, 0)
      (174, 174, 174)
      (15, 99, 179)
      (64, 81, 208)
      (120, 65, 204)
      (167, 54, 169)
      (192, 52, 112)
      (189, 60, 48)
      (159, 74, 0)
      (109, 92, 0)
      (54, 109, 0)
      (7, 119, 4)
      (0, 121, 61)
      (0, 114, 125)
      (0, 0, 0)
      (0, 0, 0)
      (0, 0, 0)
      (254, 254, 255)
      (93, 179, 255)
      (143, 161, 255)
      (200, 144, 255)
      (247, 133, 250)
      (255, 131, 192)
      (255, 139, 127)
      (239, 154, 73)
      (189, 172, 44)
      (133, 188, 47)
      (85, 199, 83)
      (60, 201, 140)
      (62, 194, 205)
      (78, 78, 78)
      (0, 0, 0)
      (0, 0, 0)
      (254, 254, 255)
      (188, 223, 255)
      (209, 216, 255)
      (232, 209, 255)
      (251, 205, 253)
      (255, 204, 229)
      (255, 207, 202)
      (248, 213, 180)
      (228, 220, 168)
      (204, 227, 169)
      (185, 232, 184)
      (174, 232, 208)
      (175, 229, 234)
      (182, 182, 182)
      (0, 0, 0)
      (0, 0, 0) ]
