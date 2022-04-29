module PPU

open Irq
open ROM
open COLOR

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

    let mutable vram = Array2D.zeroCreate<byte> 16 0x2000
    let mutable vrams = Array2D.zeroCreate<byte> 16 0x2000

    let mutable bg_line_buffer: byte array = Array.zeroCreate<byte> 264
    let mutable sp_line_buffer: int array = Array.zeroCreate<int> 264

    let mutable palette: byte array = Array.create 33 (byte 0x0f)
    let mutable sprite_ram: byte array = Array.create 0x100 (byte 0)
    let mutable spbit_pattern = Array3D.zeroCreate<byte> 256 256 8

    member this.init() =
        printfn "ppu init"
        this.reset

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
            let vv = rom.chrrom_pages[ridx, *]

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

        ()

    member this.run(cpuclock: int, irq: Irq) =
        let mutable tmpx = ppux
        ppux <- ppux + cpuclock * 3

        while 341 <= ppux do
            ppux <- ppux - 341
            line <- line + 1
            tmpx <- 0
            sprite_zero <- false

            if line < 240 then
                this.render_frame ()
            elif line = 240 then
                this.in_vblank (irq)
            elif line = 262 then
                this.post_render ()

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


    member this.render_frame() =
        if this.is_screen_enable ()
           || this.is_sprite_enable () then
            ppu_addr <-
                (ppu_addr &&& 0xfbe0)
                ||| (ppu_addr_buffer &&& 0x041f)

            if 8 <= line && line < 232 then
                this.build_bg ()
                // this.build_sp_line()
                for p in 0..255 do
                    let idx = palette[int bg_line_buffer[p]]
                    let pal = PALLETE_TABLE[int idx]
                    this.set_img_data (pal)
            else
                for p in 0..263 do
                    bg_line_buffer[p] <- byte 0x10
            // this.build_sp_line();

            if (ppu_addr &&& 0x7000) = 0x7000 then
                ppu_addr <- ppu_addr &&& 0x8fff

                if (ppu_addr &&& 0x03e0) = 0x03a0 then
                    ppu_addr <- (ppu_addr ^^^ 0x0800) &&& 0xfc1f
                elif (ppu_addr &&& 0x03e0) = 0x03e0 then
                    ppu_addr <- ppu_addr &&& 0xfc1f
                else
                    ppu_addr <- ppu_addr + 0x0020
            else
                ppu_addr <- ppu_addr + 0x1000

        elif 8 <= line && line < 232 then
            let pal = PALLETE_TABLE[int palette[0x10]]

            for x in 0..255 do
                this.set_img_data (pal)

    member this.build_bg() =
        if (regs[0x01] &&& byte 0x08) <> byte 0x08 then
            for p in 0..263 do
                bg_line_buffer[p] <- byte 0x10
        else
        // this.build_bg_line();
        if (regs[0x01] &&& byte 0x02) <> byte 0x02 then
            for p in 0..7 do
                bg_line_buffer[p] <- byte 0x10

    member this.build_bg_line() = printfn ""

    member this.build_sp_line() = printfn ""



    member this.in_vblank(irq: Irq) =
        scroll_reg_flg <- false
        regs[0x02] <- regs[0x02] &&& byte 0x1f
        regs[0x02] <- regs[0x02] ||| byte 0x80

        if (regs[0x00] &&& byte 0x80) = byte 0x80 then
            irq.nmi <- true

    member this.post_render() =
        line <- 0

        if (this.is_screen_enable ()
            || this.is_sprite_enable ()) then
            ppu_addr <- ppu_addr_buffer

        regs[0x02] <- regs[0x02] &&& byte 0x7f
        imgok <- true

    member this.set_img_data((a: int, b: int, c: int)) =
        imgdata[imgidx] <- byte a
        imgdata[imgidx + 1] <- byte b
        imgdata[imgidx + 2] <- byte c
        imgidx <- imgidx + 3

    member this.clear_img() =
        imgidx <- 0
        imgok <- false

    member this.get_img_status() =
        if imgok then
            (true, imgdata)
        else
            (false, imgdata)

    member this.is_screen_enable() = (regs[0x01] &&& byte 0x08) = byte 0x08

    member this.is_sprite_enable() = (regs[0x01] &&& byte 0x10) = byte 0x10

    member this.is_bigsize() =
        if ((regs[0x00] &&& byte 0x20) = byte 0x20) then
            16
        else
            8








    member this.write_scroll_reg(value: byte) =
        regs[0x05] <- value

        if scroll_reg_flg then
            ppu_addr_buffer <-
                ((ppu_addr_buffer &&& 0x8c1f)
                 ||| ((int value &&& 0xf8) <<< 2)
                 ||| ((int value &&& 0x07) <<< 12))
        else
            ppu_addr_buffer <-
                (ppu_addr_buffer &&& 0xffe0)
                ||| ((int value &&& 0xf8) >>> 3)

            h_scroll_val <- (int value &&& 7)

        scroll_reg_flg <- not scroll_reg_flg

    member this.write_ppu_ctrl0_reg(value: byte) =
        regs[0x00] <- value

        ppu_addr_buffer <-
            ((ppu_addr_buffer &&& 0xf3ff)
             ||| ((int value &&& 0x03) <<< 10))

    member this.write_ppu_ctrl1_reg(value: byte) =
        regs[0x01] <- value

        if this.is_screen_enable () then
            printfn ""

    member this.read_ppu_status_reg(value: byte) =
        let result = regs[0x02]
        regs[0x02] <- (regs[0x02] &&& byte 0x1f)
        scroll_reg_flg <- false
        ppu_addr_reg_flg <- false
        result

    member this.write_ppu_addr_reg(value: byte) =
        regs[0x06] <- value

        if ppu_addr_reg_flg then
            ppu_addr_buffer <- (ppu_addr_buffer &&& 0xff00) ||| int value
            ppu_addr <- ppu_addr_buffer
        else
            ppu_addr_buffer <-
                (ppu_addr_buffer &&& 0x00ff)
                ||| ((int value &&& 0x3f) <<< 8)

        ppu_addr_reg_flg <- not ppu_addr_reg_flg

    member this.read_ppu_data_reg(value: byte) =
        let tmp = ppu_read_buffer
        let addr = ppu_addr &&& 0x3fff
        ppu_read_buffer <- int vram[addr >>> 10, addr &&& 0x03ff]

        let pval =
            if ((regs[0x00] &&& byte 0x04) = byte 0x04) then
                32
            else
                1

        ppu_addr <- (ppu_addr + pval) &&& 0xffff
        tmp

    member this.write_ppu_data_reg(value: byte) =
        regs[0x07] <- value
        let tmpppu_addr = ppu_addr &&& 0x3fff
        vram[tmpppu_addr >>> 10, tmpppu_addr &&& 0x03ff] <- value

        if tmpppu_addr < 0x3000 then
            let pval =
                if ((regs[0x00] &&& byte 0x04) = byte 0x04) then
                    32
                else
                    1

            ppu_addr <- (ppu_addr + pval) &&& 0xffff
        elif tmpppu_addr < 0x3eff then
            vram[(tmpppu_addr - 0x1000) >>> 10, (tmpppu_addr - 0x1000) &&& 0x03ff] <- value

            let pval =
                if ((regs[0x00] &&& byte 0x04) = byte 0x04) then
                    32
                else
                    1

            ppu_addr <- (ppu_addr + pval) &&& 0xffff
        else
            let palNo = tmpppu_addr &&& 0x001f

            if (palNo = 0x00 || palNo = 0x10) then
                palette[0x10] <- value &&& byte 0x3f
                palette[0x00] <- palette[0x10]
            else
                palette[palNo] <- value &&& byte 0x3f

            let pval =
                if ((regs[0x00] &&& byte 0x04) = byte 0x04) then
                    32
                else
                    1

            ppu_addr <- (ppu_addr + pval) &&& 0xffff

    member this.write_sprite_data(value: byte) =
        let idx = regs[0x03]
        sprite_ram[int idx] <- value
        regs[0x03] <- (regs[0x03] + byte 1) &&& byte 0xff

    member this.write_sprite_addr_reg(value: byte) = regs[0x03] <- value
