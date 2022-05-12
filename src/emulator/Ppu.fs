module PPU
open System
open Irq
open ROM
// open COLOR

type PPU() =
    let mutable ppux: int = 341
    let mutable line: int = 0
    let mutable regs: byte array = Array.zeroCreate<byte> 8
    let mutable imgdata: byte array = Array.zeroCreate<byte> (256 * 240 * 3)

    // let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])
    // let mutable imgdata = Array.create (256 * 240) (asUint32 (0uy, 0uy, 0uy))


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

    let PALLETE:byte array =
        [| 
        10uy
        0x01uy
        0x02uy
        0x03uy
        0x10uy
        0x05uy
        0x06uy
        0x07uy
        0x10uy
        0x09uy
        0x0auy
        0x0buy
        0x10uy
        0x0duy
        0x0euy
        0x0fuy
        |]
    let PALLETE_TABLE: byte array array=
        [| 
        [|101uy; 101uy; 101uy |]
        [|0uy; 45uy; 105uy |]
        [|19uy; 31uy; 127uy |]
        [|60uy; 19uy; 124uy |]
        [|96uy; 11uy; 98uy |]
        [|115uy; 10uy; 55uy |]
        [|113uy; 15uy; 7uy |]
        [|90uy; 26uy; 0uy |]
        [|52uy; 40uy; 0uy |]
        [|11uy; 52uy; 0uy |]
        [|0uy; 60uy; 0uy |]
        [|0uy; 61uy; 16uy |]
        [|0uy; 56uy; 64uy |]
        [|0uy; 0uy; 0uy |]
        [|0uy; 0uy; 0uy |]
        [|0uy; 0uy; 0uy |]
        [|174uy; 174uy; 174uy |]
        [|15uy; 99uy; 179uy |]
        [|64uy; 81uy; 208uy |]
        [|120uy; 65uy; 204uy |]
        [|167uy; 54uy; 169uy |]
        [|192uy; 52uy; 112uy |]
        [|189uy; 60uy; 48uy |]
        [|159uy; 74uy; 0uy |]
        [|109uy; 92uy; 0uy |]
        [|54uy; 109uy; 0uy |]
        [|7uy; 119uy; 4uy |]
        [|0uy; 121uy; 61uy |]
        [|0uy; 114uy; 125uy |]
        [|0uy; 0uy; 0uy |]
        [|0uy; 0uy; 0uy |]
        [|0uy; 0uy; 0uy |]
        [|254uy; 254uy; 255uy |]
        [|93uy; 179uy; 255uy |]
        [|143uy; 161uy; 255uy |]
        [|200uy; 144uy; 255uy |]
        [|247uy; 133uy; 250uy |]
        [|255uy; 131uy; 192uy |]
        [|255uy; 139uy; 127uy |]
        [|239uy; 154uy; 73uy |]
        [|189uy; 172uy; 44uy |]
        [|133uy; 188uy; 47uy |]
        [|85uy; 199uy; 83uy |]
        [|60uy; 201uy; 140uy |]
        [|62uy; 194uy; 205uy |]
        [|78uy; 78uy; 78uy |]
        [|0uy; 0uy; 0uy |]
        [|0uy; 0uy; 0uy |]
        [|254uy; 254uy; 255uy |]
        [|188uy; 223uy; 255uy |]
        [|209uy; 216uy; 255uy |]
        [|232uy; 209uy; 255uy |]
        [|251uy; 205uy; 253uy |]
        [|255uy; 204uy; 229uy |]
        [|255uy; 207uy; 202uy |]
        [|248uy; 213uy; 180uy |]
        [|228uy; 220uy; 168uy |]
        [|204uy; 227uy; 169uy |]
        [|185uy; 232uy; 184uy |]
        [|174uy; 232uy; 208uy |]
        [|175uy; 229uy; 234uy |]
        [|182uy; 182uy; 182uy |]
        [|0uy; 0uy; 0uy |]
        [|0uy; 0uy; 0uy |] 
        |]







    member this.sprite_ram' = sprite_ram
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

            ()
        else if 0 < rom.chr_rom_page_count then
            let tmp = romPage % (rom.chr_rom_page_count * 8)
            rom.setchrrom_state[ page ] <- tmp

            let ridx = rom.setchrrom_state[page]
            let vv = rom.chrrom_pages[ridx, *]

            for j in 0 .. (vv.Length - 1) do
                vram[page, j] <- vv[j]


            ()

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

        // if sprite_zero && ((regs[0x02] &&& byte 0x40) <> byte 0x40) then
        //     let mutable i = if ppux > 255 then 255 else ppux

        //     while tmpx <= i do
        //         if sp_line_buffer[tmpx] = 0 then
        //             regs[0x02] <- (regs[0x02] ||| byte 0x40)
        //             i <- 0

        //         tmpx <- tmpx + 1

    member this.render_frame() =
        if this.is_screen_enable ()
           || this.is_sprite_enable () then
            ppu_addr <-
                (ppu_addr &&& 0xfbe0)
                ||| (ppu_addr_buffer &&& 0x041f)

            if 8 <= line && line < 232 then
                this.build_bg ()
                // this.build_sp_line ()

                for p in 0..255 do
                    let idx = palette[int bg_line_buffer[p]]
                    let pal = PALLETE_TABLE[int idx]
                    this.set_img_data (pal[0], pal[1], pal[2])
            else
                for p in 0..263 do
                    bg_line_buffer[p] <- byte 0x10

                // this.build_sp_line ()

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
                this.set_img_data (pal[0], pal[1], pal[2])


    member this.build_bg() =
        if (regs[0x01] &&& byte 0x08) <> byte 0x08 then
            for p in 0..263 do
                bg_line_buffer[p] <- byte 0x10
        else
            this.build_bg_line ()

            if (regs[0x01] &&& byte 0x02) <> byte 0x02 then
                for p in 0..7 do
                    bg_line_buffer[p] <- byte 0x10

    member this.build_bg_line() =
        let nameaddr = 0x2000 ||| (ppu_addr &&& 0x0fff)
        let tableaddr =
            ((ppu_addr &&& 0x7000) >>> 12)
            ||| (((int regs[0x00] &&& 0x10)) <<< 8)

        let mutable name_addr_h = nameaddr >>> 10
        let mutable name_addr_l = nameaddr &&& 0x03ff
        let mutable pre_name_addrh = name_addr_h
        let mutable s = h_scroll_val
        let mutable q = 0

        for p in 0..32 do
            let ptnidx = ((int vram[pre_name_addrh, name_addr_l]) <<< 4)

            let ptndist = ptnidx ||| tableaddr
            let ptndist2 = ptndist &&& 0x03ff

            let lval = (name_addr_l &&& 0x0380) >>> 4
            let rval = ((name_addr_l &&& 0x001c) >>> 2) + 0x03c0

            let lval2 = (name_addr_l &&& 0x0040) >>> 4
            let rval2 = name_addr_l &&& 0x0002

            let attr =
                ((int (vram[pre_name_addrh, lval ||| rval]) <<< 2)
                 >>> (lval2 ||| rval2))
                &&& 0x0c

            let spbidx1 = vram[(int ptndist >>> 10), ptndist2]
            let spbidx2 = vram[(int ptndist >>> 10), ptndist2+8]

            while s < 8 do
                let idx = spbit_pattern[int spbidx1, int spbidx2, s] ||| byte attr
                bg_line_buffer[q] <- byte PALLETE[int idx]
                q <- q + 1
                s <- s + 1

            s <- 0

            if (name_addr_l &&& 0x001f) = 0x001f then
                name_addr_l <- name_addr_l &&& 0xffe0
                name_addr_h <- name_addr_h ^^^ 0x01
                pre_name_addrh <- name_addr_h
            else
                name_addr_l <- name_addr_l + 1



    member this.build_sp_line() =
        let spclip =
            if (regs[0x01] &&& byte 0x04) = byte 0x04 then
                0
            else
                8

        if ((regs[0x01] &&& byte 0x10) = byte 0x10) then
            for p in 0..263 do
                sp_line_buffer[p] <- 256

            let spptableaddr = ((int regs[0x00] &&& 0x08)) <<< 9
            let mutable count = 0
            let bzsize = this.is_bigsize ()
            let mutable i = 0
            let mutable brk = false

            while not brk do
                if 252 <= i then brk <- true

                let isy = (int sprite_ram[i] + 1)

                if not (isy > line || (isy + bzsize <= line)) then
                    if i = 0 then sprite_zero <- true
                    count <- count + 1

                    if count = 9 then
                        brk <- true
                    else
                        let attr = int sprite_ram[i + 2]
                        let attribute = (((attr &&& 0x03)) <<< 2) ||| 0x10
                        let bgsp = (attr &&& 0x20) = 0x00

                        let mutable x = int sprite_ram[i + 3]
                        let mutable ex = x + 8
                        if ex > 256 then ex <- 256

                        let iy =
                            if (attr &&& 0x80) = 0x80 then
                                bzsize - 1 - (line - isy)
                            else
                                line - isy

                        let lval = ((int sprite_ram[i + 1]) <<< 4) + spptableaddr

                        let rval =
                            ((int sprite_ram[i + 1] &&& 0xfe) <<< 4)
                            + ((int sprite_ram[i + 1] &&& 0x01) <<< 12)

                        let sval = if bzsize = 8 then lval else rval
                        let tilenum = ((int iy &&& 0x08) <<< 1) + (iy &&& 0x07) + sval
                        let tlow = tilenum &&& 0x03ff

                        let mutable is = 0
                        let mutable ia = 0

                        if (attr &&& 0x40) = 0x00 then
                            is <- 0
                            ia <- 1
                        else
                            is <- 7
                            ia <- -1

                        let ptnidxl = vram[tilenum >>> 10, *][tlow]
                        let ptnidxr = vram[tilenum >>> 10, *][tlow + 8]
                        let ptn = spbit_pattern[int ptnidxl, int ptnidxr, *]

                        while x < ex do
                            let tptn = int ptn[is]

                            if (tptn <> 0x00) && (sp_line_buffer[x] = 256) then
                                sp_line_buffer[x] <- i

                                if x >= spclip
                                   && (bgsp || bg_line_buffer[x] = byte 0x10) then
                                    bg_line_buffer[x] <- byte (tptn ||| attribute)

                            x <- x + 1
                            is <- is + ia

                i <- i + 4

            if 8 <= count then
                regs[0x02] <- regs[0x02] ||| byte 0x20
            else
                regs[0x02] <- regs[0x02] &&& byte 0xdf



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

    member this.set_img_data(a: byte, b: byte, c: byte) =
        imgdata[imgidx] <- a
        imgdata[imgidx + 1] <- b
        imgdata[imgidx + 2] <- c
        imgidx <- imgidx + 3

    member this.clear_img() =
        imgidx <- 0
        imgok <- false

    member this.get_img_status() =
        if imgok then
            true
        else
            false

    member this.get_image_data() =
        imgdata

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

    member this.write_ppu_ctrl1_reg(value: byte) = regs[0x01] <- value

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

