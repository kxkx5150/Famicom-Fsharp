module MapperBase

[<AbstractClass>]
type MapperBase() =

    member this.init() = printfn "mapperbase init"

    member this.read_low(addr: int) = 0x00

    member this.write_low(addr: int, data: int) = ()

    member this.read_ppudata() = 0x00

    member this.write_ppudata() = ()

    member this.build_bgline() = ()

    member this.build_spriteline() = ()

    member this.read_sram() = 0x00

    member this.write_sram() = ()

    member this.write(addr: int, data: int) = ()

    member this.hsync() = ()

    member this.cpusync() = ()

    member this.setirq() = ()

    member this.clearirq() = ()

    member this.out_exsound() = 0x00

    member this.exsound_sync() = ()

    member this.getstate() = ()

    member this.setstate() = ()
