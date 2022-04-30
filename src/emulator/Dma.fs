module Dma

open PPU

type Dma() =

    member val on = false with get, set

    member this.run(data: int, ram: int array, ppu: PPU) =
        let mutable offset = data <<< 8

        for i in 0 .. 0x100 - 1 do
            ppu.sprite_ram'[ i ] <- byte ram[offset]
            offset <- offset + 1

        this.on <- true

    member this.get_status() = this.on

    member this.clear() = this.on <- false
