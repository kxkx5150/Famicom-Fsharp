module MEM

open Mapper0
open Dma

type t =
    { mapper: Mapper0
      dma: Dma
      ram: int array }

let makeRam mapper =

    { mapper = mapper
      dma = new Dma()
      ram = Array.zeroCreate 0x0800 }

let load m addr =
    match (addr &&& 0xe000) with
    | 0x0000 -> m.ram[addr &&& 0x7ff]
    | 0x2000 ->
        match (addr &&& 0x07) with
        | 0x0002 -> int (m.mapper.ppu.read_ppu_status_reg (byte addr))
        | 0x0007 -> int (m.mapper.ppu.read_ppu_data_reg (byte addr))
        | _ -> 0
    | 0x4000 -> 0
    | 0x6000 -> 0
    | 0x8000 -> int m.mapper.nesrom.roms[0, (addr &&& 0x1fff)]
    | 0xa000 -> int m.mapper.nesrom.roms[1, (addr &&& 0x1fff)]
    | 0xc000 -> int m.mapper.nesrom.roms[2, (addr &&& 0x1fff)]
    | 0xe000 -> int m.mapper.nesrom.roms[3, (addr &&& 0x1fff)]
    | _ -> 0

let store m addr data =
    match (addr &&& 0xe000) with
    | 0x0000 -> m.ram.[addr &&& 0x7FF] <- data
    | 0x2000 ->
        match (addr &&& 0x07) with
        | 0x00 -> m.mapper.ppu.write_ppu_ctrl0_reg (byte data)
        | 0x01 -> m.mapper.ppu.write_ppu_ctrl1_reg (byte data)
        | 0x02 -> ()
        | 0x03 -> m.mapper.ppu.write_sprite_addr_reg (byte data)
        | 0x04 -> m.mapper.ppu.write_sprite_data (byte data)
        | 0x05 -> m.mapper.ppu.write_scroll_reg (byte data)
        | 0x06 -> m.mapper.ppu.write_ppu_addr_reg (byte data)
        | 0x07 -> m.mapper.ppu.write_ppu_data_reg (byte data)
        | _ -> ()
    | 0x4000 ->
        match (addr) with
        // | 0x4014 -> m.dma.run (data, m.ram, m.mapper.ppu)
        | _ -> ()
    | 0x6000 -> ()
    | 0x8000 -> m.mapper.write (0, (addr &&& 0x1fff))
    | 0xa000 -> m.mapper.write (1, (addr &&& 0x1fff))
    | 0xc000 -> m.mapper.write (2, (addr &&& 0x1fff))
    | 0xe000 -> m.mapper.write (3, (addr &&& 0x1fff))
    | _ -> ()
