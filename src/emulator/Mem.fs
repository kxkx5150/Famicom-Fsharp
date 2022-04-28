module MEM

open Mapper0

type t = { mapper: Mapper0; ram: int array }

let makeRam mapper =
    { mapper = mapper
      ram = Array.zeroCreate 0x0800 }

let load m addr =
    match (addr &&& 0xe000) with
    | 0x0000 -> m.ram[addr &&& 0x7ff]
    | 0x2000 -> 0
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
    | 0x2000 -> ()
    | 0x4000 -> ()
    | 0x6000 -> ()
    | 0x8000 -> m.mapper.write (0, (addr &&& 0x1fff))
    | 0xa000 -> m.mapper.write (1, (addr &&& 0x1fff))
    | 0xc000 -> m.mapper.write (2, (addr &&& 0x1fff))
    | 0xe000 -> m.mapper.write (3, (addr &&& 0x1fff))
    | _ -> ()
