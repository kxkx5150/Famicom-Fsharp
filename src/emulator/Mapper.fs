module Mapper

// open ROM

// type t =
//     { rom: ROM.RomComponentsom
//       load: int -> byte
//       store: int -> byte -> unit }

// module NRom =
//     let load (rom: ROM.RomComponentsom) address =
//         if address < 0x2000 then
//             rom.chr.[address]
//         else if address >= 0x6000 && address < 0x8000 then
//             rom.ram.[address]
//         else
//             rom.prg.[address &&& (rom.headers.prg_size - 1)]

//     let store (rom: ROM.RomComponentsom) address value =
//         if address < 0x2000 then
//             rom.chr.[address] <- value
//         else if address >= 0x6000 && address < 0x8000 then
//             rom.ram.[address - 0x6000] <- value
//         else
//             failwith (sprintf "Can't store to PRG @ %04X = %02X" address value)

//     let make (rom: ROM.RomComponentsom) =
//         { rom = rom
//           load = load rom
//           store = store rom }


// let mapper_for (rom: ROM.RomComponentsom) =
//     match rom.headers.mapper with
//     | 0 -> NRom.make rom
//     | n -> failwith (sprintf "Unknwown mapper: %d" n)
