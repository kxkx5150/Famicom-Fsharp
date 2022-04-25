module MEM

open Mapper

type t = {
  mapper: Mapper.t;
  ram : int array;
}

let makeRam mapper =
  {
    mapper = mapper;
    ram = Array.zeroCreate 0x0800
  }

let load m address =
  if address < 0x2000 then
    m.ram.[int address &&& 0x7FF]
    
  else if address < 0x6000 then
    0
  else
    int (m.mapper.load address)

let store m address value =
  if address < 0x2000 then
    m.ram.[int address &&& 0x7FF] <- value

  else if address < 0x6000 then
    ()
  else
    m.mapper.store address (byte value)
