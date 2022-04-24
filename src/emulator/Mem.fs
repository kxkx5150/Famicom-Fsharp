module MEM

type t = {
  ram : int array;
}

let make  =
  {
    ram = Array.zeroCreate 0x0800
  }

let load m address =
  if address < 0x2000 then
    m.ram.[int address &&& 0x7FF]
  else if address < 0x6000 then
    0
  else
    0
    // m.mapper.load address

let store m address value =
  if address < 0x2000 then
    m.ram.[int address &&& 0x7FF] <- value
  else if address < 0x6000 then
    ()
  else
    ()
    // m.mapper.store address value
