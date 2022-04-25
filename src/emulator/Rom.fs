module ROM

open System.IO

type mirroring =
    | Vertical
    | Horizontal

type header =
    { prg_size: int
      chr_size: int
      mapper: int
      mirroring: mirroring }

type RomComponentsom =
    { headers: header
      prg: byte array
      chr: byte array
      ram: byte array }


type Rom() =
    member this.setRom(path: string) =
        let romData = path |> File.ReadAllBytes
        let headers = this.load_headers romData

        let chr =
            romData[(0x10 + headers.prg_size) .. (0x10 + headers.prg_size + headers.chr_size)]

        let prg = romData[0x10 .. (0x10 + headers.prg_size)]
        let ram: byte array = Array.zeroCreate 0x2000
        printfn "Loaded %s\n" path
        { headers = headers; prg = prg; chr = chr; ram = ram}


    member this.load_headers(rom: byte array) =
        { prg_size = int rom.[4] * 0x4000
          chr_size = int rom.[5] * 0x2000
          mapper = (int rom.[7] &&& 0xF0) ||| (int rom.[6] >>> 4)
          mirroring =
            if (int rom.[6] &&& 1) = 1 then
                Vertical
            else
                Horizontal }
