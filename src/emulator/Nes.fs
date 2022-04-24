module NES

open ROM
open CPU


type NesComponents = { Rom: Rom }
type Nes() =

    let rom = new Rom()
    let rc = rom.setRom "nestest.nes"
    
    member this.romc = rc
    member this.setRom(path: string) = 
        printfn "Loaded rom %s\n" path
