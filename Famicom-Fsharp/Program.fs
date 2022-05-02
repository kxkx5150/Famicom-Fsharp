open System
open NES

[<EntryPoint>]
[<STAThread>]
let main argv =
    let nes = new Nes("sm.nes")
    nes.initNes
    nes.loop
    0
