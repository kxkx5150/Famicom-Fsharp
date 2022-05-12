open System
open NES

[<EntryPoint>]
[<STAThread>]
let main argv =
    let nes = new Nes("sm.nes")
    nes.start_loop
    0
