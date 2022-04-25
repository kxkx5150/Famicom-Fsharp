module NESWindow

open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
open System.IO
open NES

type NESWindow() as this =
    inherit Form()
    do this.ClientSize <- Size(256, 240)
    member this.nes = new Nes()

    member this.initNES test = 
        printfn "init nes"
        this.nes.init test

    member this.mainLoop = 
        printfn "start loop"
        this.nes.loop

    override this.OnFormClosing args = base.OnFormClosing args

    override this.OnShown args = base.OnShown args

    member this.ShowError(ex: exn) =
        printfn "error %s\n" Resource.title


