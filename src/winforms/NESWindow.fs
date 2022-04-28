module NESWindow

open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
open System.IO
open NES

type NESWindow() as this =
    inherit Form()
    do this.ClientSize <- Size(256, 240)
    let nes = new Nes()
    let _ = nes.setRom "nestest.nes"

    member this.mainLoop = 
        printfn "start window loop"
        // nes.runNesTest

    override this.OnFormClosing args = base.OnFormClosing args
    override this.OnShown args = base.OnShown args
    member this.ShowError(ex: exn) =
        printfn "error %s\n" Resource.title


