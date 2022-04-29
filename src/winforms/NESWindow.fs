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
    let Loop =
        async {
            while true do
                nes.runNes
        }

    override this.OnFormClosing args = base.OnFormClosing args
    override this.OnShown args = 
        base.OnShown args
        this.BackColor <- Color.Black
        nes.initNes
        Async.Start(Loop)

    member this.ShowError(ex: exn) = printfn "error %s\n" Resource.title
