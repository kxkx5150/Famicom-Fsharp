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
    let mutable imgdata: byte array = Array.zeroCreate<byte> (256 * 240 * 3)

    let Loop =
        async {
            while true do
                let (imgflg, imgd) = nes.runNes
                imgdata <- imgd
                if imgflg then
                    this.Invalidate()
                    nes.clearImg

        }
    member this.Draw  (args: PaintEventArgs) =
        for y in [ 0 .. 240 - 1 ] do
            for x in [ 0 .. 256 - 1 ] do
                let  aaaa = new SolidBrush(Color.FromArgb(y, 0, x))
                args.Graphics.FillRectangle(
                    aaaa,
                    x ,
                    y ,
                    1,
                    1
                )

    override this.OnFormClosing args = base.OnFormClosing args
    override this.OnShown args = 
        base.OnShown args
        this.BackColor <- Color.Black
        nes.initNes
        Async.Start(Loop)

    member this.ShowError(ex: exn) = printfn "error %s\n" Resource.title
