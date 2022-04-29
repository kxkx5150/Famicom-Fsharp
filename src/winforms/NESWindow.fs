module NESWindow

open System.Windows.Forms
open System.Drawing
open NES

type NESWindow() as this =
    inherit Form()
    do this.ClientSize <- Size(256, 224)
    let nes = new Nes()
    let bmp = new Bitmap(256, 224);
    let graphic = this.CreateGraphics();

    let Loop =
        async {
            while true do
                let (imgflg, imgd) = nes.runNes
                if imgflg then
                    let mutable i = 0;
                    for y in [ 0 .. 224 - 1 ] do
                        for x in [ 0 .. 256 - 1 ] do
                            bmp.SetPixel(x, y, Color.FromArgb(int imgd[i], int imgd[i+1], int imgd[i+2]))
                            i <- i + 3
                    graphic.DrawImageUnscaled(bmp, 0, 0);
                    nes.clearImg
        }
    override this.OnFormClosing args = base.OnFormClosing args
    override this.OnShown args = 
        base.OnShown args
        this.BackColor <- Color.Black
        nes.initNes
        Async.Start(Loop)

    member this.ShowError(ex: exn) = printfn "error %s\n" Resource.title
