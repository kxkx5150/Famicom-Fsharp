open System
open System.Windows.Forms
open System.Threading
open System.IO
open System.Text
open System.Drawing
open NES

type DoubleBufferForm() =
    inherit Form()
    do
        ``base``.SetStyle(
            ControlStyles.AllPaintingInWmPaint
            ||| ControlStyles.UserPaint
            ||| ControlStyles.DoubleBuffer,
            true
        )

[<EntryPoint>]
[<STAThread>]
let main argv =
    let form = new DoubleBufferForm()
    let SCREEN_WIDTH = 256
    let SCREEN_HEIGHT = 224
    let SCALE = 2
    let screenBuffer = Array.create (SCREEN_WIDTH * SCREEN_HEIGHT) 0
    form.ClientSize <- new System.Drawing.Size(SCREEN_WIDTH*SCALE, SCREEN_HEIGHT*SCALE)
    let mutable drawflg = false
    let nes = new Nes("sm.nes")
    let mutable rgbarray: byte array = Array.zeroCreate<byte> (256 * 240 * 3)
    let mutable lastTick = 0
    let mutable frameRate = 0

    let Loop =
        async {
            while true do
                let imgflg = nes.runNes
                if imgflg then
                    rgbarray <- nes.get_img_data
                    nes.clearImg
                    drawflg <- true
                    form.Invalidate()

                // frameRate<- frameRate+1
                // if (System.Environment.TickCount - lastTick) >= 1000 then
                //     printfn "%d" frameRate
                //     frameRate <- 0
                //     lastTick <- System.Environment.TickCount
        }
    let Draw (args: PaintEventArgs) =
        if drawflg then
            drawflg <- false 
            let mutable i = 0;
            for y in [ 0 .. SCREEN_HEIGHT - 1 ] do
                for x in [ 0 .. SCREEN_WIDTH - 1 ] do
                    let sb = new SolidBrush(Color.FromArgb(int rgbarray[i], int rgbarray[i+1], int rgbarray[i+2]))
                    i <- i + 3
                    args.Graphics.FillRectangle(
                        sb,
                        x * SCALE,
                        y * SCALE,
                        SCALE,
                        SCALE
                    )

    form.Load.Add (fun e ->
        form.BackColor <- Color.Black
        nes.initNes
        Async.Start(Loop))
    form.KeyDown.Add (fun e ->
        match e.KeyCode with            
        | _ -> ())
    form.KeyUp.Add (fun e ->
        match e.KeyCode with            
        | _ -> ())
    form.Paint.Add(Draw)
    form.MaximizeBox <- false
    form.FormBorderStyle <- FormBorderStyle.FixedSingle
    Application.Run(form)
    0
