open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
open NESWindow

[<EntryPoint>]
[<STAThread>]
let main argv =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false
    use newwindow = new NESWindow()
    Application.Run(newwindow)
    0
