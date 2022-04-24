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

    member this.setRom(path: string) = nes.setRom path
    override this.OnFormClosing args = base.OnFormClosing args
    override this.OnShown args = base.OnShown args
    member this.ShowError(ex: exn) =
        MessageBox.Show(
            ex.Message,
            sprintf "%s encountered an unexpected error" Resource.title,
            MessageBoxButtons.OK,
            MessageBoxIcon.Error
        )
        |> ignore
