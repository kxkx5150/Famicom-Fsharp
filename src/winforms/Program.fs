open System
// open System.Windows.Forms
// open System.Threading
// open System.IO
// open System.Text
// open System.Drawing
open SDL
open System.Runtime.InteropServices
open NES
let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])

[<EntryPoint>]
// [<STAThread>]
let main argv =
    let nes = new Nes("sm.nes")
    nes.initNes
    let (width, height) = 256, 240

    SDL_Init(SDL_INIT_VIDEO) |> ignore
    let mutable window, renderer = IntPtr.Zero, IntPtr.Zero
    let windowFlags = SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS
    SDL_CreateWindowAndRenderer(width, height, windowFlags, &window, &renderer) |> ignore
    
    let texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height)
    let frameBuffer = Array.create (width * height) (asUint32 (0uy, 0uy, 0uy))
    let bufferPtr = IntPtr ((Marshal.UnsafeAddrOfPinnedArrayElement (frameBuffer, 0)).ToPointer ())
    // let mutable keyEvent = Unchecked.defaultof<SDL_KeyboardEvent>
    // let mutable lastTicks = SDL_GetTicks ()

    let rec drawLoop () =
        let imgflg = nes.runNes
        if imgflg then
            let rgbary = nes.get_img_data
            nes.clearImg
            let mutable i = 0
            while i < 256*224 do
                frameBuffer[i] <- (asUint32(rgbary[i+2], rgbary[i+1],rgbary[i]))
                i <- i + 3

            // let bufferPtr = IntPtr ((Marshal.UnsafeAddrOfPinnedArrayElement (fb, 0)).ToPointer ())
            SDL_UpdateTexture(texture, IntPtr.Zero, bufferPtr, 256 * 4) |> ignore
            SDL_RenderClear(renderer) |> ignore
            SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero) |> ignore
            SDL_RenderPresent(renderer) |> ignore
        drawLoop()
    drawLoop()

    SDL_DestroyTexture(texture)
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(window)
    SDL_Quit()
    0