open System
open SDL
open System.Runtime.InteropServices
open NES

let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])
[<EntryPoint>]
let main argv =
    let mutable lastTick = 0
    let mutable frameRate = 0

    let nes = new Nes("sm.nes")
    nes.initNes
    let (width, height) = 256, 240

    SDL_Init(SDL_INIT_VIDEO) |> ignore
    let mutable window, renderer = IntPtr.Zero, IntPtr.Zero
    let windowFlags = SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS
    SDL_CreateWindowAndRenderer(width, height, windowFlags, &window, &renderer) |> ignore
    
    let texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGB24, SDL_TEXTUREACCESS_STREAMING, width, height)
    let frameBuffer = Array.create (width * height) (asUint32 (255uy, 255uy, 255uy))
    
    let bufferPtr = IntPtr ((Marshal.UnsafeAddrOfPinnedArrayElement (frameBuffer, 0)).ToPointer ())
    let mutable keyEvent = Unchecked.defaultof<SDL_KeyboardEvent>

    let rec drawLoop () =
        let imgflg = nes.runNes
        if imgflg then
            let rgbary = nes.get_img_data
            nes.clearImg
            // let mutable i = 0
            // while i < 256*240-1 do
            //     frameBuffer[i] <- (asUint32(rgbary[i], rgbary[i+1],rgbary[i+2]))
            //     i <- i + 2
            SDL_UpdateTexture(texture, IntPtr.Zero, bufferPtr, width * 4) |> ignore
            SDL_RenderClear(renderer) |> ignore
            SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero) |> ignore
            SDL_RenderPresent(renderer) |> ignore
            frameRate<- frameRate+1

        if (System.Environment.TickCount - lastTick) >= 1000 then
            printfn "%d" frameRate
            frameRate <- 0
            lastTick <- System.Environment.TickCount





        if (keyEvent.``type`` = SDL_QUIT) then
            ()
        else if SDL_PollEvent(&keyEvent) = 0 || (keyEvent.``type`` <> SDL_KEYDOWN && keyEvent.``type`` <> SDL_KEYUP) then
            drawLoop()
        else if keyEvent.keysym.sym = SDLK_ESCAPE  || keyEvent.keysym.sym = SDLK_BACKSPACE then 
            // quit the game by exiting the loop
            ()
        else
            drawLoop()

    drawLoop()
    SDL_DestroyTexture(texture)
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(window)
    SDL_Quit()
    0
