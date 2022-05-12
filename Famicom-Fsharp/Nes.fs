module NES
open System
open SDL
open System.Runtime.InteropServices

open Irq
open ROM
open Mapper0
open MEM
open CPU
open NESTEST
open PPU

let trace (cpu: CPU.t) instruction opcode =
    let str_op = instruction.op.ToString()
    printfn
        "%04X %s    A:%02X X:%02X Y:%02X P:%02X SP:%02X    CYC:%4d"
        cpu.pc
        str_op
        cpu.a
        cpu.x
        cpu.y
        (CPU.flags_to_int cpu)
        cpu.s
        cpu.cycles
    if cpu.nestest then
        let s1 =
            sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" cpu.a cpu.x cpu.y (CPU.flags_to_int cpu) cpu.s
        let pcstr = sprintf "%04X" cpu.pc
        if
            not (s1.Equals(reglog[cpu.steps + 1]))
            || pcstr = ""
        then
            printfn "\n           : %04X %s" cpu.pc str_op
            printfn "OK         : %s" reglog[cpu.steps + 1]
            printfn "           : %s" s1
            failwith (sprintf "nestest error")

type Nes(path: string) =
    let irq = new Irq()
    let rom = new Rom()
    let ppu = new PPU()
    let mapper = new Mapper0(rom, ppu)
    let mem = MEM.makeRam mapper
    let mutable lcpu = CPU.make false false mem
    let _ = mem.mapper.setRom path
    let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])
    
    let mutable lastTick = 0
    let mutable frameRate = 0
    let (width, height) = 256, 240

    member this.initNes = 
        let _ = CPU.init &lcpu
        ()

    member this.runNes =
        let prev_cycles = lcpu.cycles
        CPU.stepCpu &lcpu irq trace
        let mutable cycles = lcpu.cycles - prev_cycles
        if mem.dma.get_status() then
            mem.dma.clear();
            cycles <- cycles + 514

        mem.mapper.ppu.run(cycles, irq)
        mem.mapper.ppu.get_img_status()

    member this.get_img_data =
        mem.mapper.ppu.get_image_data()

    member this.clearImg =
        mem.mapper.ppu.clear_img()


    member this.Loop (renderer, texture, frameBuffer:uint32 array, bufferPtr) = 
        async {
            while true do
                let imgflg = this.runNes
                if imgflg then
                    let rgbary = this.get_img_data
                    this.clearImg
                    // let mutable i = 0
                    // while i < 256*224-1 do
                    //     let aaaa = asUint32(rgbary[i*3], rgbary[i*3+1],rgbary[i*3+2])
                    //     frameBuffer.[i] <- aaaa
                        // i <- i + 2

                    SDL_UpdateTexture(texture, IntPtr.Zero, bufferPtr, width * 4) |> ignore
                    SDL_RenderClear(renderer) |> ignore
                    SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero) |> ignore
                    SDL_RenderPresent(renderer) |> ignore

                frameRate<- frameRate+1
                if (System.Environment.TickCount - lastTick) >= 1000 then
                    printfn "%d" frameRate
                    frameRate <- 0
                    lastTick <- System.Environment.TickCount
        }




    member this.start_loop =
        SDL_Init(SDL_INIT_VIDEO) |> ignore
        let mutable window, renderer = IntPtr.Zero, IntPtr.Zero
        let windowFlags = SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS
        SDL_CreateWindowAndRenderer(width, height, windowFlags, &window, &renderer) |> ignore
        let texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height)
        let frameBuffer = Array.create (width * height) (asUint32 (255uy, 255uy, 255uy))
        let bufferPtr = IntPtr ((Marshal.UnsafeAddrOfPinnedArrayElement (frameBuffer, 0)).ToPointer ())
        let mutable keyEvent = Unchecked.defaultof<SDL_KeyboardEvent>



        frameBuffer[0] <- uint32 0
        this.initNes
        Async.Start(this.Loop(renderer, texture, frameBuffer, bufferPtr))

        let mutable brk = false
        while not brk do
            let evt = SDL_PollEvent(&keyEvent)
            if (keyEvent.``type`` = SDL_QUIT) then
                brk <- true
            else if keyEvent.keysym.sym = SDLK_ESCAPE then 
                brk <- true

        SDL_DestroyTexture(texture)
        SDL_DestroyRenderer(renderer)
        SDL_DestroyWindow(window)
        SDL_Quit()

        
