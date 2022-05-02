module Irq

type Irq() =

    member val nmi = false with get, set
    member val irq = false with get, set

    member this.init() =
        this.clear()

    member this.check_interrupt() =
        if this.nmi then
            "nmi"
        elif this.irq then
            "irq"
        else
            ""

    member this.clear_irq() =
        this.irq < false

    member this.clear_nmi() =
        this.nmi < false

    member this.clear() =
        this.nmi <- false
        this.irq < false


