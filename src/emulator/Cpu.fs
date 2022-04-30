module CPU

open Irq
open MEM
open Instructions

module Flags =
    let carry = 0b0000_0001
    let zero = 0b0000_0010
    let interrupt = 0b0000_0100
    let decimal = 0b0000_1000
    let break4 = 0b0001_0000
    let break5 = 0b0010_0000
    let overflow = 0b0100_0000
    let negative = 0b1000_0000

type t =
    { mutable a: int
      mutable x: int
      mutable y: int
      mutable pc: int
      mutable s: int

      mutable carry: bool
      mutable zero: bool
      mutable interrupt: bool
      mutable decimal: bool
      mutable overflow: bool
      mutable negative: bool

      mutable cycles: int
      mutable extra_cycles: int
      mutable steps: int

      mutable nestest: bool
      mutable tracing: bool

      memory: MEM.t }

type instr =
    { op: Instructions.t
      mode: AddressingMode.t
      args: int
      target: int option
      cycles: int
      size: int }

let make nestest tracing memory =
    { cycles = 0
      extra_cycles = 0
      a = 0
      x = 0
      y = 0
      s = 0xFD
      pc = 0
      zero = false
      negative = false
      carry = false
      decimal = false
      interrupt = true
      overflow = false
      steps = -1
      tracing = tracing
      nestest = nestest
      memory = memory }

let wrapping_add a b = (a + b) &&& 0xFF
let wrapping_sub a b = wrapping_add a (-b)
let wrapping_add_w a b = (a + b) &&& 0xFFFF
let wrapping_sub_w a b = wrapping_add_w a (-b)

let load_byte cpu address = MEM.load cpu.memory address

let store_byte cpu address value =
    if address = 0x4014 then (* Extra cycles for DMA *)
        cpu.extra_cycles <-
            cpu.extra_cycles
            + 513
            + System.Convert.ToInt32(cpu.cycles % 2 = 1)

    MEM.store cpu.memory address value

let load_word cpu address =
    let a = load_byte cpu address in
    let b = load_byte cpu (address + 1) in
    a ||| (b <<< 8)

let load_word_zero_page cpu address =
    let a =
        (address &&& 0xFF00)
        ||| (wrapping_add (address &&& 0xFF) 1) in

    let lo = load_byte cpu address in
    let hi = load_byte cpu a in
    lo ||| (hi <<< 8)

let store_word cpu address value =
    let lo = value &&& 0xFF in
    let hi = (value >>> 8) &&& 0xFF in
    store_byte cpu address lo
    store_byte cpu (address + 1) hi

let push_byte cpu value =
    store_byte cpu (0x100 + cpu.s) value
    cpu.s <- wrapping_sub cpu.s 1

let push_word cpu value =
    push_byte cpu ((value &&& 0xFF00) >>> 8)
    push_byte cpu (value &&& 0xFF)

let pop_byte cpu =
    cpu.s <- wrapping_add cpu.s 1
    load_byte cpu (0x100 + cpu.s)

let pop_word cpu =
    let lo = pop_byte cpu in
    let hi = pop_byte cpu in
    (hi <<< 8) ||| lo

let flags_to_int cpu =
    let mutable p = Flags.break5 in

    if cpu.negative then
        p <- p ||| Flags.negative

    if cpu.overflow then
        p <- p ||| Flags.overflow

    if cpu.decimal then
        p <- p ||| Flags.decimal

    if cpu.interrupt then
        p <- p ||| Flags.interrupt

    if cpu.zero then p <- p ||| Flags.zero

    if cpu.carry then p <- p ||| Flags.carry

    p

let page_crossed a b = (a &&& 0xFF00) <> (b &&& 0xFF00)

let do_read =
    function
    | STA
    | STX
    | STY -> false
    | _ -> true

let write_target cpu target value =
    match target with
    | None -> cpu.a <- value
    | Some addr -> store_byte cpu addr value

let set_nz_flags cpu value =
    cpu.zero <- value = 0
    cpu.negative <- (value &&& 0x80) <> 0
    value

let branch cpu addr cond =
    if cond then
        let cycles =
            if page_crossed addr cpu.pc then
                2
            else
                1 in

        cpu.extra_cycles <- cpu.extra_cycles + cycles
        cpu.pc <- addr

let compare_op cpu a b =
    let result = wrapping_sub a b in
    cpu.carry <- a >= b
    cpu.zero <- a = b
    cpu.negative <- result > 127

let nmi cpu (irq: Irq) =
    push_word cpu cpu.pc
    let flags = flags_to_int cpu in
    push_byte cpu flags
    irq.nmi <- false
    cpu.cycles <- cpu.cycles + 7
    cpu.interrupt <- true
    cpu.pc <- load_word cpu 0xFFFA

let get_addr cpu am extra_page_cycles =
    let pc = cpu.pc + 1

    match am with
    | AddressingMode.Immediate -> (lazy (load_byte cpu pc), Some pc, 1)

    | AddressingMode.Absolute ->
        let address = load_word cpu pc
        (lazy (load_byte cpu address), Some address, 2)

    | AddressingMode.AbsoluteX ->
        let arg = load_word cpu pc in
        let address = wrapping_add_w arg cpu.x in

        if page_crossed arg address then
            cpu.extra_cycles <- cpu.extra_cycles + extra_page_cycles

        (lazy (load_byte cpu address), Some address, 2)
    | AddressingMode.AbsoluteY ->
        let arg = load_word cpu pc in
        let address = wrapping_add_w arg cpu.y in

        if page_crossed arg address then
            cpu.extra_cycles <- cpu.extra_cycles + extra_page_cycles

        (lazy (load_byte cpu address), Some address, 2)
    | AddressingMode.ZeroPage -> let address = load_byte cpu pc in (lazy (load_byte cpu address), Some address, 1)
    | AddressingMode.ZeroPageX ->
        let address = wrapping_add (load_byte cpu pc) cpu.x in (lazy (load_byte cpu address), Some address, 1)
    | AddressingMode.ZeroPageY ->
        let address = wrapping_add (load_byte cpu pc) cpu.y in (lazy (load_byte cpu address), Some address, 1)
    | AddressingMode.Relative ->
        let offset =
            lazy
                (let byte = load_byte cpu pc in

                 if byte < 0x80 then
                     pc + 1 + byte
                 else
                     pc + 1 + byte - 0x100) in (offset, Some pc, 1)
    | AddressingMode.IndirectX ->
        let zero_page = wrapping_add (load_byte cpu pc) cpu.x in
        let address = load_word_zero_page cpu zero_page in
        (lazy (load_byte cpu address), Some(address), 1)
    | AddressingMode.IndirectY ->
        let zero_page = load_byte cpu pc in
        let word = load_word_zero_page cpu zero_page in
        let address = wrapping_add_w word cpu.y in

        if page_crossed word address then
            cpu.extra_cycles <- cpu.extra_cycles + extra_page_cycles

        (lazy (load_byte cpu address), Some(address), 1)
    | AddressingMode.Indirect ->
        let zero_page = load_word cpu pc in
        let dest = load_word_zero_page cpu zero_page in
        (lazy (load_word cpu dest), Some(dest), 2)
    | AddressingMode.Accumulator -> (lazy (cpu.a), None, 0)
    | AddressingMode.Implicit -> (lazy 0, None, 0)

let exe_instruction cpu instruction =
    let args = instruction.args in
    let target = instruction.target in

    match instruction.op with
    | ADC ->
        let sum = cpu.a + args + System.Convert.ToInt32(cpu.carry) in
        cpu.carry <- sum > 0xFF
        (* Oh boy... *)
        cpu.overflow <- (~~~(cpu.a ^^^ args)) &&& (cpu.a ^^^ sum) &&& 0x80 > 0
        cpu.a <- set_nz_flags cpu (sum % 0x100)
    | AND -> cpu.a <- set_nz_flags cpu (cpu.a &&& args)
    | ASL ->
        cpu.carry <- args &&& 0x80 > 0
        let result = set_nz_flags cpu ((args <<< 1) &&& 0xFF) in
        write_target cpu target result
    | BCS -> branch cpu args cpu.carry
    | BCC -> branch cpu args (not cpu.carry)
    | BEQ -> branch cpu args cpu.zero
    | BIT ->
        cpu.zero <- (cpu.a &&& args) = 0
        cpu.overflow <- args &&& Flags.overflow <> 0
        cpu.negative <- args &&& Flags.negative <> 0
    | BMI -> branch cpu args cpu.negative
    | BNE -> branch cpu args (not cpu.zero)
    | BPL -> branch cpu args (not cpu.negative)
    | BRK ->
        push_word cpu (cpu.pc + 1)
        push_byte cpu ((flags_to_int cpu) ||| Flags.break4)
        cpu.interrupt <- true
        cpu.pc <- load_word cpu 0xFFFE
    | BVS -> branch cpu args cpu.overflow
    | BVC -> branch cpu args (not cpu.overflow)
    | CLC -> cpu.carry <- false
    | CLD -> cpu.decimal <- false
    | CLI -> cpu.interrupt <- false
    | CLV -> cpu.overflow <- false
    | CMP -> compare_op cpu cpu.a args
    | CPX -> compare_op cpu cpu.x args
    | CPY -> compare_op cpu cpu.y args
    | DEC -> let result = wrapping_sub args 1 in write_target cpu target (set_nz_flags cpu result)
    | DEX -> cpu.x <- set_nz_flags cpu (wrapping_sub cpu.x 1)
    | DEY -> cpu.y <- set_nz_flags cpu (wrapping_sub cpu.y 1)
    | EOR -> cpu.a <- set_nz_flags cpu (cpu.a ^^^ args)
    | INC -> let result = wrapping_add args 1 in write_target cpu target (set_nz_flags cpu result)
    | INX -> cpu.x <- set_nz_flags cpu (wrapping_add cpu.x 1)
    | INY -> cpu.y <- set_nz_flags cpu (wrapping_add cpu.y 1)
    | JMP -> cpu.pc <- target.Value
    | JSR ->
        push_word cpu (wrapping_sub_w cpu.pc 1)
        cpu.pc <- target.Value
    | LDA -> cpu.a <- set_nz_flags cpu args
    | LDX -> cpu.x <- set_nz_flags cpu args
    | LDY -> cpu.y <- set_nz_flags cpu args
    | LSR ->
        cpu.carry <- args &&& 1 > 0
        let result = set_nz_flags cpu (args >>> 1) in
        write_target cpu target result

    | NOP -> ()
    | ORA -> cpu.a <- set_nz_flags cpu (cpu.a ||| args)
    | PHA -> push_byte cpu cpu.a
    | PHP -> let flags = (flags_to_int cpu) ||| Flags.break4 in push_byte cpu flags
    | PLA -> cpu.a <- set_nz_flags cpu (pop_byte cpu)
    | PLP ->
        let flags = pop_byte cpu in
        cpu.carry <- flags &&& Flags.carry > 0
        cpu.zero <- flags &&& Flags.zero > 0
        cpu.interrupt <- flags &&& Flags.interrupt > 0
        cpu.decimal <- flags &&& Flags.decimal > 0
        cpu.overflow <- flags &&& Flags.overflow > 0
        cpu.negative <- flags &&& Flags.negative > 0
    | ROL ->
        let carry = if cpu.carry then 1 else 0 in
        cpu.carry <- args &&& 0x80 > 0
        let result = ((args <<< 1) ||| carry) &&& 0xFF in
        write_target cpu target (set_nz_flags cpu result)

    | ROR ->
        let carry = if cpu.carry then 1 else 0 in
        cpu.carry <- args &&& 1 = 1
        let result = (args >>> 1) ||| (carry <<< 7) in
        write_target cpu target (set_nz_flags cpu result)

    | RTI ->
        let flags = pop_byte cpu in
        cpu.carry <- flags &&& Flags.carry > 0
        cpu.zero <- flags &&& Flags.zero > 0
        cpu.interrupt <- flags &&& Flags.interrupt > 0
        cpu.decimal <- flags &&& Flags.decimal > 0
        cpu.overflow <- flags &&& Flags.overflow > 0
        cpu.negative <- flags &&& Flags.negative > 0
        cpu.pc <- pop_word cpu
    | RTS -> cpu.pc <- (pop_word cpu) + 1
    | SBC ->
        let args2 = (args ^^^ 0xFF)
        let sum = cpu.a + args2 + System.Convert.ToInt32(cpu.carry) in
        cpu.carry <- sum > 0xFF

        cpu.overflow <-
            (~~~(cpu.a ^^^ args2))
            &&& (cpu.a ^^^ sum)
            &&& 0x80 > 0

        cpu.a <- set_nz_flags cpu (sum % 0x100)
    | SEC -> cpu.carry <- true
    | SED -> cpu.decimal <- true
    | SEI -> cpu.interrupt <- true
    | STA -> store_byte cpu target.Value cpu.a
    | STX -> store_byte cpu target.Value cpu.x
    | STY -> store_byte cpu target.Value cpu.y
    | TAX -> cpu.x <- set_nz_flags cpu cpu.a
    | TAY -> cpu.y <- set_nz_flags cpu cpu.a
    | TSX -> cpu.x <- set_nz_flags cpu cpu.s
    | TXA -> cpu.a <- set_nz_flags cpu cpu.x
    | TXS -> cpu.s <- cpu.x
    | TYA -> cpu.a <- set_nz_flags cpu cpu.y
    // undocumented opcodes
    | LAX ->
        cpu.a <- args
        cpu.x <- set_nz_flags cpu cpu.a

    | SAX -> store_byte cpu target.Value (cpu.a &&& cpu.x)
    | DCP ->
        let mutable dat = if args = 0 then 0xff else args - 1
        let value = (dat &&& 0xff)
        store_byte cpu target.Value value
        let value2 = (value ^^^ 0xff)
        let result = cpu.a + value2 + 1
        cpu.carry <- (result > 0xff)
        let _ = set_nz_flags cpu (result &&& 0xff)
        ()
    | ISB ->
        let mutable dat = if args = 0xff then 0 else args + 1
        let value = (dat &&& 0xff)
        store_byte cpu target.Value value
        let value2 = (value ^^^ 0xff)
        let result = cpu.a + value2 + if cpu.carry then 1 else 0
        cpu.carry <- (result > 0xff)

        cpu.overflow <-
            ((cpu.a &&& 0x80) = (value2 &&& 0x80))
            && ((value2 &&& 0x80) <> ((result &&& 0x80)))

        cpu.a <- (result &&& 0xff)
        let _ = set_nz_flags cpu (cpu.a &&& 0xff)
        ()
    | SLO ->
        let res = (args <<< 1)
        cpu.carry <- (res > 0xff)
        let result = (res &&& 0xff)
        store_byte cpu target.Value result
        cpu.a <- (cpu.a ||| result)
        let _ = set_nz_flags cpu (cpu.a &&& 0xff)
        ()
    | RLA ->
        let res = ((args <<< 1) ||| (if cpu.carry then 1 else 0))
        cpu.carry <- (res > 0xff)
        let result = (res &&& 0xff)
        store_byte cpu target.Value result
        cpu.a <- (cpu.a &&& result)
        let _ = set_nz_flags cpu (cpu.a &&& 0xff)
        ()
    | SRE ->
        let carry = args &&& 0x1
        let result = args >>> 1
        cpu.carry <- carry > 0
        store_byte cpu target.Value result
        cpu.a <- cpu.a ^^^ result
        let _ = set_nz_flags cpu (cpu.a &&& 0xff)
        ()

    | RRA ->
        let carry = args &&& 0x1

        let result =
            (args >>> 1)
            ||| ((if cpu.carry then 1 else 0) <<< 7)

        store_byte cpu target.Value result
        let data = cpu.a + result + carry
        cpu.carry <- (data > 0xff)

        cpu.overflow <-
            ((cpu.a &&& 0x80) = (result &&& 0x80))
            && ((result &&& 0x80) <> ((result &&& 0x80)))

        cpu.a <- (data &&& 0xff)
        let _ = set_nz_flags cpu (cpu.a &&& 0xff)
        ()

let decode_instruction cpu instruction =
    let (op, mode, cycles, extra_page_cycles) = decode instruction
    let (laz_args, target, size) = get_addr cpu mode extra_page_cycles

    let args =
        if do_read op || cpu.nestest then
            laz_args.Force()
        else
            0x1337

    { op = op
      mode = mode
      cycles = cycles
      args = args
      target = target
      size = size + 1 }


let init (cpu: byref<t>) =
    if cpu.nestest then
        printfn "init cpu test"
        cpu.pc <- 0xC000
        &cpu
    else
        printfn "init cpu"
        let data = load_word cpu 0xfffc
        cpu.pc <- data
        &cpu

let stepCpu (cpu: byref<t>) (irq: Irq) trace =
    if irq.nmi then nmi cpu irq
    let opcode = load_byte cpu cpu.pc
    let instruction = decode_instruction cpu opcode

    if cpu.tracing then
        trace cpu instruction opcode

    cpu.pc <- cpu.pc + instruction.size
    exe_instruction cpu instruction
    cpu.cycles <- cpu.cycles + instruction.cycles + cpu.extra_cycles
    cpu.extra_cycles <- 0
    cpu.steps <- cpu.steps + 1
