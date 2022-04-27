module CPU

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
      mutable nmi_triggered: bool

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
      nmi_triggered = false
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

let decode_addressing_mode cpu am extra_page_cycles =
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

let write_target cpu target value =
    match target with
    | None -> cpu.a <- value
    | Some addr -> store_byte cpu addr value

let set_nz_flags cpu value =
    cpu.zero <- value = 0
    cpu.negative <- (value &&& 0x80) <> 0
    value






let tax c = c.x <- set_nz_flags c c.a
let txa c = c.a <- set_nz_flags c c.x
let tay c = c.y <- set_nz_flags c c.a
let tya c = c.a <- set_nz_flags c c.y

let txs c =
    c.s <- c.x (* Not a bug. TXS is the only transfer that doesn't change NZ *)

let tsx c = c.x <- set_nz_flags c c.s

let inc cpu args target =
    let result = wrapping_add args 1 in write_target cpu target (set_nz_flags cpu result)

let dec cpu args target =
    let result = wrapping_sub args 1 in write_target cpu target (set_nz_flags cpu result)

let dex c =
    c.x <- set_nz_flags c (wrapping_sub c.x 1)

let dey c =
    c.y <- set_nz_flags c (wrapping_sub c.y 1)

let inx c =
    c.x <- set_nz_flags c (wrapping_add c.x 1)

let iny c =
    c.y <- set_nz_flags c (wrapping_add c.y 1)

let sta c addr = store_byte c addr c.a
let stx c addr = store_byte c addr c.x
let sty c addr = store_byte c addr c.y
let lda c args = c.a <- set_nz_flags c args
let ldx c args = c.x <- set_nz_flags c args
let ldy c args = c.y <- set_nz_flags c args

let jmp cpu target = cpu.pc <- target

let brk cpu =
    push_word cpu (cpu.pc + 1)
    push_byte cpu ((flags_to_int cpu) ||| Flags.break4)
    cpu.interrupt <- true
    cpu.pc <- load_word cpu 0xFFFE

let branch cpu addr cond =
    if cond then
        let cycles =
            if page_crossed addr cpu.pc then
                2
            else
                1 in

        cpu.extra_cycles <- cpu.extra_cycles + cycles
        cpu.pc <- addr

let bcs cpu offset = branch cpu offset cpu.carry
let bcc cpu offset = branch cpu offset (not cpu.carry)
let beq cpu offset = branch cpu offset cpu.zero
let bne cpu offset = branch cpu offset (not cpu.zero)
let bmi cpu offset = branch cpu offset cpu.negative
let bpl cpu offset = branch cpu offset (not cpu.negative)
let bvs cpu offset = branch cpu offset cpu.overflow
let bvc cpu offset = branch cpu offset (not cpu.overflow)

let bit cpu byte =
    cpu.zero <- (cpu.a &&& byte) = 0
    cpu.overflow <- byte &&& Flags.overflow <> 0
    cpu.negative <- byte &&& Flags.negative <> 0

let php cpu =
    let flags = (flags_to_int cpu) ||| Flags.break4 in push_byte cpu flags

let plp cpu =
    let flags = pop_byte cpu in
    cpu.carry <- flags &&& Flags.carry > 0
    cpu.zero <- flags &&& Flags.zero > 0
    cpu.interrupt <- flags &&& Flags.interrupt > 0
    cpu.decimal <- flags &&& Flags.decimal > 0
    cpu.overflow <- flags &&& Flags.overflow > 0
    cpu.negative <- flags &&& Flags.negative > 0

let pla cpu =
    cpu.a <- set_nz_flags cpu (pop_byte cpu)

let and_op cpu args =
    cpu.a <- set_nz_flags cpu (cpu.a &&& args)

let ora cpu args =
    cpu.a <- set_nz_flags cpu (cpu.a ||| args)

let eor cpu args =
    cpu.a <- set_nz_flags cpu (cpu.a ^^^ args)

let lsr_op cpu args target =
    cpu.carry <- args &&& 1 > 0
    let result = set_nz_flags cpu (args >>> 1) in
    write_target cpu target result

let asl_op cpu args target =
    cpu.carry <- args &&& 0x80 > 0
    let result = set_nz_flags cpu ((args <<< 1) &&& 0xFF) in
    write_target cpu target result

let ror cpu args target =
    let carry = if cpu.carry then 1 else 0 in
    cpu.carry <- args &&& 1 = 1
    let result = (args >>> 1) ||| (carry <<< 7) in
    write_target cpu target (set_nz_flags cpu result)

let rol cpu args target =
    let carry = if cpu.carry then 1 else 0 in
    cpu.carry <- args &&& 0x80 > 0
    let result = ((args <<< 1) ||| carry) &&& 0xFF in
    write_target cpu target (set_nz_flags cpu result)

let compare_op cpu a b =
    let result = wrapping_sub a b in
    cpu.carry <- a >= b
    cpu.zero <- a = b
    cpu.negative <- result > 127

let cmp cpu args = compare_op cpu cpu.a args
let cpx cpu args = compare_op cpu cpu.x args
let cpy cpu args = compare_op cpu cpu.y args

let adc cpu args =
    let sum = cpu.a + args + System.Convert.ToInt32(cpu.carry) in
    cpu.carry <- sum > 0xFF
    (* Oh boy... *)
    cpu.overflow <- (~~~(cpu.a ^^^ args)) &&& (cpu.a ^^^ sum) &&& 0x80 > 0
    cpu.a <- set_nz_flags cpu (sum % 0x100)

let sbc cpu args = adc cpu (args ^^^ 0xFF)

let jsr cpu address =
    push_word cpu (wrapping_sub_w cpu.pc 1)
    cpu.pc <- address

let rts cpu = cpu.pc <- (pop_word cpu) + 1

let rti cpu =
    plp cpu
    cpu.pc <- pop_word cpu

// undocumented opcodes

let lax c args =
    c.a <- args
    c.x <- set_nz_flags c c.a

let sax c args = store_byte c args (c.a &&& c.x)

let dcp c args addr =
    let mutable dat = if args = 0 then 0xff else args - 1

    let value = (dat &&& 0xff)
    store_byte c addr value
    let value2 = (value ^^^ 0xff)

    let result = c.a + value2 + 1
    c.carry <- (result > 0xff)
    let _ = set_nz_flags c (result &&& 0xff)
    ()

let isb c args addr =
    let mutable dat = if args = 0xff then 0 else args + 1

    let value = (dat &&& 0xff)
    store_byte c addr value
    let value2 = (value ^^^ 0xff)

    let result = c.a + value2 + if c.carry then 1 else 0
    c.carry <- (result > 0xff)

    c.overflow <-
        ((c.a &&& 0x80) = (value2 &&& 0x80))
        && ((value2 &&& 0x80) <> ((result &&& 0x80)))

    c.a <- (result &&& 0xff)
    let _ = set_nz_flags c (c.a &&& 0xff)
    ()

let slo c args addr =
    let res = (args <<< 1)
    c.carry <- (res > 0xff)
    let result = (res &&& 0xff)

    store_byte c addr result
    c.a <- (c.a ||| result)
    let _ = set_nz_flags c (c.a &&& 0xff)
    ()

let rla c args addr =
    let res = ((args <<< 1) ||| (if c.carry then 1 else 0))
    c.carry <- (res > 0xff)
    let result = (res &&& 0xff)
    store_byte c addr result
    c.a <- (c.a &&& result)
    let _ = set_nz_flags c (c.a &&& 0xff)
    ()

let sre c args addr =
    let carry = args &&& 0x1
    let result = args >>> 1
    c.carry <- carry > 0
    store_byte c addr result
    c.a <- c.a ^^^ result
    let _ = set_nz_flags c (c.a &&& 0xff)
    ()

let rra c args addr =
    let carry = args &&& 0x1

    let result =
        (args >>> 1)
        ||| ((if c.carry then 1 else 0) <<< 7)

    store_byte c addr result
    let data = c.a + result + carry
    c.carry <- (data > 0xff)

    c.overflow <-
        ((c.a &&& 0x80) = (result &&& 0x80))
        && ((result &&& 0x80) <> ((result &&& 0x80)))

    c.a <- (data &&& 0xff)
    let _ = set_nz_flags c (c.a &&& 0xff)
    ()

let nmi cpu =
    push_word cpu cpu.pc
    let flags = flags_to_int cpu in
    push_byte cpu flags
    cpu.nmi_triggered <- false
    cpu.cycles <- cpu.cycles + 7
    cpu.interrupt <- true
    cpu.pc <- load_word cpu 0xFFFA


let execute_instruction cpu instruction =
    let args = instruction.args in
    let target = instruction.target in

    match instruction.op with
    | ADC -> adc cpu args
    | AND -> and_op cpu args
    | ASL -> asl_op cpu args target
    | BCS -> bcs cpu args
    | BCC -> bcc cpu args
    | BEQ -> beq cpu args
    | BIT -> bit cpu args
    | BMI -> bmi cpu args
    | BNE -> bne cpu args
    | BPL -> bpl cpu args
    | BRK -> brk cpu
    | BVS -> bvs cpu args
    | BVC -> bvc cpu args
    | CLC -> cpu.carry <- false
    | CLD -> cpu.decimal <- false
    | CLI -> cpu.interrupt <- false
    | CLV -> cpu.overflow <- false
    | CMP -> cmp cpu args
    | CPX -> cpx cpu args
    | CPY -> cpy cpu args
    | DEC -> dec cpu args target
    | DEX -> dex cpu
    | DEY -> dey cpu
    | EOR -> eor cpu args
    | INC -> inc cpu args target
    | INX -> inx cpu
    | INY -> iny cpu
    | JMP -> jmp cpu target.Value
    | JSR -> jsr cpu target.Value
    | LDA -> lda cpu args
    | LDX -> ldx cpu args
    | LDY -> ldy cpu args
    | LSR -> lsr_op cpu args target
    | NOP -> ()
    | ORA -> ora cpu args
    | PHA -> push_byte cpu cpu.a
    | PHP -> php cpu
    | PLA -> pla cpu
    | PLP -> plp cpu
    | ROL -> rol cpu args target
    | ROR -> ror cpu args target
    | RTI -> rti cpu
    | RTS -> rts cpu
    | SBC -> sbc cpu args
    | SEC -> cpu.carry <- true
    | SED -> cpu.decimal <- true
    | SEI -> cpu.interrupt <- true
    | STA -> sta cpu target.Value
    | STX -> stx cpu target.Value
    | STY -> sty cpu target.Value
    | TAX -> tax cpu
    | TAY -> tay cpu
    | TSX -> tsx cpu
    | TXA -> txa cpu
    | TXS -> txs cpu
    | TYA -> tya cpu
    // undocumented opcodes
    | LAX -> lax cpu args
    | SAX -> sax cpu target.Value
    | DCP -> dcp cpu args target.Value
    | ISB -> isb cpu args target.Value
    | SLO -> slo cpu args target.Value
    | RLA -> rla cpu args target.Value
    | SRE -> sre cpu args target.Value
    | RRA -> rra cpu args target.Value

let decode_instruction cpu instruction =
    let (op, mode, cycles, extra_page_cycles) = decode instruction

    let (laz_args, target, size) = decode_addressing_mode cpu mode extra_page_cycles

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

let initTest (cpu: byref<t>) test =
    if test then
        cpu.pc <- 0xC000
        printfn "init cpu test"
    else
        printfn "init cpu"

    &cpu

let stepCpu (cpu: byref<t>) trace =
    if cpu.nmi_triggered then nmi cpu
    let opcode = load_byte cpu cpu.pc
    let instruction = decode_instruction cpu opcode
    trace cpu instruction opcode

    cpu.pc <- cpu.pc + instruction.size
    execute_instruction cpu instruction
    cpu.cycles <- cpu.cycles + instruction.cycles + cpu.extra_cycles
    cpu.extra_cycles <- 0
    cpu.steps <- cpu.steps + 1
