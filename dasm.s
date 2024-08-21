; Permission to use, copy, modify, and/or distribute this software for
; any purpose with or without fee is hereby granted.
; 
; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
; FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
; DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
; OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

        .feature org_per_seg
        .export dasm
        .exportzp pc
        .import outchar

        .zeropage
pc:     .res    2               ; input pc, updated by dasm
opcode: .res    3               ; holds the opcode and operand bytes being processsed
pcrel:  .res    1               ; branch relative byte
tmp1:   .res    1               ; temp

        .segment "CODE"

; Disassemble a 65c02 instruction
; In:
;       pc
; Out:
;       Output via the 'outchar' function, which takes 'A' as
;       a character and preserves X,Y.
;       Line termination is a single CR character 10.
;       e.g.:
;               1234: 71 34     | adc  ($34,y)\n
dasm:
        ldx     pc              ; (A,X) = pc
        lda     pc+1
        ; output the PC followed by operand bytes and separator
        ; e.g. "1234: 71 34     | "
        jsr     out16           ; write PC to output
        lda     #':'            ; followed by a colon
        jsr     outchar
        jsr     load_objbyte    ; fetch the opcode, ++pc
        sta     opcode          ; save it
        jsr     get_am_info     ; fetch the length of the inst
        phx                     ; save operand count
        dex                     ; an operands?
        bmi     @out            ; no
        jsr     load_objbyte    ; load first operand byte
        sta     opcode+1        ; and save it
        sta     pcrel           ; it might be a BRA dest
        dex                     ; any more operands?
        bmi     @out            ; no
        jsr     load_objbyte    ; load second operand byte
        sta     opcode+2        ; and save it
        bbr0    opcode,@out
        sta     pcrel           ; it might be a BBR dest
@out:
        ply                     ; Y = operand count
        ldx     padd,y          ; X = number of pad spaces to emit
@spcloop:
        jsr     outspc          ; emit pad spaces
        dex
        bne     @spcloop
        lda     #'|'            ; emit separator followed by space
        jsr     outchar
        jsr     outspc
        ldx     opcode          ; lookup the instruction string in ins_map
        lda     ins_map,x
        bpl     @found          ; +ve values are valid
        ; illegal opcode, so emit the literal opcode byte
        ; e.g. ".byte $22"
        ldx     #5              ; emit ".byte "
@byteloop:
        lda     dotbyte,x
        jsr     outchar
        dex
        bpl     @byteloop
        lda     opcode          ; ...followed by the opcode byte
        jsr     outdollar8
        bra     outcr           ; output cr and return
@found: 
        ; output the instruction name, e.g. "lda"
        asl     a               ; string table offset is index*2
        tax
        lda     ins_str_tab,x   ; load first byte: 0aaaaabb
        pha                     ; save it
        lsr     a               ; >> 2 to give the first 5-bit character code
        lsr     a
        jsr     outench         ; emit the first character
        pla                     ; restore the whole code
        asl     a               ; << 3 to position the 'bb' bits in [4:3]
        asl     a
        asl     a
        sta     tmp1            ; save aaabb000
        lda     ins_str_tab+1,x ; load the second byte: bbbccccc
        pha                     ; save it
        jsr     lsr5            ; >> 5 to move bbb to [2:0]
        ora     tmp1            ; or in the upper bb bits
        jsr     outench         ; output (outench will mask off the junk)
        pla                     ; restore bbbccccc
        jsr     outench         ; output the ccccc character
        ; append the 0..7 trailer for rmb, smb, bbr, and bbs.
        lda     opcode          ; fetch opcode
        pha                     ; save it
        and     #$07            ; is $x7 or $xF?
        cmp     #$07
        bne     @notbb          ; no, not one of these instructions
        pla                     ; restore opcode
        jsr     lsr4            ; trailer = (opcode >> 4) & 7
        and     #$07
        jsr     out4            ; output it
        ; emit the addressing mode string (operands)
@contam:
        jsr     outspc          ; space between instruction and operands
        jsr     get_am_info     ; A = addressing mode
        tay
        ldx     amoffs,y        ; X = offset to start of string (won't be $ff)
        ; output operand string loop
        ; each character in the string is output unless it is
        ; 'b', 'w', or 'r', in which case it is interpreted as an
        ; 8-bit operand (b), 16-bit operand (w), or PC-relative operand (r)
@amloop:
        lda     am_str_tab,x    ; load character
        php                     ; save N flag
        inx                     ; advance index
        and     #$7f            ; drop bit 7 (end-of-string flag)
        phx                     ; preserve X
        jsr     outamchar       ; emit addressing mode character
        plx                     ; restore X
        plp                     ; restore N
        bpl     @amloop         ; branch if not end-of-string
        bra     outcr           ; output CR and return
@notbb:
        ; not an rmb, smb, etc instruction.
        pla                     ; drop opcode
        jsr     get_am_info     ; A = addressing mode
        cmp     #0
        beq     outcr           ; nothing to do, emit CR and return
        jsr     outspc          ; emit an extra space
        bra     @contam         ; continue to emit addresing mode string

; Fetch an object code byte from (pc),
; output the byte, preceded by a space,
; update pc,
; and return the byte in A
load_objbyte:
        jsr     outspc          ; output space
        lda     (pc)            ; fetch byte at pc
        inc     pc              ; inc pc
        bne     @skip           ; ... and account for page crossing
        inc     pc+1
@skip:
        pha                     ; save value
        jsr     out8            ; output 8-bit hex
        pla                     ; restore and return
        rts

; Output a carriage return
outcr:
        lda     #10
loutchar:                       ; loutchar: branch-local jump to outchar
        jmp     outchar

; Get addressing mode
; Out:
;       A: addressing mode
;       X: number of operand bytes
; Clobbers:
;       Y
get_am_info:
        lda     opcode          ; get opcode
        lsr     a               ; / 2
        tax                     ; into x
        lda     amtab,x         ; get addressing mode pair
        bbr0    opcode,@skip1   ; bits [3:0] used for even opcodes
        jsr     lsr4            ; shift bits [7:4] down for odd opcodes
@skip1:
        and     #$f             ; mask off unwanted bits; A = addressing mode
        tay                     ; into y
        ldx     amlen,y         ; X = number of operand bytes
        rts

; Emit a single encoded character
; In:
;       A encoded character
; Clobber:
;       A, X
outamchar:
        cmp     #'b'            ; character is 'b'?
        bne     @notb           ; no
        lda     opcode+1        ; is 'b', so output an 8-bit operand
        bra     outdollar8      ; ... and return
@notb:
        cmp     #'w'            ; is 'w'?
        bne     @notw           ; no
        ldx     opcode+1        ; is 'w', so output a 16-bit operand
        lda     opcode+2
        bra     outdollar16     ; ... and return
@notw:
        cmp     #'r'            ; is 'r'?
        bne     loutchar        ; no... output the character as-is, and return
        lda     pc              ; is 'r', so calculate the PC-relative address
        clc
        adc     pcrel           ; add branch dest
        sta     tmp1            ; tmp1 holds low-byte of result
        lda     pc+1
        bbs7    pcrel,@pcsub    ; adc #0 for forward, or $ff for backward
        adc     #0
        bra     @pcdone
@pcsub:
        adc     #$ff
@pcdone:
        ldx     tmp1            ; (A,X) is the 16-bit PC-rel value to output
outdollar16:                    ; output 16-bit hex (A,X), prefixed by $
        jsr     outdollar
out16:                          ; output a 16-bit hex number in (A,X)
        jsr     out8
        txa
out8:                           ; output an 8-bit hex number in A
        pha
        jsr     lsr4
        jsr     out4
        pla
out4:                           ; output a 4-bit hex number in A[3:0]
        and     #$0f
        cmp     #10
        bcc     @lt10
        adc     #'a'-'9'-2
@lt10:
        adc     #'0'
        bra     loutchar

; emit an instruction sting character
; A is the character code (1=a, 2=b, etc)
outench:
        and     #$1f
        clc
        adc     #'a'-1
        bra     loutchar

; right-shift utils
lsr5:
        lsr     a
lsr4:
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        rts

; output a $, preserving A
outdollar:
        pha
        lda     #'$'
        jsr     outchar
        pla
        rts

; output a space
outspc:
        lda     #' '
        bra     loutchar

; output an 8-bit value with a $ prefix
outdollar8:
        jsr     outdollar
        bra     out8

; addressing mode string table
; 15 entries, indexed via am_offs table
; the end of a string is indicated by bit 7 set.
; characters 'b', 'w', and 'r' are special, the rest are output as-is.
; 'b' represents an 8-bit operand.
; 'w' represents a 16-bit operand.
; 'r' represends a PC-relative 8-bit operand.
; processing done by 'outamchar'
am_str_tab:
        .byte 'w'+$80           ; w             - abs
        .byte "#",'b'+$80       ; #b    and b   - immed and zp
        .byte "(b,x",')'+$80    ; (b,x)         - zp ind x
        .byte "(b),",'y'+$80    ; (b),y         - zp ind y
        .byte "(b",')'+$80      ; (b)           - zp ind
        .byte "b,",'x'+$80      ; b,x           - zp x
        .byte "b,",'y'+$80      ; b,y           - zp y
        .byte "w,",'y'+$80      ; w,y           - abs y
        .byte 'a'+$80           ; a             - acc
        .byte "w,",'x'+$80      ; w,x           - abs x
        .byte "(w",')'+$80      ; (w)           - abs ind
        .byte "(w,x",')'+$80    ; (w,x)         - abs ind x
        .byte "b,",'r'+$80      ; b,r   and r   - zp+rel and rel

; 16-entry table indexed by addressing mode (0..15)
; provides the offset within am_name_tab to the
; start of the addressing mode string.
; $ff means "no am string" (e.g. for nop, brk, etc). 
amoffs: .byte   $ff,$27,$00,$01,$03,$08,$0d,$02
        .byte   $10,$13,$16,$19,$1a,$1d,$20,$25

; opcode to addressing mode map
; 128-byte table, indexed by (opcode / 2):
;       bits [7:4]: addressing mode for odd opcodes.
;       bits [3:0]: addressing mode for even opcodes.
amtab:
        .byte $40,$00,$77,$77,$30,$0b,$22,$f2
        .byte $51,$06,$87,$78,$a0,$0b,$c2,$fc
        .byte $42,$00,$77,$77,$30,$0b,$22,$f2
        .byte $51,$06,$88,$78,$a0,$0b,$cc,$fc
        .byte $40,$00,$70,$77,$30,$0b,$22,$f2
        .byte $51,$06,$80,$78,$a0,$00,$c0,$fc
        .byte $40,$00,$77,$77,$30,$0b,$2d,$f2
        .byte $51,$06,$88,$78,$a0,$00,$ce,$fc
        .byte $41,$00,$77,$77,$30,$00,$22,$f2
        .byte $51,$06,$88,$79,$a0,$00,$c2,$fc
        .byte $43,$03,$77,$77,$30,$00,$22,$f2
        .byte $51,$06,$88,$79,$a0,$00,$cc,$fa
        .byte $43,$00,$77,$77,$30,$00,$22,$f2
        .byte $51,$06,$80,$78,$a0,$00,$c0,$fc
        .byte $43,$00,$77,$77,$30,$00,$22,$f2
        .byte $51,$06,$80,$78,$a0,$00,$c0,$fc

; number of operand bytes per addressing mode
; simple 16-entry linear table, indexed by addressing mode 0..15.
amlen:
        .byte 0,1,2,1,1,1,1,1
        .byte 1,1,2,0,2,2,2,2

; instruction strings table
; 70 instruction; 16-bits holding 3 characters
; packed 5-bits truncated ASCII per character: 0aaaaabb_bbbccccc
; character code 1 = 'a', 2 = 'b', etc.
ins_str_tab:
        .byte $04,$83,$05,$c4,$06,$6c,$08,$52
        .byte $08,$53,$08,$63,$08,$73,$08,$b1
        .byte $09,$34,$09,$a9,$09,$c5,$0a,$0c
        .byte $0a,$41,$0a,$4b,$0a,$c3,$0a,$d3
        .byte $0d,$83,$0d,$84,$0d,$89,$0d,$96
        .byte $0d,$b0,$0e,$18,$0e,$19,$10,$a3
        .byte $10,$b8,$10,$b9,$15,$f2,$25,$c3
        .byte $25,$d8,$25,$d9,$29,$b0,$2a,$72
        .byte $30,$81,$30,$98,$30,$99,$32,$72
        .byte $39,$f0,$3e,$41,$41,$01,$41,$10
        .byte $41,$18,$41,$19,$41,$81,$41,$90
        .byte $41,$98,$41,$99,$49,$a2,$49,$ec
        .byte $49,$f2,$4a,$89,$4a,$93,$4c,$43
        .byte $4c,$a3,$4c,$a4,$4c,$a9,$4d,$a2
        .byte $4e,$81,$4e,$90,$4e,$98,$4e,$99
        .byte $4e,$9a,$50,$38,$50,$39,$52,$42
        .byte $52,$62,$52,$78,$53,$01,$53,$13
        .byte $53,$21,$5c,$29

; simple 256-entry linear map from opcode to instrution name,
; indexed by opcode.
ins_map:
        .byte $0d,$25,$ff,$ff,$40,$25,$02,$2e
        .byte $27,$25,$02,$ff,$40,$25,$02,$03
        .byte $0b,$25,$25,$ff,$3f,$25,$02,$2e
        .byte $10,$25,$1b,$ff,$3f,$25,$02,$03
        .byte $1f,$01,$ff,$ff,$08,$01,$2f,$2e
        .byte $2b,$01,$2f,$ff,$08,$01,$2f,$03
        .byte $09,$01,$01,$ff,$08,$01,$2f,$2e
        .byte $34,$01,$17,$ff,$08,$01,$2f,$03
        .byte $31,$1a,$ff,$ff,$ff,$1a,$23,$2e
        .byte $26,$1a,$23,$ff,$1e,$1a,$23,$03
        .byte $0e,$1a,$1a,$ff,$ff,$1a,$23,$2e
        .byte $12,$1a,$29,$ff,$ff,$1a,$23,$03
        .byte $32,$00,$ff,$ff,$3c,$00,$30,$2e
        .byte $2a,$00,$30,$ff,$1e,$00,$30,$03
        .byte $0f,$00,$00,$ff,$3c,$00,$30,$2e
        .byte $36,$00,$2d,$ff,$1e,$00,$30,$03
        .byte $0c,$38,$ff,$ff,$3b,$38,$3a,$37
        .byte $19,$08,$42,$ff,$3b,$38,$3a,$04
        .byte $05,$38,$38,$ff,$3b,$38,$3a,$37
        .byte $44,$38,$43,$ff,$3c,$38,$3c,$04
        .byte $22,$20,$21,$ff,$22,$20,$21,$37
        .byte $3e,$20,$3d,$ff,$22,$20,$21,$04
        .byte $06,$20,$20,$ff,$22,$20,$21,$37
        .byte $13,$20,$41,$ff,$22,$20,$21,$04
        .byte $16,$14,$ff,$ff,$16,$14,$17,$37
        .byte $1d,$14,$18,$45,$16,$14,$17,$04
        .byte $0a,$14,$14,$ff,$ff,$14,$17,$37
        .byte $11,$14,$28,$39,$ff,$14,$17,$04
        .byte $15,$33,$ff,$ff,$15,$33,$1b,$37
        .byte $1c,$33,$24,$ff,$15,$33,$1b,$04
        .byte $07,$33,$33,$ff,$ff,$33,$1b,$37
        .byte $35,$33,$2c,$ff,$ff,$33,$1b,$04

; number of pad spaces to add to the end of the operand bytes:
; (7 - (3 * num_operand_bytes)):
;       f02c: ca       | dex
;               ^^^^^^^ 0 operand bytes, 7 spaces.
;       f02d: d0 fa    | bne  $f029
;                  ^^^^ 1 operand byte, 4 spaces.
;       f042: bd 8e f3 | lda  $f38e,x
;                     ^ 2 operand bytes, 1 space.
padd:   .byte 7,4,1

; ".byte ", backwards
dotbyte: .byte " etyb."

