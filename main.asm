!to "c:\64\loader",cbm

* = $0801                                 ; BASIC start address (#2049
!byte $0d,$08,$dc,$07,$9e,$20,$32         ; BASIC loader to start at $c000...
!byte $31,$37,$36,$00,$00,$00,$00         ; puts BASIC line 2012 SYS 49152
* = $0880                                 ; start address for 6502 code
  inflateaddr   = $CD00
  compressed    = $C000
  screenmem     = $03ff
  address_music = $1000                   ; loading address for sid tune
  sid_init      = $1000                        ; init routine for music
  sid_play      = $1003                        ; play music routine
  payload       = $2700                        ; payload
  
  FRAMETRIGGER  = 50
  SCREENCOLS    = 40
  SCREENLINES   = 24
  STATUSLINE    = SCREENLINES / 2 + 4
  
  INTCLOCK      = $02                       ; intrupt clock / frame number
  PTRH          = $FC                       ; cursor high byte zero page address
  PTRL          = $FB                       ; cursor low byte zero page address
  
  KERNAL_CHROUT  = $ffd2                     ; CHROUT Kernal routine
  CLOCKPTR       = $0400 + 40 * 23 + 33  
  TEXTCOL        = $286                      ; Text color
  BACKCOL        = $d021                     ; Background color
  BORDCOL        = $d020                     ; Border color
  CHRSET         = $d018                     ; current char set
  
  
  lda $01                                 ; Switch out Basic
  and #$fe                                ;
  sta $01                                 ;
  
  
  
  lda #FRAMETRIGGER                       ; number of frames until counter is reset
  sta INTCLOCK                            ; inttrupt generated clock
  
  lda #0
  sta loadmusic 
  
  lda #$00                                ; Initalise pointer
  sta PTRL                                ; 
  sta PTRH                                ;
  sta BACKCOL                             ; initalise background and text color the same                       ; 
  sta TEXTCOL                             ; for a 'quick blank' 
  sta bdrcol
  sta embeddaddr                          ; dynamic border colour updated every FRAME
  
  lda #$17                                ; switch to alternitve chr set
  sta CHRSET                              ; 
  lda #$93                                ; clear screen 
  jsr KERNAL_CHROUT                          ;
  
  
                               ; clear interrupt disable flag
  
  lda #$9B
  sta copy
  
  lda #>copy
  sta PTRH
  lda #<copy
  sta PTRL  
  lda #copylast-copy                        ;length of string
  sta txtlength
  JSR consoleout
  
 
  
  
  
  lda #<inflateaddr
  sta loadaddr
  lda #>inflateaddr
  sta loadaddr + 1
  LDA #inamelast-iname
  sta txtlength
  LDX #<iname
  LDY #>iname
  JSR loadcode
  
  lda #<compressed
  sta loadaddr
  sta $30
  lda #>compressed
  sta loadaddr + 1
  sta $31
 
  LDA #snamelast-sname
  sta txtlength
  LDX #<sname
  LDY #>sname
  JSR loadcode
  
  lda #<screenmem
  sta $32
  lda #>screenmem
  sta $33
  
 jsr inflateaddr
  
   
  ;jmp *
  
  sei 
  ldy #$7f                                ; $7f = %01111111
  sty $dc0d                               ; Turn off CIAs Timer interrupts
  sty $dd0d                               ; Turn off CIAs Timer interrupts
  lda $dc0d                               ; cancel all CIA-IRQs in queue/unprocessed
  lda $dd0d                               ; cancel all CIA-IRQs in queue/unprocessed
          
  lda #$01                                ; Set Interrupt Request Mask...
  sta $d01a                               ; ...we want IRQ by Rasterbeam

  lda #<irq                               ; point IRQ Vector to our custom irq routine
  ldx #>irq 
  sta $314                                ; store in $314/$315
  stx $315    

  lda #$10                                ; trigger first interrupt at row zero
  sta $d012

  lda $d011                               ; Bit#0 of $d011 is basically...
  and #$7f                                ; ...the 9th Bit for $d012
  sta $d011                               ; we need to make sure it is set to zero 
  cli
  lda#1
  
  sta embeddaddr
  LDA #cnamelast-cname
  sta txtlength
  LDX #<cname
  LDY #>cname
  JSR loadcode
  
  LDA #sidlast-sid
  sta txtlength
  LDX #<sid
  LDY #>sid
  
  
  lda loadmusic
  bne .noload
  JSR loadcode
  jsr sid_init
  
  lda #$01
  sta musicready
.noload 
 
  lda #$9B
  sta status  
  lda #<status
  sta PTRL
  lda #>status
  sta PTRH
  
  lda #0
  sta oldcount

main_loop 
                   ; ------------------------------------------------Full Speed loop 
    inc bdrcol

    
      
    lda bdrcol     ; set border colour
    sta BORDCOL    ; 
                   ; 
    lda count      ;    
    cmp oldcount   ;
    beq main_loop  ; Wait untll the counter changes        -----------Full Speed loop end
    sta oldcount   ; update oldcount with new count
                   ;--------------------------------------------------Count Speed loop
    
    jsr countanim
    jsr statusout
    
    jmp main_loop  ;---------------------------------------------------Count Speed loop end


  jmp * 
;--------------------------------------------------------------------------------------------------------------------------------IRQ
irq        
   dec $d019                    ; acknowledge IRQ / clear register for next interrupt
   
    pha                         ; Push the registers on the stack
    txa                         ;
    pha                         ;       
    tya                         ;
    pha                         ;
    lda PTRH                    ; Push the Pointer on the stack
    pha                         ;
    lda PTRL                    ;
    pha                         ;
 
    dec INTCLOCK                ; count down int clock
    bne .skip1                  ; skip ahead until zero
    lda #FRAMETRIGGER           ; then reset int clock 
    sta INTCLOCK                ; to FRAMETRIGGER default
   
    
    
    sed                         ; DECIMAL MODE
    clc                         ; add one to count and carry
    lda count                   ; thanks to Robin @bedfordlvlexp
    adc #1                      ; https://www.youtube.com/channel/UC3gRBswFkuteshdwMZAQafQ
    sta count                   ; for this one 
    lda count + 1               ;
    adc #0                      ;
    sta count + 1               ;
    lda count + 2               ;
    adc #0                      ;
    sta count + 2               ;
    cld                         ; END DECIMAL MODE
    
    
.skip1                          ;
    lda musicready              ; if music ready is not ready
    cmp #1                      ; then skip calling the play subroutine  
    bne .skipplay               ;
    jsr sid_play                ;
.skipplay                        
    
    pla                         ; Pull the pointer off the stack
    sta PTRL                    ;
    pla                         ;
    sta PTRH                    ;
    pla                         ; Pull the regesters off the staack 
    tay                         ;
    pla                         ;
    tax                         ;
    pla                         ;
    
    jmp $ea31                   ; return to kernel interrupt routine  
;--------------------------------------------------------------------------------------------------------------------------------IRQ 
   
;--------------------------------------------------------------------------------------------------------------------------------SUBroutines
statusout
    lda #$13                    ; Cursor Home
    jsr KERNAL_CHROUT
    ldy #0
@start  
    lda #$11                    ; Cursor Down
    jsr KERNAL_CHROUT
    iny
    cpy #STATUSLINE
    bne @start
    
    lda #$1D                    ; Cursor Right
    jsr KERNAL_CHROUT 
    jmp textout
   
consoleout                      ; Output cursor down until last line
    lda #$13
    jsr KERNAL_CHROUT
    ldy #0
@start  
    lda #$11
    jsr KERNAL_CHROUT
    iny
    cpy #SCREENLINES
    bne @start

padcenter
     lda txtlength
     clc
     ror a
     sta pads
     lda # SCREENCOLS / 2
     sec
     sbc pads
     bcc .cont
     sta pads
     tay
     lda #$1D 
.lp
     jsr KERNAL_CHROUT     
     dey
     bne .lp
.cont

textout    
    ldy #$00                    ;
.loopout    
    lda (PTRL),y                ; read chrs from pointer
    jsr KERNAL_CHROUT              ; output using rom
    INY                         ; 
    CPY txtlength               ; has index (y) reached the length of text
    BNE .loopout                ; if not loop
    RTS
    
loadcode
        lda txtlength
        JSR $FFBD               ; call SETNAM
        LDA #$01
        LDX $BA                 ; last used device number
        BNE @skip
        LDX #$08                ; default to device 8
 @skip  LDY embeddaddr          ; $00 means: load to new address
        JSR $FFBA               ; call SETLFS
        LDX loadaddr
        LDY loadaddr+1
        LDA #$00                ; $00 means: load to memory (not verify)
        JSR $FFD5               ; call LOAD
        BCS .error              ; if carry set, a load error has happened
        RTS
.error
    sei
    
    lda #$93                    ; clear screen character
    jsr KERNAL_CHROUT  
    lda #$1C                    ; Change to RED text
    sta error
    
    lda #errorlast - error      ; Output error message
    sta txtlength
    lda #<error                 
    sta PTRL
    lda #>error
    sta PTRH
    jsr consoleout
    lda #02
    sta bdrcol 


countanim
    ldy #5
    ldx #0
.sloop
    lda count,x
    pha
    and #$0f
    jsr plotbcd
    
    pla 
    lsr a
    lsr a
    lsr a
    lsr a
    jsr plotbcd
    inx
    cpx #3
    bne .sloop
    rts
    
plotbcd 
    clc
    adc #48
    sta CLOCKPTR,y 
    dey
    rts
        
   JMP*   
;-------------------------------------------------------------------------DATA
musicready  !byte 0
pads        !byte 0
txtlength   !byte 0
oldcount    !byte 0
bdrcol      !byte 0
loadmusic   !byte 0 
embeddaddr  !byte 0
loadaddr    !word 0

error         !text ">eRROR - 404 file not found" 
errorlast 
copy          !text ">LOADER V 1.0 (C)DANNYARNOLD.COM 2019"
copylast
cname         !text "COL1"
cnamelast        
sname         !text "SCN1"
snamelast
iname         !text "INF"
inamelast      
sid           !text "SID"
sidlast
status        !text ">   mUSIC:- dR wHO and the tardis    ",
              !text "           BY aLISSE gLASS           ",
              !text "           pRESS m FOR MUTE          ",
              !text "      ..........................     "
statend

count       !byte 0, 0, 0
