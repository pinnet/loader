!to "c:\64\loader",cbm

* = $0801                                 ; BASIC start address (#2049
!byte $0d,$08,$dc,$07,$9e,$20,$32         ; BASIC loader to start at $c000...
!byte $31,$37,$36,$00,$00,$00,$00         ; puts BASIC line 2012 SYS 49152
* = $0880                                 ; start address for 6502 code

  address_music = $1000                   ; loading address for sid tune
  sid_init      = $1000                        ; init routine for music
  sid_play      = $1003                        ; play music routine
  payload       = $2700                         ; payload
  FRAMETRIGGER  = 5
  SCREENCOLS    = 40
  SCREENLINES   = 24
  STATUSLINE    = SCREENLINES / 2 + 4
  MAXFRAMES     = 3
  
  INTCLOCK      = $02                       ; intrupt clock / frame number
  CURH          = $fc                       ; cursor high byte zero page address
  CURL          = $fb                       ; cursor low byte zero page address
  
  SUB_CHROUT  = $ffd2                     ; CHROUT Kernal routine
  SCREEN      = $0400 + 40 * 23 + 33  
  TEXTCOL     = $286                      ; Text color
  BACKCOL     = $d021                     ; Background color
  BORDCOL     = $d020                     ; Border color
  CHRSET      = $d018                     ; current char set
  
  lda #FRAMETRIGGER 
  sta INTCLOCK
  
  
  
  lda #$00                                ; Initalise cursor 
  sta CURL                                ; 
  sta CURH                                ;
  sta BACKCOL                             ; initalise border background and text color
  sta BORDCOL                             ; 
  sta TEXTCOL                             ;
  
  
  lda #$17                                ; switch to alternitve chr set
  sta CHRSET                              ; 
  lda #$93                                ; clear screen 
  jsr SUB_CHROUT                          ;
  
  
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
  cli                                     ; clear interrupt disable flag
  
  lda #$9B
  sta copy
  
  lda #>copy
  sta CURH
  lda #<copy
  sta CURL  
  lda #copylast-copy                        ;length of string
  sta txtlength
  JSR consoleout

  LDA #fnamelast-fname
  LDX #<fname
  LDY #>fname
  JSR loadcode
  LDA #cnamelast-cname
  LDX #<cname
  LDY #>cname
  JSR loadcode
  LDA #sidlast-sid
  LDX #<sid
  LDY #>sid
  JSR loadcode
  jsr sid_init
  
  lda #$01
  sta musicready
  
  lda #$9B
  sta status  
  
  lda #<status
  sta CURL
  lda #>status
  sta CURH
  
  lda #0
  sta oldcount
  
 
    
main_loop 
    
    jsr countanim
    
    lda count 
    cmp oldcount
    beq main_loop
    sta oldcount
    
    
    
    jsr statusout
    
    jmp main_loop


  jmp * 
;--------------------------------------------------------------------------------------------------------------------------------IRQ
irq        
   dec $d019                                    ; acknowledge IRQ / clear register for next interrupt
    ;lda #$04
    ;sta BORDCOL
    
    pha
    txa
    pha
    tya
    pha
    lda CURH
    pha
    lda CURL
    pha
 
    dec INTCLOCK
    bne .skip1
    lda #FRAMETRIGGER
    sta INTCLOCK
   
    
    sed
    clc
    lda count
    adc #1
    sta count
    lda count + 1
    adc #0
    sta count + 1
    lda count + 2
    adc #0
    sta count + 2
    cld
    
    
.skip1
    lda musicready
    cmp #1
    bne .endloop
    jsr sid_play
    
.endloop
    pla 
    sta CURL
    pla 
    sta CURH
    pla 
    tay
    pla
    tax
    pla
    
    
    ;lda #$00
    ;sta BORDCOL
    jmp $ea31  
    ; return to kernel interrupt routine  
;--------------------------------------------------------------------------------------------------------------------------------IRQ 
   
;--------------------------------------------------------------------------------------------------------------------------------SUBroutines
statusout
    lda #$13
    jsr SUB_CHROUT
    ldy #0
.start  
    lda #$11
    jsr SUB_CHROUT
    iny
    cpy #STATUSLINE
    bne .start
    
    lda #$1D 
    jsr SUB_CHROUT 

    jmp textout
   
consoleout                      ; output cursor down until last line
    lda #$13
    jsr SUB_CHROUT
    ldy #0
.start1  
    lda #$11
    jsr SUB_CHROUT
    iny
    cpy #SCREENLINES
    bne .start1

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
     jsr SUB_CHROUT     
     dey
     bne .lp
.cont

textout    
    ldy #$00                    ;
.loopout    
    lda (CURL),y                ; read chrs from pointer
    jsr SUB_CHROUT              ; output using rom
    INY                         ; 
    CPY txtlength               ; has index (y) reached the length of text
    BNE .loopout                ; if not loop
    RTS
    
loadcode
        JSR $FFBD               ; call SETNAM
        LDA #$01
        LDX $BA                 ; last used device number
        BNE .skip
        LDX #$08                ; default to device 8
.skip                           ; not $01 means: load to address stored in file
        JSR $FFBA               ; call SETLFS
        LDA #$00                ; $00 means: load to memory (not verify)
        JSR $FFD5               ; call LOAD
        BCS .error              ; if carry set, a load error has happened
        RTS
.error
    sei
    ldy #$7f                                ; $7f = %01111111
    sty $dc0d                               ; Turn off CIAs Timer interrupts
    sty $dd0d                               ; Turn off CIAs Timer interrupts
    lda $dc0d                               ; cancel all CIA-IRQs in queue/unprocessed
    lda $dd0d                               ; cancel all CIA-IRQs in queue/unprocessed
    lda #$93                                ; clear screen character
    jsr SUB_CHROUT  
    lda #$1C
    sta error
    lda #errorlast - error
    sta txtlength
    lda #<error
    sta CURL
    lda #>error
    sta CURH
    jsr consoleout
    lda #02
    sta BORDCOL 


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
    sta SCREEN,y 
    dey
    rts
        
   JMP*   
;-------------------------------------------------------------------------DATA
musicready  !byte 0
pads        !byte 0
txtlength   !byte 0
oldcount    !byte 0
 

error !text ">eRROR - 404 file not found" 
errorlast 
copy !text ">LOADER V 1.0 (C)DANNYARNOLD.COM 2019"
copylast
cname  !text "COL1"
cnamelast        
fname  !text "SCN1"
fnamelast
sid !text "SID"
sidlast
status        !text ">       dR wHO and the tardis        ",
              !text "        mUSIC BY aLISSE gLASS        ",
              !text "           pRESS m FOR MUTE          ",
              !text "        dR wHO and the tardis        "
statend

count       !byte 0, 0, 0
