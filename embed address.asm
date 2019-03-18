* = $0801                                 ; BASIC start address (#2049
!byte $0d,$08,$dc,$07,$9e,$20,$32         ; BASIC loader to start at $c000...
!byte $31,$37,$36,$00,$00,$00,$00         ; puts BASIC line 2012 SYS 49152
                        
address_music =  $C000; loading address for sid tune

* = $0880 ; start address for 6502 code.
    
   file_start = $C000   ; example addresses
    file_end   = $C0A5

        LDA #fname_end-fname
        LDX #<fname
        LDY #>fname
        JSR $FFBD     ; call SETNAM
        LDA #$00
        LDX $BA       ; last used device number
        BNE .skip
        LDX #$08      ; default to device 8
.skip   LDY #$00
        JSR $FFBA     ; call SETLFS

        LDA #<file_start
        STA $C1
        LDA #>file_start
        STA $C2

        LDX #<file_end
        LDY #>file_end
        LDA #$C1      ; start address located in $C1/$C2
        JSR $FFD8     ; call SAVE
        BCS .error    ; if carry set, a load error has happened

 .error
 
        
fname  !text "SCN1.trm"
fname_end


* = address_music                         ; address to load the music data
!bin "scn1.deflate",,
