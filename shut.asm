; "Shut the Box" for the Apple I

; Jeff Jetton
; April 2020

; Written for the dasm assembler, but should assemble under others
; with a few tweaks here and there.



        processor 6502
        
        ; Contants
KBD     equ $D010           ; Location of input character from keyboard
KBDCR   equ $D011           ; Keyboard control: Indicator that a new input
                            ;                   character is ready
PRBYTE  equ $FFDC           ; WozMon routine to diaplay register A in hex
ECHO    equ $FFEF           ; WozMon routine to display register A char
WOZMON  equ $FF1F           ; Entry point back to WozMonitor (It's own GETLINE)
CR      equ $0D             ; Carriage return ASCII value
BACKSP  equ "_"             ; Yup. This is what the backspace key generates.
MAXBUFF equ 220             ; Size of input text buffer (if buffer is maxed out,
                            ; we'll automatically add a CR to the end, so total
                            ; size might be as much as MAXBUFF + 1)
SHUT    equ $2F             ; Value to represent "shut" in the DOORS array
                            ; When converted to ASCII in the same way as the 
                            ; other digits, this will display as an underscore


        ; Zero-page variables
        seg.u VARS

        org $0000

DOORS   ds 9    ; The state of the 9 "doors"
CHOICES ds 9    ; Keeps track of unique door numbers picked by the player
CYCOFF  ds 1    ; A constantly-cycled (during key polling) offset into the
                ; die results table.  Creates essentially-random die rolls
                ; without actually futzing around with a PRNG.
CURROLL ds 1    ; Current turn's dice roll result (two numbers in BCD)
ROLLTOT ds 1    ; The combined value of the dice roll
CHOITOT ds 1    ; Total of door number(s) chosen by the player on a turn
SCORE   ds 1    ; Running score of remaining door values
TXTLO   ds 1    ; Low-order byte of address of beginning of text to print
TXTHI   ds 1    ; High-order byte of text
BUFFER  ds 1    ; Beginning of text buffer (ensure that this is defined last!)
        
        
; Main program  --------------------------------------------------------------

        seg CODE
        org $0300
        
        ; Init the program
        cld                 ; BCD mode off
        ldx #$FF
        txs                 ; Reset stack to $FF
        inx                 ; Set the cycled dice offset to zero
        stx CYCOFF
        
        ; Show welcome message and ask if user wants instructions
        lda #<TXT_WELCOME   ; Store low byte of text data location
        sta TXTLO
        lda #>TXT_WELCOME   ; Store high byte of text data location
        sta TXTHI
        jsr PRINTXT         ; Call generic print function
        
        ; Get and handle answer to instructions question
        jsr GETYN
        bne NEWGAME         ; Skip to game if they didn't type "Y"
        lda #<TXT_INSTRUCT  ; Otherwise, print instructions...
        sta TXTLO
        lda #>TXT_INSTRUCT
        sta TXTHI
        jsr PRINTXT
        
        ; Initialize game with all "doors" open
NEWGAME ldx #8
        lda #9
.initlp sta DOORS,x
        sec
        sbc #1
        dex
        bpl .initlp
        lda #45
        sta SCORE
        
        
        ; Print game state
PRINGAM jsr NEWLINE
        jsr NEWLINE
        ldx #0
.prgmlp lda DOORS,x
        clc
        adc #"0"        ; Convert integer representation to ASCII
        jsr ECHO
        inx
        cpx #9
        bne .prgmlp
        
        ; Check to see if we've won
        lda SCORE
        bne GETDICE
        
        ; TODO: Finish this bit. Display totally shut doors, etc.
        jsr NEWLINE
        jsr NEWLINE
        lda #":"
        jsr ECHO
        lda #"-"
        jsr ECHO
        lda #")"
        jsr ECHO
        jsr NEWLINE
        jmp WOZMON
        
GETDICE ; Get and print dice roll (and also calculate dice total)
        lda #<TXT_ROLL      ; Label for dice roll results
        sta TXTLO
        lda #>TXT_ROLL
        sta TXTHI
        jsr PRINTXT
        ldx CYCOFF          ; Get current dice value
        lda DICE,x
        sta CURROLL         ; Store as raw BCD for now...
        
        ; TODO: Use some wozmon routines for some of this?
        lsr                 ; Isolate first die number
        lsr
        lsr
        lsr
        sta ROLLTOT         ; Save as first part of total
        clc                 ; Convert to ASCII value
        adc #"0"
        jsr ECHO            ; Display first die text
        lda #<TXT_AND       ; Print " & "
        sta TXTLO
        lda #>TXT_AND
        sta TXTHI
        jsr PRINTXT
        lda CURROLL         ; Isolate second die value
        and #$0F
        tax                 ; Remember it in X for a bit...
        clc                 ; Add die value to total
        adc ROLLTOT
        sta ROLLTOT
        txa                 ; Bring the die value back
        adc #"0"            ; Convert to ASCII and display
        jsr ECHO
        jsr NEWLINE
        jsr NEWLINE


        
        
CHKDONE ; Check to see if there even is any valid move or if game over
        ; TODO:  ???
        
        
        ; Get player's door choice(s)
GETMOVE jsr GETLINE
        
        ; Process the input line.  TODO: Could this go backwards from the end?
        ldx #8          ; First clear out the bytes that track unique...
        lda #0          ; ...door number choices for this turn
.clrlp  sta CHOICES,x
        dex
        bpl .clrlp
        inx             ; Now prepare to move through input buffer
.chkcr  lda BUFFER,x
        inx
        cmp #CR         ; Have we reached the end of the buffer?
        beq CHKMOVE
        sec             ; Is this a valid digit, 1-9?...
        sbc #"1"        ;   Converts "1" to 0 and "9" to 8
        cmp #9          ;   If we're not less than 9, not a valid door number
        bcs .chkcr      ;   ...so ignore and move to next byte
        tay             ; Use converted value as an offset
        adc #1          ; Convert A from offset to value (note that C is clear)
        sta CHOICES,y   ; Set unique choice number byte to value of that choice
        jmp .chkcr
        
        
        
CHKMOVE ; Is the move valid? (Didn't pick an already-shut door)
        ; This also calculates the total of the chosen door values
        lda #0          ; Clear out choice total
        sta CHOITOT
        
        ldx #8          ; Iterate over DOORS and CHOICES at the same time
.choilp lda CHOICES,x
        beq .nextch     ; Zero means user didn't pick this door number
        clc
        adc CHOITOT     ; This door was chosen, so add its value
        sta CHOITOT
        lda DOORS,x     ; Check the corresponding door...
        cmp #SHUT       ; Is it already shut?
        bne .nextch
        jsr PRINERR     ; Shut door chosen. Show error message...
        jmp GETMOVE     ;   ...and have user re-enter move
.nextch dex
        bpl .choilp
        ; Did the user pick numbers that have same total as dice roll?
        lda ROLLTOT
        cmp CHOITOT
        beq SHUTEM
        jsr PRINERR     ; Totals don't match. Show unhelpful message.
        jmp GETMOVE     ;   ...and try again
        
        
SHUTEM  ; Go back through and set chosen doors to "shut"
        ldx #8
.doorlp lda CHOICES,x
        beq .nextdr
        lda SCORE       ; Decrease the score
        sec             ;   by the value
        sbc CHOICES,x   ;   of the shut
        sta SCORE       ;   door
        lda #SHUT       ; Mark the door as being shut
        sta DOORS,x
.nextdr dex
        bpl .doorlp
        
        ; That's one turn down...
        jmp PRINGAM



        ; Game over...
        lda #<TXT_REPLAY    ; Prompt for playing another round
        sta TXTLO
        lda #>TXT_REPLAY
        sta TXTHI
        jsr PRINTXT
        jsr GETYN           ; Get valid Y or N input
        bne ENDGAME
        jmp NEWGAME
        
ENDGAME lda #<TXT_BYE
        sta TXTLO
        lda #>TXT_BYE
        sta TXTHI
        jsr PRINTXT
        jmp WOZMON          ; Return to WozMonitor

        
; Subroutines   ***************************************************************
; TODO: Implement ESCape
; TODO: Use woz method to limit buffer size if bytes still need to be shaved
GETLINE SUBROUTINE  ; Read input into the buffer (also cycles dice offset)
        ldx #0          ; Register X is our current buffer offset
        ; Dice cycling...
.getkey dec CYCOFF      ; Bump the die results offset down one byte
        bmi .rstoff     ; If we've gone past zero, reset back to the end
        bit 0           ; Waste three cycles so branch timing matches
        jmp .chkpia     ; 3 cycles
.rstoff lda #35         ; 2 cycles (plus extra cycle from the taken branch)
        sta CYCOFF      ; 3 cycles
        ; ...end of dice cycling
.chkpia lda KBDCR       ; Check PIA for keyboard input
        bpl .getkey     ; Loop if A is "positive" (bit 7 low... no key ready)
        lda KBD         ; Get the keyboard character
        and #%01111111  ; Clear bit 7, which is always set for some reason
        jsr ECHO        ; Always echo what the user just typed
        sta BUFFER,x    ; Store it in the buffer too
        cmp #CR         ; Did they hit "return"?
        bne .chkbs
        rts
.chkbs  cmp #BACKSP     ; Did they hit backspace?
        bne .next
        dex             ; Move buffer pointer back
        dex             ; TODO:  Move where we do this so we never write the _
        ;  to the buffer and only have to do one dex
        bmi GETLINE     ; Reset the whole shebang if we backspaced all the way 
.next   inx             ; Next buffer offset please
        cpx #MAXBUFF
        bne .getkey     ; As long as there's room in the buffer, get another key
        lda #CR
        sta BUFFER,x    ; Otherwise, tack on a CR and peace out
.done   rts



PRINTXT SUBROUTINE  ; Put string pointer in TXTLO & TXTHI before calling
        ldy #0          ; Y is the offset within the string
.loop   lda (TXTLO),Y   ; Load A with whatever's at pointer + Y
        beq .end        ; If char data is zero, that's the end
        jsr ECHO        ; Otherwise, print it
        iny
        jmp .loop
.end    rts             ; Note that we've destoyed A & Y


; TODO: put a TWOLINE routine here to replace two newlines in a row
NEWLINE SUBROUTINE  ; Just print out a newline (destroys A)
        lda #$0D
        jmp ECHO



GETYN   SUBROUTINE  ; Gets a valid Y or N response from user. Sets Z flag on Y.
        jsr GETLINE
        lda BUFFER
        cmp #"N"
        beq .nope
        cmp #"Y"
        beq .yup
        jsr PRINERR
        jmp GETYN       ; Try again
.nope   tsx             ; This clears the zero flag
.yup    rts



PRINERR SUBROUTINE  ; Displays a standard input error message
        lda #<TXT_BADINPUT
        sta TXTLO
        lda #>TXT_BADINPUT
        sta TXTHI
        jmp PRINTXT



; Stored data   ****************************************************************

TXT_WELCOME
        .byte $0D
        .byte $0D
        dc "     SHUT THE BOX -- BY JEFF JETTON"
        .byte $0D
        .byte $0D
        dc "           (WORK IN PROGRESS)"
        .byte $0D
        .byte $0D
        .byte $0D
        .byte $0D
        dc "INSTRUCTIONS (Y/N)? "
        .byte $00

TXT_INSTRUCT
        .byte $0D
        .byte $0D
        dc "ENTER ONE OR MORE AVAILABLE DIGITS TO"
        .byte $0D
        dc "REMOVE THEM.  CHOICE OF DIGIT(S) MUST"
        .byte $0D
        dc "HAVE THE SAME TOTAL AS YOUR DICE ROLL."
        .byte $0D
        .byte $0D
        dc "GAME ENDS IF NO VALID CHOICE REMAINS."
        .byte $0D
        dc "REMOVE EVERY DIGIT TO WIN!"
        .byte $0D
        .byte $00  


TXT_ROLL
        .byte $0D
        .byte $0D
        dc "YOU ROLL "
        .byte $00
        
TXT_AND
        dc " & "
        .byte $00        

TXT_BADINPUT
        .byte $0D
        dc "INVALID CHOICE.  TRY AGAIN: "
        .byte $00
        
        
TXT_REPLAY
        .byte $0D
        .byte $0D
        dc "PLAY AGAIN (Y/N)? "
        .byte $00

TXT_BYE
        .byte $0D
        .byte $0D
        dc "BYE!"
        .byte $0D
        .byte $0D
        .byte $00

        ; TODO if I can code a 16-bit prng in less than 36 bytes...
        ;      or create a table in memory in less than 36 bytes
DICE    hex 11 12 13 14 15 16
        hex 41 42 43 44 45 46
        hex 31 32 33 34 35 36
        hex 41 42 43 44 45 46
        hex 51 52 53 54 55 56
        hex 21 22 23 24 25 26
        hex 61 62 63 64 65 66