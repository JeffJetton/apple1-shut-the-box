; "Shut the Box" for the Apple I

; Jeff Jetton
; April-May 2020

; Written for the dasm assembler, but should assemble under others
; with a few tweaks here and there.

; 1_3______  with a single-die roll of 4???


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
WIDTH   equ 40              ; Screen width, in characters
SHUT    equ $2F             ; Value to represent "shut" in the DOORS array
                            ; When converted to ASCII in the same way as the 
                            ; other digits, this will display as an underscore.
                            ; (Also makes the game over check efficient)


        ; Zero-page variables
        seg.u VARS

        org $0000

DOORS   ds 9    ; The state of the 9 "doors"
CHOICES ds 9    ; Keeps track of unique door numbers picked by the player
CYCOFF  ds 1    ; A constantly-cycled (during key polling) offset into the
                ; die results table.  Creates "random" die rolls (sort of)
                ; without actually futzing around with a PRNG.
CURROLL ds 1    ; Current turn's dice roll result (two numbers in BCD)
ROLLTOT ds 1    ; The combined value of the dice roll
CHOITOT ds 1    ; Total of door number(s) chosen by the player on a turn
SCORE   ds 1    ; Running score of remaining door values
NUMOPEN ds 1    ; Running count of open doors
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
        jsr PRTEXT2         ; Call version of print function that adds CRs
        
        ; Get and handle answer to instructions question
        jsr GETYN
        bne NEWGAME         ; Skip to game if they didn't type "Y"
        lda #<TXT_INSTRUCT  ; Otherwise, print instructions...
        sta TXTLO
        lda #>TXT_INSTRUCT
        sta TXTHI
        jsr PRTEXT2
        
        
        ; Initialize game with all "doors" open
NEWGAME ldx #8
        lda #9
        sta NUMOPEN
.initlp sta DOORS,x
        sec
        sbc #1
        dex
        bpl .initlp
        lda #$45        ; Score starts at BCD 45 (total of all digits)
        sta SCORE
        
        
        ; Print game state
PRINGAM jsr TWOLINE
        ldx #0
.prgmlp lda DOORS,x
        jsr ECHOINT     ; Display the door value
        inx
        cpx #9
        bne .prgmlp
        
        ; Check to see if we've won (no doors open--i.e. all shut)
        lda NUMOPEN
        bne GETDICE
        ; We've won!!!!
        jsr TWOLINE
        jsr TWOLINE
        lda #"*"
        ldx #40
        jsr PRINREP
        lda #" "
        ldx #92
        jsr PRINREP
        lda #<TXT_WINNER
        sta TXTLO
        lda #>TXT_WINNER
        sta TXTHI
        jsr PRTEXT
        lda #"*"
        ldx #40
        jsr PRINREP
        jsr TWOLINE
        jmp REPLAY

        
GETDICE ; Get and print dice roll (and also calculate dice total)
        lda #<TXT_ROLL      ; Label for dice roll results
        sta TXTLO
        lda #>TXT_ROLL
        sta TXTHI
        jsr PRTEXT2
        ldx CYCOFF          ; Get current dice value
        lda DICE,x
        sta CURROLL         ; Store as raw BCD for now...
        
        ; TODO: Use some wozmon routines for some of this?
        lsr                 ; Isolate first die number
        lsr
        lsr
        lsr
        sta ROLLTOT         ; Save as first part of total
        jsr ECHOINT         ; Display first die text
        lda SCORE           ; Is the total of remaining (unshut)
        cmp #7              ;   digits 6 or less? If so,
        bmi .skpdie         ;   only use the first die
        lda #<TXT_AND       ; Print " & "
        sta TXTLO
        lda #>TXT_AND
        sta TXTHI
        jsr PRTEXT
        lda CURROLL         ; Isolate value of
        and #$0F            ;   the second die
        tax                 ; Remember it in X
        jsr ECHOINT         ;   display it (this destroys A)
        txa                 ;   then bring it back
        clc                 ; Add die value to total
        adc ROLLTOT
        sta ROLLTOT
.skpdie jsr TWOLINE

        
        
CHKDONE ; Check to see if there even is any valid move or if game over
        ; Work backwards through array. Y is our "current top" index
        ; X will be the "moving left from top" index. A is working total.
        ldy #8
.newtop lda DOORS,y     ; Is the current "top"
        cmp #SHUT       ;   digit already shut?
        beq .dectop     ;   If so, skip it
        tya
        tax             ; x = y
        lda ROLLTOT
.trysub cmp DOORS,x     ; Can current door be subtracted from working total?
        bmi .skpsub     ; Don't subtract if not. Note that the "shut" value
        sec             ; is large enought to always branch, which is handy!
        sbc DOORS,x
        beq GETMOVE     ; Down to zero? Valid move exists--game not over yet!
.skpsub dex             ; Bump index to the left
        bpl .trysub     ; If not past the end of array, try another subtraction
.dectop dey             ; Bump the top to the left
        bpl .newtop     ; Top not past end? Reset X & A and try again.
        

GAMEOVER    ; No more moves. Display "game over" message
        lda #<TXT_GAMEOVER
        sta TXTLO
        lda #>TXT_GAMEOVER
        sta TXTHI
        jsr PRTEXT
        
        ; Score display is different if only one digit left
        lda NUMOPEN
        cmp #1
        beq .onedig
        
        ; Standard score message
        jsr ECHOINT             ; Print num digits
        lda #<TXT_SCOREMULT     ; Print score "label" text
        sta TXTLO
        lda #>TXT_SCOREMULT
        sta TXTHI
        jsr PRTEXT
        lda SCORE               ; Tack score on end
        jsr PRBYTE
        jmp REPLAY
        
        ; Score message if one digit left
.onedig lda #<TXT_SCOREONE
        sta TXTLO
        lda #>TXT_SCOREONE
        sta TXTHI
        jsr PRTEXT
        jmp REPLAY
        
        
GETMOVE ldx #8          ; Zero out the bytes that track unique
        lda #0          ;   door number choices for this turn
.clrlp  sta CHOICES,x
        dex
        bpl .clrlp
        jsr GETLINE
        
        ; Process the input line.
        ; Note that, after GETLINE, X points to one past end of buffer text
.chkcr  dex             ; Move x one char to the left
        bmi CHKMOVE     ; If past beginning of 128-char buffer, we're done
        lda BUFFER,x    ; Otherwise, put char in A
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
        sed             ; Prepare for BCD operation...
        lda SCORE       ; Decrease the score
        sec             ;   by the value
        sbc CHOICES,x   ;   of the shut
        sta SCORE       ;   door
        cld             ; Back to decimal operation
        dec NUMOPEN     ; Update count of open doors
        lda #SHUT       ; Mark the door as being shut
        sta DOORS,x
.nextdr dex
        bpl .doorlp
        
        ; That's one turn down...
        
        ; TEMP fake a win...
        ; lda #0
        ; sta NUMOPEN
        
        jmp PRINGAM



        ; Prompt for playing another round
REPLAY  lda #<TXT_REPLAY
        sta TXTLO
        lda #>TXT_REPLAY
        sta TXTHI
        jsr PRTEXT2
        jsr GETYN           ; Get valid Y or N input
        bne GOODBYE
        jmp NEWGAME
        
GOODBYE lda #<TXT_BYE       ; TODO: Save about 19 bytes by getting rid of this
        sta TXTLO
        lda #>TXT_BYE
        sta TXTHI
        jsr PRTEXT2
        jmp WOZMON          ; Return to WozMonitor



; Subroutines   ***************************************************************
; TODO: Implement ESCape
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
        
.chkbs  cmp #BACKSP     ; Did they hit backspace?
        bne .bufstr
        dex             ; Move buffer pointer back
        bmi GETLINE     ; Reset the whole shebang if we backspaced all the way
        jmp .getkey     ; Otherwise, just get next key input
             
.bufstr sta BUFFER,x    ; Store key in the buffer
        cmp #CR         ; Did they hit "return"?
        beq .done

.next   inx             ; Next buffer offset please
        bpl .getkey     ; Wozmon-style 128-character limit. Return if full.
.done   rts


PRTEXT2 jsr TWOLINE     ; Call this to put two CRs before what you're printing
PRTEXT  SUBROUTINE      ; Put string pointer in TXTLO & TXTHI before calling
        ldy #0          ; Y is the offset within the string
.loop   lda (TXTLO),Y   ; Load A with whatever's at pointer + Y
        beq .end        ; If char data is zero, that's the end
        jsr ECHO        ; Otherwise, print it
        iny
        jmp .loop
.end    rts             ; Note that we've destoyed A & Y



TWOLINE lda #CR
        jsr ECHO
        jmp ECHO


ECHOINT ; Convert integer in A to ASCII equivalent before calling ECHO
        clc
        adc #"0"
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



PRINERR ; Displays a standard input error message
        lda #<TXT_BADINPUT
        sta TXTLO
        lda #>TXT_BADINPUT
        sta TXTHI
        jmp PRTEXT2


PRINREP SUBROUTINE  ; Prints whatever's in A for X times
.loop   jsr ECHO
        dex
        bne .loop
        rts
        


; Stored data   ****************************************************************

TXT_WELCOME
        dc "SHUT THE BOX - BY JEFF JETTON"
        .byte $0D
        .byte $0D
        .byte $0D
        dc "INSTRUCTIONS (Y/N)? "
        .byte $00

TXT_INSTRUCT  ; TODO: remove periods if needed!
        dc "ENTER ONE OR MORE AVAILABLE DIGITS TO"
        .byte $0D
        dc "REMOVE THEM.  DIGIT(S) PICKED MUST HAVE "
        dc "SAME TOTAL AS DICE."
        .byte $0D
        .byte $0D
        dc "ONLY 1 DIE ROLLED IF REMAINING DIGITS"
        .byte $0D
        dc "TOTAL 6 OR LESS"
        .byte $0D
        .byte $0D
        dc "GAME ENDS IF NO VALID CHOICE LEFT"
        .byte $0D
        .byte $0D
        dc "REMOVE EVERY DIGIT TO WIN!"
        .byte $0D
        .byte $00

TXT_ROLL
        dc "YOU ROLL "
        .byte $00
        
TXT_AND
        dc " & "
        .byte $00        

TXT_BADINPUT
        dc "INVALID CHOICE.  TRY AGAIN: "
        .byte $00
        
TXT_WINNER
        dc "CONGRATULATIONS!"
        .byte $0D
        .byte $0D
        .byte $0D
        .byte $0D
        dc "          YOU'VE SHUT THE BOX!"
        .byte $0D
        .byte $0D
        .byte $0D
        byte $00

TXT_GAMEOVER
        dc "NO MORE MOVES"
        .byte $0D
        .byte $0D
        .byte $0D
        .byte $00

TXT_SCOREMULT
        dc " DIGITS LEFT TOTALING "
        .byte $00
                
TXT_SCOREONE
        dc "OOOH... SO CLOSE!"
        .byte $0D
        .byte $0D
        dc "JUST ONE LEFT!"
        .byte $00            
        
TXT_REPLAY
        .byte $0D
        dc "PLAY AGAIN (Y/N)? "
        .byte $00

TXT_BYE
        dc "BYE!"
        .byte $0D
        .byte $0D
        .byte $00

        ; TODO if I can code a 16-bit prng in less than 36 bytes...
        ;      or create a table in memory in less than 36 bytes
        ; Or mix up digit two better
DICE    hex 11 12 13 14 15 16
        hex 41 42 43 44 45 46
        hex 31 32 33 34 35 36
        hex 41 42 43 44 45 46
        hex 51 52 53 54 55 56
        hex 21 22 23 24 25 26
        hex 61 62 63 64 65 66