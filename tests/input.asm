.ORIG x3000
    ; with three registers
    ADD     R4, R0, R2
    AND     R3, R5, R4	; both ADD and AND work
    AND     R3, R7, R2

    ; with two registers and immediate
    LABEL:  
    ADD     R2, R5, #12
    AND     R1, R1, x9

    ; not 
    NOT R3, R4

    ; branching 
    BRn     12
    BR      x32

    ; jumping
    JMP     R7

    ; loading/storing (indirectly?)
    LEA     R3, #-3      ; loading effective address
    LD      R0, #2
    LDI     R1, #23
    ST      R2, x35
    STI     R3, x85

    ; loading/storing from register
    LDR     R3, R4, #0
    STR     R2, R6, #15
.END
