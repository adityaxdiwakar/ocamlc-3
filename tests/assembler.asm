.ORIG x3000
    ADD     R0, R2, R5
    ADD     R2, R6, #-12
    AND     R3, R6, R2
    AND     R2, R6, #-12

    BRnz    242
.END


.ORIG x4000
    LABEL:  ADD     R0, R2, R5
.END
