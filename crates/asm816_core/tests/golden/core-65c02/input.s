.org $4000
start:
    BRA next
    NOP
next:
    STZ <$20
    BIT #$80
    LDA ($10)
    JMP ($1234, X)
