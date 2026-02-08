.org $8000
start:
    LDA #$01
    STA <$10
    ADC #$02
    BNE start
    RTS
