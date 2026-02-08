    LDA #LABEL+1
    STA (32),Y
    LDA (16,X)
    ADC LABEL+2*3
