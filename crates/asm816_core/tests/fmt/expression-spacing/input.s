LDA #( LABEL + 1 )
STA ( $20 ) , Y
LDA ( $10 , X )
ADC LABEL + ( 2 * 3 )
