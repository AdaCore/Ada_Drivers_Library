This is a simple test/example for nRF24L01+ with
STM32F429Disco. Two nRF24L01+ should be connected
to the following board's pins:

| TX:  |     |
|------|-----|
| CE   | PB7 |
| CSN  | PE3 |
| SCK  | PE2 |
| MISO | PE5 |
| MOSI | PE6 |

| RX:  |      |
|------|------|
| CE   | PC11 |
| CSN  | PC12 |
| IRQ  | PB4  |
| SCK  | PE2  |
| MISO | PE5  |
| MOSI | PE6  |

to communicate with each other.
