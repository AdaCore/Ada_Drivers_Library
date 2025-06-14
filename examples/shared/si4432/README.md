This is a simple test/example for Si4432B1 with
STM32F429Disco. Two Si4432B1 should be connected
to the following board's pins:

| TX:  |     |
|------|-----|
| SDN  | PB7 |
| CSN  | PE3 |
| IRQ  | PC3 |
| SCK  | PE2 |
| MISO | PE5 |
| MOSI | PE6 |

| RX:  |      |
|------|------|
| SDN  | PC11 |
| CSN  | PC12 |
| IRQ  | PB4  |
| SCK  | PE2  |
| MISO | PE5  |
| MOSI | PE6  |

to communicate with each other.
