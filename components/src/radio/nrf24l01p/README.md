This directory contains the driver for nRF24L01+ radio module. The nRF24AP2
ICs are low-cost, high-performance 2.4 GHz ISM single-chip connectivity devices
with an embedded ANT protocol stack.

nRF24L01 can also be driven with some reservations. For example, for Air_Data_Rate
use Rate_250kbps (2Mbps on nRF24L01) or Rate_1Mbps (1Mbps on nRF24L01) only.
Refer to the specifications for more information.

The examples/STM32F429_Discovery/nrf24l01p_f429disco.gpr project contains
a simple example.
