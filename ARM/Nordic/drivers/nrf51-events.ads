------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with System;
with NRF51_SVD.POWER;
with NRF51_SVD.CLOCK;
with NRF51_SVD.GPIOTE;
with NRF51_SVD.RADIO;
with NRF51_SVD.TIMER;
with NRF51_SVD.RTC;
with NRF51_SVD.WDT;
with NRF51_SVD.RNG;
with NRF51_SVD.TEMP;
with NRF51_SVD.ECB;
with NRF51_SVD.CCM;
with NRF51_SVD.AAR;
with NRF51_SVD.SPI;
with NRF51_SVD.TWI;
with NRF51_SVD.UART;
with NRF51_SVD.QDEC;
with NRF51_SVD.ADC;
with HAL; use HAL;


package nRF51.Events is

   function Triggered (Evt : Event_Type) return Boolean;

   procedure Enable_Interrupt (Evt : Event_Type);

   procedure Disable_Interrupt (Evt : Event_Type);

   procedure Clear (Evt : Event_Type);
   --  Software event clear

   function Get_Address (Evt : Event_Type) return System.Address;
   function Get_Address (Evt : Event_Type) return UInt32;

   --  Power management events
   Power_POFWARN       : constant Event_Type;

   --  Clock events
   Clock_HFCLKSTARTED  : constant Event_Type;
   Clock_LFCLKSTARTED  : constant Event_Type;
   Clock_DONE          : constant Event_Type;
   Clock_CTTO          : constant Event_Type;

   --  GPIOTE events
   GPIOTE_IN_0         : constant Event_Type;
   GPIOTE_IN_1         : constant Event_Type;
   GPIOTE_IN_2         : constant Event_Type;
   GPIOTE_IN_3         : constant Event_Type;
   GPIOTE_PORT         : constant Event_Type;

   --  Radio events
   Radio_READY         : constant Event_Type;
   Radio_ADDRESS       : constant Event_Type;
   Radio_PAYLOAD       : constant Event_Type;
   Radio_END           : constant Event_Type;
   Radio_DISABLED      : constant Event_Type;
   Radio_DEVMATCH      : constant Event_Type;
   Radio_DEVMISS       : constant Event_Type;
   Radio_RSSIEND       : constant Event_Type;
   Radio_BCMATCH       : constant Event_Type;

   --  Timer 0 events
   Timer_0_COMPARE_0   : constant Event_Type;
   Timer_0_COMPARE_1   : constant Event_Type;
   Timer_0_COMPARE_2   : constant Event_Type;
   Timer_0_COMPARE_3   : constant Event_Type;

   --  Timer 1 events
   Timer_1_COMPARE_0   : constant Event_Type;
   Timer_1_COMPARE_1   : constant Event_Type;
   Timer_1_COMPARE_2   : constant Event_Type;
   Timer_1_COMPARE_3   : constant Event_Type;

   --  Timer 2 events
   Timer_2_COMPARE_0   : constant Event_Type;
   Timer_2_COMPARE_1   : constant Event_Type;
   Timer_2_COMPARE_2   : constant Event_Type;
   Timer_2_COMPARE_3   : constant Event_Type;

   --  RTC 0 events
   RTC_0_TICK          : constant Event_Type;
   RTC_0_OVERFLW       : constant Event_Type;
   RTC_0_COMPARE_0     : constant Event_Type;
   RTC_0_COMPARE_1     : constant Event_Type;
   RTC_0_COMPARE_2     : constant Event_Type;
   RTC_0_COMPARE_3     : constant Event_Type;

   --  RTC 1 events
   RTC_1_TICK          : constant Event_Type;
   RTC_1_OVERFLW       : constant Event_Type;
   RTC_1_COMPARE_0     : constant Event_Type;
   RTC_1_COMPARE_1     : constant Event_Type;
   RTC_1_COMPARE_2     : constant Event_Type;
   RTC_1_COMPARE_3     : constant Event_Type;

   --  Watchdog task
   Watchdog_TIMEOUT    : constant Event_Type;

   --  Random Number Genrator events
   RNG_VALRDY          : constant Event_Type;

   --  Temperature events
   Temperature_DATARDY : constant Event_Type;

   --  AES Electronic Codebook mode encryption (ECB) events
   ECB_END             : constant Event_Type;
   ECB_ERROR           : constant Event_Type;

   --  AES CCM mode encryption (CCM) events
   CCM_KSGEN_END       : constant Event_Type;
   CCM_CRYPT_END       : constant Event_Type;
   CCM_ERROR           : constant Event_Type;

   --  Accelerated Address Resolver (AAR) events
   AAR_END             : constant Event_Type;
   AAR_RESOLVED        : constant Event_Type;
   AAR_NOTRESOLVED     : constant Event_Type;

   --  Serial Peripheral Interface (SPI) 0 events
   SPI_0_READY           : constant Event_Type;

   --  Serial Peripheral Interface (SPI) 1 events
   SPI_1_READY           : constant Event_Type;

   --  Two Wire Interface (TWI) 0 events
   TWI_0_STOPPED       : constant Event_Type;
   TWI_0_RXDRDY        : constant Event_Type;
   TWI_0_TXDENT        : constant Event_Type;
   TWI_0_ERRO          : constant Event_Type;
   TWI_0_BB            : constant Event_Type;
   TWI_0_SUSPENDED     : constant Event_Type;

   --  Two Wire Interface (TWI) 1 events
   TWI_1_STOPPED       : constant Event_Type;
   TWI_1_RXDRDY        : constant Event_Type;
   TWI_1_TXDENT        : constant Event_Type;
   TWI_1_ERRO          : constant Event_Type;
   TWI_1_BB            : constant Event_Type;
   TWI_1_SUSPENDED     : constant Event_Type;

   --  Universal Asynchronous Receiver/Transmitter (UART) Events
   UART_RXDRDY         : constant Event_Type;
   UART_TXDRDY         : constant Event_Type;
   UART_ERROR          : constant Event_Type;

   --  Quadrature Decoder (QDEC)
   QDEC_SAMPLERDY      : constant Event_Type;
   QDEC_REPORTRDY      : constant Event_Type;
   QDEC_ACCOF          : constant Event_Type;

   --  Analof to Digital Converter (ADC)
   ADC_END             : constant Event_Type;

private

   --  Power management events
   Power_POFWARN       : constant Event_Type :=
     Event_Type (NRF51_SVD.POWER.POWER_Periph.EVENTS_POFWARN'Address);

   --  Clock events
   Clock_HFCLKSTARTED  : constant Event_Type :=
     Event_Type (NRF51_SVD.CLOCK.CLOCK_Periph.EVENTS_HFCLKSTARTED'Address);
   Clock_LFCLKSTARTED  : constant Event_Type :=
     Event_Type (NRF51_SVD.CLOCK.CLOCK_Periph.EVENTS_LFCLKSTARTED'Address);
   Clock_DONE          : constant Event_Type :=
     Event_Type (NRF51_SVD.CLOCK.CLOCK_Periph.EVENTS_DONE'Address);
   Clock_CTTO          : constant Event_Type :=
     Event_Type (NRF51_SVD.CLOCK.CLOCK_Periph.EVENTS_CTTO'Address);

   --  GPIOTE events
   GPIOTE_IN_0         : constant Event_Type :=
     Event_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.EVENTS_IN (0)'Address);
   GPIOTE_IN_1         : constant Event_Type :=
     Event_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.EVENTS_IN (1)'Address);
   GPIOTE_IN_2         : constant Event_Type :=
     Event_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.EVENTS_IN (2)'Address);
   GPIOTE_IN_3         : constant Event_Type :=
     Event_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.EVENTS_IN (3)'Address);
   GPIOTE_PORT         : constant Event_Type :=
     Event_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.EVENTS_PORT'Address);

   --  Radio events
   Radio_READY         : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_READY'Address);
   Radio_ADDRESS       : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_ADDRESS'Address);
   Radio_PAYLOAD       : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_PAYLOAD'Address);
   Radio_END           : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_END'Address);
   Radio_DISABLED      : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_DISABLED'Address);
   Radio_DEVMATCH      : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_DEVMATCH'Address);
   Radio_DEVMISS       : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_DEVMISS'Address);
   Radio_RSSIEND       : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_RSSIEND'Address);
   Radio_BCMATCH       : constant Event_Type :=
     Event_Type (NRF51_SVD.RADIO.RADIO_Periph.EVENTS_BCMATCH'Address);

   --  Timer 0 events
   Timer_0_COMPARE_0   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER0_Periph.EVENTS_COMPARE (0)'Address);
   Timer_0_COMPARE_1   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER0_Periph.EVENTS_COMPARE (1)'Address);
   Timer_0_COMPARE_2   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER0_Periph.EVENTS_COMPARE (2)'Address);
   Timer_0_COMPARE_3   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER0_Periph.EVENTS_COMPARE (3)'Address);

   --  Timer 1 events
   Timer_1_COMPARE_0   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER1_Periph.EVENTS_COMPARE (0)'Address);
   Timer_1_COMPARE_1   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER1_Periph.EVENTS_COMPARE (1)'Address);
   Timer_1_COMPARE_2   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER1_Periph.EVENTS_COMPARE (2)'Address);
   Timer_1_COMPARE_3   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER1_Periph.EVENTS_COMPARE (3)'Address);

   --  Timer 2 events
   Timer_2_COMPARE_0   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER2_Periph.EVENTS_COMPARE (0)'Address);
   Timer_2_COMPARE_1   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER2_Periph.EVENTS_COMPARE (1)'Address);
   Timer_2_COMPARE_2   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER2_Periph.EVENTS_COMPARE (2)'Address);
   Timer_2_COMPARE_3   : constant Event_Type :=
     Event_Type (NRF51_SVD.TIMER.TIMER2_Periph.EVENTS_COMPARE (3)'Address);

   --  RTC 0 events
   RTC_0_TICK          : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC0_Periph.EVENTS_TICK'Address);
   RTC_0_OVERFLW       : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC0_Periph.EVENTS_OVRFLW'Address);
   RTC_0_COMPARE_0     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC0_Periph.EVENTS_COMPARE (0)'Address);
   RTC_0_COMPARE_1     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC0_Periph.EVENTS_COMPARE (1)'Address);
   RTC_0_COMPARE_2     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC0_Periph.EVENTS_COMPARE (2)'Address);
   RTC_0_COMPARE_3     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC0_Periph.EVENTS_COMPARE (3)'Address);

   --  RTC 1 events
   RTC_1_TICK          : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC1_Periph.EVENTS_TICK'Address);
   RTC_1_OVERFLW       : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC1_Periph.EVENTS_OVRFLW'Address);
   RTC_1_COMPARE_0     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC1_Periph.EVENTS_COMPARE (0)'Address);
   RTC_1_COMPARE_1     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC1_Periph.EVENTS_COMPARE (1)'Address);
   RTC_1_COMPARE_2     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC1_Periph.EVENTS_COMPARE (2)'Address);
   RTC_1_COMPARE_3     : constant Event_Type :=
     Event_Type (NRF51_SVD.RTC.RTC1_Periph.EVENTS_COMPARE (3)'Address);

   --  Watchdog task
   Watchdog_TIMEOUT    : constant Event_Type :=
     Event_Type (NRF51_SVD.WDT.WDT_Periph.EVENTS_TIMEOUT'Address);

   --  Random Number Genrator events
   RNG_VALRDY          : constant Event_Type :=
     Event_Type (NRF51_SVD.RNG.RNG_Periph.EVENTS_VALRDY'Address);

   --  Temperature events
   Temperature_DATARDY : constant Event_Type :=
     Event_Type (NRF51_SVD.TEMP.TEMP_Periph.EVENTS_DATARDY'Address);

   --  AES Electronic Codebook mode encryption (ECB) events
   ECB_END             : constant Event_Type :=
     Event_Type (NRF51_SVD.ECB.ECB_Periph.EVENTS_ENDECB'Address);
   ECB_ERROR           : constant Event_Type :=
     Event_Type (NRF51_SVD.ECB.ECB_Periph.EVENTS_ERRORECB'Address);

   --  AES CCM mode encryption (CCM) events
   CCM_KSGEN_END       : constant Event_Type :=
     Event_Type (NRF51_SVD.CCM.CCM_Periph.EVENTS_ENDKSGEN'Address);
   CCM_CRYPT_END       : constant Event_Type :=
     Event_Type (NRF51_SVD.CCM.CCM_Periph.EVENTS_ENDCRYPT'Address);
   CCM_ERROR           : constant Event_Type :=
     Event_Type (NRF51_SVD.CCM.CCM_Periph.EVENTS_ERROR'Address);

   --  Accelerated Address Resolver (AAR) events
   AAR_END             : constant Event_Type :=
     Event_Type (NRF51_SVD.AAR.AAR_Periph.EVENTS_END'Address);
   AAR_RESOLVED        : constant Event_Type :=
     Event_Type (NRF51_SVD.AAR.AAR_Periph.EVENTS_RESOLVED'Address);
   AAR_NOTRESOLVED     : constant Event_Type :=
     Event_Type (NRF51_SVD.AAR.AAR_Periph.EVENTS_NOTRESOLVED'Address);

   --  Serial Peripheral Interface (SPI) events
   SPI_0_READY         : constant Event_Type :=
     Event_Type (NRF51_SVD.SPI.SPI0_Periph.EVENTS_READY'Address);

   --  Serial Peripheral Interface (SPI) events
   SPI_1_READY         : constant Event_Type :=
     Event_Type (NRF51_SVD.SPI.SPI1_Periph.EVENTS_READY'Address);

   --  Two Wire Interface (TWI) 0 events
   TWI_0_STOPPED       : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI0_Periph.EVENTS_STOPPED'Address);
   TWI_0_RXDRDY        : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI0_Periph.EVENTS_RXDREADY'Address);
   TWI_0_TXDENT        : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI0_Periph.EVENTS_TXDSENT'Address);
   TWI_0_ERRO          : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI0_Periph.EVENTS_ERROR'Address);
   TWI_0_BB            : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI0_Periph.EVENTS_BB'Address);
   TWI_0_SUSPENDED     : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI0_Periph.EVENTS_SUSPENDED'Address);

   --  Two Wire Interface (TWI) 1 events
   TWI_1_STOPPED       : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI1_Periph.EVENTS_STOPPED'Address);
   TWI_1_RXDRDY        : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI1_Periph.EVENTS_RXDREADY'Address);
   TWI_1_TXDENT        : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI1_Periph.EVENTS_TXDSENT'Address);
   TWI_1_ERRO          : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI1_Periph.EVENTS_ERROR'Address);
   TWI_1_BB            : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI1_Periph.EVENTS_BB'Address);
   TWI_1_SUSPENDED     : constant Event_Type :=
     Event_Type (NRF51_SVD.TWI.TWI1_Periph.EVENTS_SUSPENDED'Address);

   --  Universal Asynchronous Receiver/Transmitter (UART) Events
   UART_RXDRDY         : constant Event_Type :=
     Event_Type (NRF51_SVD.UART.UART0_Periph.EVENTS_RXDRDY'Address);
   UART_TXDRDY         : constant Event_Type :=
     Event_Type (NRF51_SVD.UART.UART0_Periph.EVENTS_TXDRDY'Address);
   UART_ERROR          : constant Event_Type :=
     Event_Type (NRF51_SVD.UART.UART0_Periph.EVENTS_ERROR'Address);

   --  Quadrature Decoder (QDEC)
   QDEC_SAMPLERDY      : constant Event_Type :=
     Event_Type (NRF51_SVD.QDEC.QDEC_Periph.EVENTS_SAMPLERDY'Address);
   QDEC_REPORTRDY      : constant Event_Type :=
     Event_Type (NRF51_SVD.QDEC.QDEC_Periph.EVENTS_REPORTRDY'Address);
   QDEC_ACCOF          : constant Event_Type :=
     Event_Type (NRF51_SVD.QDEC.QDEC_Periph.EVENTS_ACCOF'Address);

   --  Analof to Digital Converter (ADC)
   ADC_END             : constant Event_Type :=
     Event_Type (NRF51_SVD.ADC.ADC_Periph.EVENTS_END'Address);

end nRF51.Events;
