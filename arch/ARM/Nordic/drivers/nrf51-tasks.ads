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
with NRF51_SVD.PPI;
with NRF51_SVD.RADIO;
with NRF51_SVD.TIMER;
with NRF51_SVD.RTC;
with NRF51_SVD.WDT;
with NRF51_SVD.RNG;
with NRF51_SVD.TEMP;
with NRF51_SVD.ECB;
with NRF51_SVD.CCM;
with NRF51_SVD.AAR;
with NRF51_SVD.TWI;
with NRF51_SVD.UART;
with NRF51_SVD.QDEC;
with NRF51_SVD.ADC;
with HAL; use HAL;


package nRF51.Tasks is

   procedure Trigger (T : Task_Type);
   --  Software task trigger

   function Get_Address (T : Task_Type) return System.Address;
   function Get_Address (T : Task_Type) return UInt32;

   --  Power management tasks
   Power_CONSTLAT   : constant Task_Type;
   Power_LOWPWR     : constant Task_Type;

   --  Clock tasks
   Clock_HFCLKSTART : constant Task_Type;
   Clock_HFCLKSTOP  : constant Task_Type;
   Clock_LFCLKSTART : constant Task_Type;
   Clock_LFCLKSTOP  : constant Task_Type;
   Clock_CAL        : constant Task_Type;
   Clock_CTSTART    : constant Task_Type;
   Clock_CTSTOP     : constant Task_Type;

   --  GPIO tasks
   GPIOTE_OUT_0     : constant Task_Type;
   GPIOTE_OUT_1     : constant Task_Type;
   GPIOTE_OUT_2     : constant Task_Type;
   GPIOTE_OUT_3     : constant Task_Type;

   --  Programmable Peripheral Interconnect tasks
   PPI_CHG_0_EN     : constant Task_Type;
   PPI_CHG_0_DIS    : constant Task_Type;
   PPI_CHG_1_EN     : constant Task_Type;
   PPI_CHG_1_DIS    : constant Task_Type;
   PPI_CHG_2_EN     : constant Task_Type;
   PPI_CHG_2_DIS    : constant Task_Type;
   PPI_CHG_3_EN     : constant Task_Type;
   PPI_CHG_3_DIS    : constant Task_Type;

   --  Radio tasks
   Radio_TXEN       : constant Task_Type;
   Radio_RXEN       : constant Task_Type;
   Radio_START      : constant Task_Type;
   Radio_STOP       : constant Task_Type;
   Radio_DISABLE    : constant Task_Type;
   Radio_RSSISTART  : constant Task_Type;
   Radio_RSSISTOP   : constant Task_Type;
   Radio_BCSTART    : constant Task_Type;
   Radio_BCSTOP     : constant Task_Type;

   --  Timer 0 tasks
   Timer_0_START      : constant Task_Type;
   Timer_0_STOP       : constant Task_Type;
   Timer_0_COUNT      : constant Task_Type;
   Timer_0_CLEAR      : constant Task_Type;
   Timer_0_CAPTURE_0  : constant Task_Type;
   Timer_0_CAPTURE_1  : constant Task_Type;
   Timer_0_CAPTURE_2  : constant Task_Type;
   Timer_0_CAPTURE_3  : constant Task_Type;

   --  Timer 1 tasks
   Timer_1_START      : constant Task_Type;
   Timer_1_STOP       : constant Task_Type;
   Timer_1_COUNT      : constant Task_Type;
   Timer_1_CLEAR      : constant Task_Type;
   Timer_1_CAPTURE_0  : constant Task_Type;
   Timer_1_CAPTURE_1  : constant Task_Type;
   Timer_1_CAPTURE_2  : constant Task_Type;
   Timer_1_CAPTURE_3  : constant Task_Type;

   --  Timer 2 tasks
   Timer_2_START      : constant Task_Type;
   Timer_2_STOP       : constant Task_Type;
   Timer_2_COUNT      : constant Task_Type;
   Timer_2_CLEAR      : constant Task_Type;
   Timer_2_CAPTURE_0  : constant Task_Type;
   Timer_2_CAPTURE_1  : constant Task_Type;
   Timer_2_CAPTURE_2  : constant Task_Type;
   Timer_2_CAPTURE_3  : constant Task_Type;

   --  RTC 0 tasks
   RTC_0_START          : constant Task_Type;
   RTC_0_STOP           : constant Task_Type;
   RTC_0_CLEAR          : constant Task_Type;
   RTC_0_TRIGOVRFLW     : constant Task_Type;

   --  RTC 1 tasks
   RTC_1_START          : constant Task_Type;
   RTC_1_STOP           : constant Task_Type;
   RTC_1_CLEAR          : constant Task_Type;
   RTC_1_TRIGOVRFLW     : constant Task_Type;

   --  Watchdog task
   Watchdog_START       : constant Task_Type;

   --  Random Number Genrator tasks
   RNG_START            : constant Task_Type;
   RNG_STOP             : constant Task_Type;

   --  Temperature tasks
   Temperature_START    : constant Task_Type;
   Temperature_STOP     : constant Task_Type;

   --  AES Electronic Codebook mode encryption (ECB) tasks
   ECB_START            : constant Task_Type;
   ECB_STOP             : constant Task_Type;

   --  AES CCM mode encryption (CCM) tasks
   CCM_KSGEN            : constant Task_Type;
   CCM_CRYPT            : constant Task_Type;
   CCM_STOP             : constant Task_Type;

   --  Accelerated Address Resolver (AAR) tasks
   AAR_START            : constant Task_Type;
   AAR_STOP             : constant Task_Type;

   --  Two Wire Interface (TWI) 0 tasks
   TWI_0_STARTRX          : constant Task_Type;
   TWI_0_STARTTX          : constant Task_Type;
   TWI_0_STOP             : constant Task_Type;
   TWI_0_SUSPEND          : constant Task_Type;
   TWI_0_RESUME           : constant Task_Type;

   --  Two Wire Interface (TWI) 1 tasks
   TWI_1_STARTRX          : constant Task_Type;
   TWI_1_STARTTX          : constant Task_Type;
   TWI_1_STOP             : constant Task_Type;
   TWI_1_SUSPEND          : constant Task_Type;
   TWI_1_RESUME           : constant Task_Type;

   --  Universal Asynchronous Receiver/Transmitter (UART) Tasks
   UART_STARTRX           : constant Task_Type;
   UART_STOPRX            : constant Task_Type;
   UART_STARTTX           : constant Task_Type;
   UART_STOPTX            : constant Task_Type;

   --  Quadrature Decoder (QDEC)
   QDEC_START             : constant Task_Type;
   QDEC_STOP              : constant Task_Type;
   QDEC_READCLRACC        : constant Task_Type;

   --  Analof to Digital Converter (ADC)
   ADC_START              : constant Task_Type;
   ADC_STOP               : constant Task_Type;
private

   --  Power management tasks
   Power_CONSTLAT : constant Task_Type :=
     Task_Type (NRF51_SVD.POWER.POWER_Periph.TASKS_CONSTLAT'Address);
   Power_LOWPWR   : constant Task_Type :=
     Task_Type (NRF51_SVD.POWER.POWER_Periph.TASKS_LOWPWR'Address);

   --  Clock tasks
   Clock_HFCLKSTART : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_HFCLKSTART'Address);
   Clock_HFCLKSTOP  : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_HFCLKSTOP'Address);
   Clock_LFCLKSTART : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_LFCLKSTART'Address);
   Clock_LFCLKSTOP  : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_LFCLKSTOP'Address);
   Clock_CAL        : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_CAL'Address);
   Clock_CTSTART    : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_CTSTART'Address);
   Clock_CTSTOP     : constant Task_Type :=
     Task_Type (NRF51_SVD.CLOCK.CLOCK_Periph.TASKS_CTSTOP'Address);

   --  GPIOTE tasks
   GPIOTE_OUT_0     : constant Task_Type :=
     Task_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.TASKS_OUT (0)'Address);
   GPIOTE_OUT_1     : constant Task_Type :=
     Task_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.TASKS_OUT (1)'Address);
   GPIOTE_OUT_2     : constant Task_Type :=
     Task_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.TASKS_OUT (2)'Address);
   GPIOTE_OUT_3     : constant Task_Type :=
     Task_Type (NRF51_SVD.GPIOTE.GPIOTE_Periph.TASKS_OUT (3)'Address);

   --  Programmable Peripheral Interconnect Tasks
   PPI_CHG_0_EN     : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (0).EN'Address);
   PPI_CHG_0_DIS    : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (0).DIS'Address);
   PPI_CHG_1_EN     : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (1).EN'Address);
   PPI_CHG_1_DIS    : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (1).DIS'Address);
   PPI_CHG_2_EN     : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (2).EN'Address);
   PPI_CHG_2_DIS    : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (2).DIS'Address);
   PPI_CHG_3_EN     : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (3).EN'Address);
   PPI_CHG_3_DIS    : constant Task_Type :=
     Task_Type (NRF51_SVD.PPI.PPI_Periph.TASKS_CHG (3).DIS'Address);

   --  Radio tasks
   Radio_TXEN       : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_TXEN'Address);
   Radio_RXEN       : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_RXEN'Address);
   Radio_START      : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_START'Address);
   Radio_STOP       : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_STOP'Address);
   Radio_DISABLE    : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_DISABLE'Address);
   Radio_RSSISTART  : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_RSSISTART'Address);
   Radio_RSSISTOP   : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_RSSISTOP'Address);
   Radio_BCSTART    : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_BCSTART'Address);
   Radio_BCSTOP     : constant Task_Type :=
     Task_Type (NRF51_SVD.RADIO.RADIO_Periph.TASKS_BCSTOP'Address);

   --  Timer 0 tasks
   Timer_0_START      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_STOP       : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_COUNT      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_CLEAR      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_CAPTURE_0  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_CAPTURE_1  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_CAPTURE_2  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);
   Timer_0_CAPTURE_3  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER0_Periph.TASKS_START'Address);

   --  Timer 1 tasks
   Timer_1_START      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_STOP       : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_COUNT      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_CLEAR      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_CAPTURE_0  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_CAPTURE_1  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_CAPTURE_2  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);
   Timer_1_CAPTURE_3  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER1_Periph.TASKS_START'Address);

   --  Timer 2 tasks
   Timer_2_START      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_STOP       : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_COUNT      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_CLEAR      : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_CAPTURE_0  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_CAPTURE_1  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_CAPTURE_2  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);
   Timer_2_CAPTURE_3  : constant Task_Type :=
     Task_Type (NRF51_SVD.TIMER.TIMER2_Periph.TASKS_START'Address);

   --  RTC 0 tasks
   RTC_0_START          : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC0_Periph.TASKS_START'Address);
   RTC_0_STOP           : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC0_Periph.TASKS_STOP'Address);
   RTC_0_CLEAR          : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC0_Periph.TASKS_CLEAR'Address);
   RTC_0_TRIGOVRFLW     : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC0_Periph.TASKS_TRIGOVRFLW'Address);

   --  RTC 1 tasks
   RTC_1_START          : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC1_Periph.TASKS_START'Address);
   RTC_1_STOP           : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC1_Periph.TASKS_STOP'Address);
   RTC_1_CLEAR          : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC1_Periph.TASKS_CLEAR'Address);
   RTC_1_TRIGOVRFLW     : constant Task_Type :=
     Task_Type (NRF51_SVD.RTC.RTC1_Periph.TASKS_TRIGOVRFLW'Address);

   --  Watchdog tasks
   Watchdog_START       : constant Task_Type :=
     Task_Type (NRF51_SVD.WDT.WDT_Periph.TASKS_START'Address);

      --  Random Number Genrator tasks
   RNG_START            : constant Task_Type :=
     Task_Type (NRF51_SVD.RNG.RNG_Periph.TASKS_START'Address);
   RNG_STOP             : constant Task_Type :=
     Task_Type (NRF51_SVD.RNG.RNG_Periph.TASKS_START'Address);

   --  Temperature tasks
   Temperature_START    : constant Task_Type :=
     Task_Type (NRF51_SVD.TEMP.TEMP_Periph.TASKS_START'Address);
   Temperature_STOP     : constant Task_Type :=
     Task_Type (NRF51_SVD.TEMP.TEMP_Periph.TASKS_STOP'Address);

   --  AES Electronic Codebook mode encryption (ECB) tasks
   ECB_START            : constant Task_Type :=
     Task_Type (NRF51_SVD.ECB.ECB_Periph.TASKS_STARTECB'Address);
   ECB_STOP             : constant Task_Type :=
     Task_Type (NRF51_SVD.ECB.ECB_Periph.TASKS_STOPECB'Address);

   --  AES CCM mode encryption (CCM) tasks
   CCM_KSGEN            : constant Task_Type :=
     Task_Type (NRF51_SVD.CCM.CCM_Periph.TASKS_KSGEN'Address);
   CCM_CRYPT            : constant Task_Type :=
     Task_Type (NRF51_SVD.CCM.CCM_Periph.TASKS_CRYPT'Address);
   CCM_STOP             : constant Task_Type :=
     Task_Type (NRF51_SVD.CCM.CCM_Periph.TASKS_STOP'Address);

   --  Accelerated Address Resolver (AAR) tasks
   AAR_START            : constant Task_Type :=
     Task_Type (NRF51_SVD.AAR.AAR_Periph.TASKS_START'Address);
   AAR_STOP             : constant Task_Type :=
     Task_Type (NRF51_SVD.AAR.AAR_Periph.TASKS_STOP'Address);

   --  Two Wire Interface (TWI) 0 tasks
   TWI_0_STARTRX          : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI0_Periph.TASKS_STARTRX'Address);
   TWI_0_STARTTX          : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI0_Periph.TASKS_STARTTX'Address);
   TWI_0_STOP             : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI0_Periph.TASKS_STOP'Address);
   TWI_0_SUSPEND          : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI0_Periph.TASKS_SUSPEND'Address);
   TWI_0_RESUME           : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI0_Periph.TASKS_RESUME'Address);

   --  Two Wire Interface (TWI) 1 tasks
   TWI_1_STARTRX          : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI1_Periph.TASKS_STARTRX'Address);
   TWI_1_STARTTX          : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI1_Periph.TASKS_STARTTX'Address);
   TWI_1_STOP             : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI1_Periph.TASKS_STOP'Address);
   TWI_1_SUSPEND          : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI1_Periph.TASKS_SUSPEND'Address);
   TWI_1_RESUME           : constant Task_Type :=
     Task_Type (NRF51_SVD.TWI.TWI1_Periph.TASKS_RESUME'Address);

   --  Universal Asynchronous Receiver/Transmitter (UART) Tasks
   UART_STARTRX           : constant Task_Type :=
     Task_Type (NRF51_SVD.UART.UART0_Periph.TASKS_STARTRX'Address);
   UART_STOPRX            : constant Task_Type :=
     Task_Type (NRF51_SVD.UART.UART0_Periph.TASKS_STOPRX'Address);
   UART_STARTTX           : constant Task_Type :=
     Task_Type (NRF51_SVD.UART.UART0_Periph.TASKS_STARTTX'Address);
   UART_STOPTX            : constant Task_Type :=
     Task_Type (NRF51_SVD.UART.UART0_Periph.TASKS_STOPTX'Address);

   --  Quadrature Decoder (QDEC)
   QDEC_START             : constant Task_Type :=
     Task_Type (NRF51_SVD.QDEC.QDEC_Periph.TASKS_START'Address);
   QDEC_STOP              : constant Task_Type :=
     Task_Type (NRF51_SVD.QDEC.QDEC_Periph.TASKS_STOP'Address);
   QDEC_READCLRACC        : constant Task_Type :=
     Task_Type (NRF51_SVD.QDEC.QDEC_Periph.TASKS_READCLRACC'Address);

   --  Analof to Digital Converter (ADC)
   ADC_START              : constant Task_Type :=
     Task_Type (NRF51_SVD.ADC.ADC_Periph.TASKS_START'Address);
   ADC_STOP               : constant Task_Type :=
     Task_Type (NRF51_SVD.ADC.ADC_Periph.TASKS_STOP'Address);

end nRF51.Tasks;
