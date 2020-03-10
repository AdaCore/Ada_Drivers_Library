------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016-2020, AdaCore                      --
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

with nRF.GPIO;       use nRF.GPIO;
with nRF.RTC;        use nRF.RTC;
with NRF_SVD.RTC;
with nRF.TWI;        use nRF.TWI;
with NRF_SVD.TWI;
with nRF.SPI_Master; use nRF.SPI_Master;
with NRF_SVD.SPI;
with nRF.Timers;     use nRF.Timers;
with NRF_SVD.TIMER;
with nRF.UART;       use nRF.UART;
with NRF_SVD.UART;

package nRF.Device is
   pragma Elaborate_Body;

   P00 : aliased GPIO_Point := (Pin => 00);
   P01 : aliased GPIO_Point := (Pin => 01);
   P02 : aliased GPIO_Point := (Pin => 02);
   P03 : aliased GPIO_Point := (Pin => 03);
   P04 : aliased GPIO_Point := (Pin => 04);
   P05 : aliased GPIO_Point := (Pin => 05);
   P06 : aliased GPIO_Point := (Pin => 06);
   P07 : aliased GPIO_Point := (Pin => 07);
   P08 : aliased GPIO_Point := (Pin => 08);
   P09 : aliased GPIO_Point := (Pin => 09);
   P10 : aliased GPIO_Point := (Pin => 10);
   P11 : aliased GPIO_Point := (Pin => 11);
   P12 : aliased GPIO_Point := (Pin => 12);
   P13 : aliased GPIO_Point := (Pin => 13);
   P14 : aliased GPIO_Point := (Pin => 14);
   P15 : aliased GPIO_Point := (Pin => 15);
   P16 : aliased GPIO_Point := (Pin => 16);
   P17 : aliased GPIO_Point := (Pin => 17);
   P18 : aliased GPIO_Point := (Pin => 18);
   P19 : aliased GPIO_Point := (Pin => 19);
   P20 : aliased GPIO_Point := (Pin => 20);
   P21 : aliased GPIO_Point := (Pin => 21);
   P22 : aliased GPIO_Point := (Pin => 22);
   P23 : aliased GPIO_Point := (Pin => 23);
   P24 : aliased GPIO_Point := (Pin => 24);
   P25 : aliased GPIO_Point := (Pin => 25);
   P26 : aliased GPIO_Point := (Pin => 26);
   P27 : aliased GPIO_Point := (Pin => 27);
   P28 : aliased GPIO_Point := (Pin => 28);
   P29 : aliased GPIO_Point := (Pin => 29);
   P30 : aliased GPIO_Point := (Pin => 30);
   P31 : aliased GPIO_Point := (Pin => 31);

   RTC_0 : aliased Real_Time_Counter (NRF_SVD.RTC.RTC0_Periph'Access);
   RTC_1 : aliased Real_Time_Counter (NRF_SVD.RTC.RTC1_Periph'Access);
   RTC_2 : aliased Real_Time_Counter (NRF_SVD.RTC.RTC2_Periph'Access);


   --  Be carefull of shared resources between the TWI and SPI controllers.
   --  TWI_O and SPI_Master_0 cannot be used at the same time.
   --  TWI_1 and SPI_Master_1 cannot be used at the same time.
   --
   --  See nRF Series Reference Manual, chapter Memory.Instantiation.

   TWI_0 : aliased TWI_Master (NRF_SVD.TWI.TWI0_Periph'Access);
   TWI_1 : aliased TWI_Master (NRF_SVD.TWI.TWI1_Periph'Access);


   SPI_Master_0 : aliased nRF.SPI_Master.SPI_Master (NRF_SVD.SPI.SPI0_Periph'Access);
   SPI_Master_1 : aliased nRF.SPI_Master.SPI_Master (NRF_SVD.SPI.SPI1_Periph'Access);
   SPI_Master_2 : aliased nRF.SPI_Master.SPI_Master (NRF_SVD.SPI.SPI2_Periph'Access);


   Timer_0 : aliased Timer (NRF_SVD.TIMER.TIMER0_Periph'Access);
   Timer_1 : aliased Timer (NRF_SVD.TIMER.TIMER1_Periph'Access);
   Timer_2 : aliased Timer (NRF_SVD.TIMER.TIMER2_Periph'Access);
   Timer_3 : aliased Timer (NRF_SVD.TIMER.TIMER3_Periph'Access);
   Timer_4 : aliased Timer (NRF_SVD.TIMER.TIMER4_Periph'Access);


   UART_0 : aliased UART_Device (NRF_SVD.UART.UART0_Periph'Access);
end nRF.Device;
