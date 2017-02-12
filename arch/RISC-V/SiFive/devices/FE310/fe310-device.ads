------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with FE310.GPIO; use FE310.GPIO;
with FE310.UART; use FE310.UART;

with FE310_SVD;

package FE310.Device is

   P00 : aliased GPIO_Point (Pin => 00);
   P01 : aliased GPIO_Point (Pin => 01);
   P02 : aliased GPIO_Point (Pin => 02);
   P03 : aliased GPIO_Point (Pin => 03);
   P04 : aliased GPIO_Point (Pin => 04);
   P05 : aliased GPIO_Point (Pin => 05);
   P06 : aliased GPIO_Point (Pin => 06);
   P07 : aliased GPIO_Point (Pin => 07);
   P08 : aliased GPIO_Point (Pin => 08);
   P09 : aliased GPIO_Point (Pin => 09);
   P10 : aliased GPIO_Point (Pin => 10);
   P11 : aliased GPIO_Point (Pin => 11);
   P12 : aliased GPIO_Point (Pin => 12);
   P13 : aliased GPIO_Point (Pin => 13);
   P14 : aliased GPIO_Point (Pin => 14);
   P15 : aliased GPIO_Point (Pin => 15);
   P16 : aliased GPIO_Point (Pin => 16);
   P17 : aliased GPIO_Point (Pin => 17);
   P18 : aliased GPIO_Point (Pin => 18);
   P19 : aliased GPIO_Point (Pin => 19);
   P20 : aliased GPIO_Point (Pin => 20);
   P21 : aliased GPIO_Point (Pin => 21);
   P22 : aliased GPIO_Point (Pin => 22);
   P23 : aliased GPIO_Point (Pin => 23);
   P24 : aliased GPIO_Point (Pin => 24);
   P25 : aliased GPIO_Point (Pin => 25);
   P26 : aliased GPIO_Point (Pin => 26);
   P27 : aliased GPIO_Point (Pin => 27);
   P28 : aliased GPIO_Point (Pin => 28);
   P29 : aliased GPIO_Point (Pin => 29);
   P30 : aliased GPIO_Point (Pin => 30);
   P31 : aliased GPIO_Point (Pin => 31);

   IOF_UART0 : constant IO_Function := IOF0;
   IOF_UART1 : constant IO_Function := IOF0;
   IOF_QSPI1 : constant IO_Function := IOF0;
   IOF_QSPI2 : constant IO_Function := IOF0;
   IOF_PWM0  : constant IO_Function := IOF1;
   IOF_PWM1  : constant IO_Function := IOF1;
   IOF_PWM2  : constant IO_Function := IOF1;

   Internal_UART0 : aliased Internal_UART with Import, Volatile, Address => FE310_SVD.UART0_Base;
   Internal_UART1 : aliased Internal_UART with Import, Volatile, Address => FE310_SVD.UART1_Base;

   UART0 : aliased UART_Port (Internal_UART0'Access);
   UART1 : aliased UART_Port (Internal_UART1'Access);

end FE310.Device;
