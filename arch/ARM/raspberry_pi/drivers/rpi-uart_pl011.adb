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

with RPi.Regs.UART;        use RPi.Regs.UART;

with RPi.Device;           use RPi.Device;
with RPi.GPIO;             use RPi.GPIO;

package body RPi.UART_PL011 is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Pins       : UART_Pins;
      UART_Clock : UInt32;
      Baud_Rate  : UInt32)
   is
      Baud16   : constant UInt32 := Baud_Rate * 16;
      Int_Div  : constant UInt32 := UART_Clock / Baud16;
      Frac2    : constant UInt32 := (UART_Clock mod Baud16) * 8 / Baud_Rate;
      Frac_Div : constant UInt32 := Frac2 / 2 + Frac2 mod 2;

   begin
      case Pins is
         when P14_17 =>
            RPi.GPIO.Configure (GPIO_Points'(P14, P16), Mode_AF0);
            RPi.GPIO.Configure (GPIO_Points'(P16, P17), Mode_AF3);
         when P30_33 =>
            RPi.GPIO.Configure (GPIO_Points'(P30, P31, P32, P33), Mode_AF3);
         when P36_39 =>
            RPi.GPIO.Configure (GPIO_Points'(P36, P35, P38, P39), Mode_AF2);
      end case;

      UART_Periph.IMSC := 0;
      UART_Periph.ICR  := 16#7FF#;
      UART_Periph.IBRD.IBRD := UInt16 (Int_Div);
      UART_Periph.FBRD.FBRD := UInt6 (Frac_Div);

      --  ??? TODO: Below should be generic, e.g. using user parameters
      UART_Periph.LCRH.WLEN := Length_8_Bits;
      UART_Periph.LCRH.PEN  := False; --  HCI transport requires no parity
      UART_Periph.LCRH.STP2 := False; --  HCI transport requires 1 stop bit

      UART_Periph.IFLS      := (RXIFLSEL => FIFO_1_4_Full,
                                others => <>);
      UART_Periph.CR        := (UARTEN => True,
                                TXE    => True,
                                RXE    => True,
                                RTSEN  => True, --  Allow hardware flow ctrl
                                CTSEN  => True, --        "
                                others => <>);
   end Configure;

   -----------
   -- Write --
   -----------

   procedure Write (Data : UInt8_Array)
   is
   begin
      for J in Data'Range loop
         Write (Data (J));
      end loop;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (Data : UInt8)
   is
   begin
      while UART_Periph.FR.TXFF loop
         --  FIFO Full Flag
         null;
      end loop;

      UART_Periph.DR.Data := Data;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (Data : out UInt8_Array)
   is
   begin
      for J in Data'Range loop
         Data (J) := Read;
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   function Read return UInt8
   is
   begin
      while UART_Periph.FR.RXFE loop
         --  RX FIFO Empty
         null;
      end loop;

      return UART_Periph.DR.Data;
   end Read;

end RPi.UART_PL011;
