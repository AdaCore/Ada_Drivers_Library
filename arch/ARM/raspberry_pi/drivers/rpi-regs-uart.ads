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

package RPi.Regs.UART is

   --  The UART_DR Register is the data register.
   --
   --  For words to be transmitted :
   --  * if the FIFOs are enabled, data written to this location is pushed onto
   --    the transmit FIFO.
   --  * if the FIFOs are not enabled, data is stored in the transmitter
   --    holding register (the bottom word of the transmit FIFO).
   --
   --  The write operation initiates transmission from the UART. The data is
   --  prefixed with a start bit, appended with the appropriate parity bit
   --  (if parity is enabled), and a stop bit. The resultant word is then
   --  transmitted.
   --
   --  For received words:
   --  * if the FIFOs are enabled, the data byte and the 4-bit status (break,
   --    frame, parity, and overrun) is pushed onto the 12-bit wide receive
   --    FIFO
   --  * if the FIFOs are not enabled, the data byte and status are stored in
   --    the receiving holding register (the bottom word of the receive FIFO).
   type DR_Register is record
      Data           : UInt8    := 0;
      FE             : Boolean := False; --  Framing Error
      PE             : Boolean := False; --  Parity Error
      BE             : Boolean := False; --  Break Error
      OE             : Boolean := False; --  Overrun Error
      Reserved_12_31 : UInt20  := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   --  The UART_RSRECR Register is the receive status register/error clear
   --  register. If the status is read from this register, then the status
   --  information for break, framing and parity corresponds to the data
   --  character read from the Data Register, UART_DR. The status information
   --  for overrun is set immediately when an overrun condition occurs. NOTE:
   --  The received data character must be read first from the Data Register,
   --  UART_DR on before reading the error status associated with that data
   --  character from this register.
   type RSRECR_Register is record
      FE       : Boolean := False;
      PE       : Boolean := False;
      BE       : Boolean := False;
      OE       : Boolean := False;
      Reserved : UInt28 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   type FR_Register is record
      CTS      : Boolean := False;
      DSR      : Boolean := False;
      DCD      : Boolean := False;
      BUSY     : Boolean := False;
      RXFE     : Boolean := False;
      TXFF     : Boolean := False;
      RXFF     : Boolean := False;
      TXFE     : Boolean := False;
      RI       : Boolean := False;
      Reserved : UInt23 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   --  The UART_IBRD Register is the integer part of the baud rate divisor
   --  value.
   type IBRD_Register is record
      IBRD     : UInt16 := 0;
      Reserved : UInt16 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   --  The UART_FBRD Register is the fractional part of the baud rate divisor
   --  value. The baud rate divisor is calculated as follows:
   --
   --  Baud rate divisor BAUDDIV = (FUARTCLK/(16 Baud rate))
   --
   --  where FUARTCLK is the UART reference clock frequency. The BAUDDIV is
   --  comprised of the integer value IBRD and the fractional value FBRD.
   --  NOTE: The contents of the IBRD and FBRD registers are not updated
   --  until transmission or reception of the current character is complete.
   type FBRD_Register is record
      FBRD     : UInt6 := 0;
      Reserved : UInt26 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   type UART_Word_Length is
     (Length_5_Bits,
      Length_6_Bits,
      Length_7_Bits,
      Length_8_Bits)
     with Size => 2;

   --  The UARTLCR_ LCRH Register is the line control register.
   --  NOTE: The UART_LCRH, UART_IBRD, and UART_FBRD registers must not be
   --  changed:
   --  * when the UART is enabled
   --  * when completing a transmission or a reception when it has been
   --    programmed to become disabled.
   type LCRH_Register is record
      BRK      : Boolean := False; --  Send break
      PEN      : Boolean := False; --  Parity enable
      EPS      : Boolean := False; --  Even parity select
      STP2     : Boolean := False; --  Two stop bits select
      FEN      : Boolean := False; --  Enable FIFO
      WLEN     : UART_Word_Length := Length_5_Bits;
      SPS      : Boolean := False; --  Stick Parity Select
      Reserved : UInt24 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   --  The UART_CR Register is the control register.
   --
   --  NOTE: To enable transmission, the TXE bit and UARTEN bit must be set to
   --  1. Similarly, to enable reception, the RXE bit and UARTEN bit, must be
   --  set to 1.
   --
   --  NOTE: Program the control registers as follows :
   --  1. Disable the UART.
   --  2. Wait for the end of transmission or reception of the current
   --     character.
   --  3. Flush the transmit FIFO by setting the FEN bit to 0 in the Line
   --     Control Register, UART_LCRH.
   --  4. Reprogram the Control Register, UART_CR.
   --  5. Enable the UART.
   type CR_Register is record
      UARTEN   : Boolean := False; --  UART enable
      SIREN    : Boolean := False; --  Unsupported
      SIRLP    : Boolean := False; --  Unsupported
      LBE      : Boolean := False; --  Loopback enable
      TXE      : Boolean := False; --  Transmit enable
      RXE      : Boolean := False; --  Receive enable
      DTR      : Boolean := False; --  Unsupported
      RTS      : Boolean := False; --  Request to send
      OUT1     : Boolean := False; --  Unsupported
      OUT2     : Boolean := False; --  Unsupported
      RTSEN    : Boolean := False; --  RTS Hw flow control enable
      CTSEN    : Boolean := False; --  CTS Hw flow control enable
      Reserved : UInt16 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   type UART_FIFO_Level is
     (FIFO_1_8_Full,
      FIFO_1_4_Full,
      FIFO_1_2_Full,
      FIFO_3_4_Full,
      FIFO_7_8_Full)
     with Size => 3;

   --  The UART_IFLS Register is the interrupt FIFO level select register.
   --  You can use this register to define the FIFO level that triggers the
   --  assertion of the combined interrupt signal.
   --
   --  The interrupts are generated based on a transition through a level
   --  rather than being based on the level. That is, the interrupts are
   --  generated when the fill level progresses through the trigger level.
   --
   --  The bits are reset so that the trigger level is when the FIFOs are at
   --  the half-way mark.
   type IFLS_Register is record
      --  Transmit interrupt FIFO level select.
      TXIFLSEL : UART_FIFO_Level := FIFO_1_8_Full;
      --  Receive interrupt FIFO Level select
      RXIFLSEL : UART_FIFO_Level := FIFO_1_8_Full;
      TXIFPSEL : UInt3 := 0; --  Unsupported
      RXIFPSEL : UInt3 := 0; --  Unsupported
      Reserved : UInt20 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   --  ??? TODO: Interrupt registers mapping

   type DMACR_Register is record
      RXDMAE   : Boolean := False;
      TXDMAE   : Boolean := False;
      DMAONERR : Boolean := False;
      Reserved : UInt29 := 0;
   end record with Volatile_Full_Access, Size => 32, Pack;

   type UART_Peripheral is record
      --  Data register
      DR     : aliased DR_Register;
      --  Receive status register/Error clear register
      RSRECR : aliased RSRECR_Register;
      --  Flag register
      FR     : aliased FR_Register;
      --  Not in use
      ILPR   : aliased UInt32;
      --  Integer Baud rate divisor
      IBRD   : aliased IBRD_Register;
      --  Fractional Baud rate divisor
      FBRD   : aliased FBRD_Register;
      --  Line Control Register
      LCRH   : aliased LCRH_Register;
      --  Control Register
      CR     : aliased CR_Register;
      --  Interupt FIFO Level Select Register
      IFLS   : aliased IFLS_Register;
      --  Interupt Mask Set Clear Register
      IMSC   : aliased UInt32;
      --  Raw Interupt Status Register
      RIS    : aliased UInt32;
      --  Masked Interupt Status Register
      MIS    : aliased UInt32;
      --  Interupt Clear Register
      ICR    : aliased UInt32;
      --  DMA Control Register
      DMACR  : aliased DMACR_Register;
      --  Test Control Register
      ITCR   : aliased UInt32;
      --  Integration test input register
      ITIP   : aliased UInt32;
      --  Integration test output register
      ITOP   : aliased UInt32;
      --  Test Data register
      TDR    : aliased UInt32;
   end record with Volatile;

   for UART_Peripheral use record
      DR     at 16#00# range 0 .. 31;
      RSRECR at 16#04# range 0 .. 31;
      FR     at 16#18# range 0 .. 31;
      ILPR   at 16#20# range 0 .. 31;
      IBRD   at 16#24# range 0 .. 31;
      FBRD   at 16#28# range 0 .. 31;
      LCRH   at 16#2C# range 0 .. 31;
      CR     at 16#30# range 0 .. 31;
      IFLS   at 16#34# range 0 .. 31;
      IMSC   at 16#38# range 0 .. 31;
      RIS    at 16#3C# range 0 .. 31;
      MIS    at 16#40# range 0 .. 31;
      ICR    at 16#44# range 0 .. 31;
      DMACR  at 16#48# range 0 .. 31;
      ITCR   at 16#80# range 0 .. 31;
      ITIP   at 16#84# range 0 .. 31;
      ITOP   at 16#88# range 0 .. 31;
      TDR    at 16#8C# range 0 .. 31;
   end record;

   UART_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (UART_Base);

end RPi.Regs.UART;
