------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2019, AdaCore                      --
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

package body SiFive.UART is

   -------------------
   -- Set_Stop_Bits --
   -------------------

   procedure Set_Stop_Bits (This : in out UART_Device; To : Stop_Bits) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);

   begin
      Periph.TXCTRL.NSTOP := (case To is
                                 when Stopbits_1 => False,
                                 when Stopbits_2 => True);
   end Set_Stop_Bits;

   -------------------
   -- Set_Baud_Rate --
   -------------------

   procedure Set_Baud_Rate (This          : in out UART_Device;
                            CPU_Frequency : UInt32;
                            To            : Baud_Rates)
   is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.DIV.DIV := UInt16 (CPU_Frequency / (To - 1));
   end Set_Baud_Rate;

   ---------------
   -- Enable_RX --
   ---------------

   procedure Enable_RX (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.RXCTRL.ENABLE := True;
   end Enable_RX;

   ---------------
   -- Enable_TX --
   ---------------

   procedure Enable_TX (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.TXCTRL.ENABLE := True;
   end Enable_TX;

   ----------------
   -- Disable_RX --
   ----------------

   procedure Disable_RX (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.RXCTRL.ENABLE := False;
   end Disable_RX;

   ----------------
   -- Disable_TX --
   ----------------

   procedure Disable_TX (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.TXCTRL.ENABLE := False;
   end Disable_TX;

   --------------------------
   -- RX_Interrupt_Pending --
   --------------------------

   function RX_Interrupt_Pending (This : UART_Device) return Boolean
   is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      return Periph.IP.RXWM;
   end RX_Interrupt_Pending;

   --------------------------
   -- TX_Interrupt_Pending --
   --------------------------

   function TX_Interrupt_Pending (This : UART_Device) return Boolean
   is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      return Periph.IP.TXWM;
   end TX_Interrupt_Pending;

   -------------------------
   -- Enable_RX_Interrupt --
   -------------------------

   procedure Enable_RX_Interrupt (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.IE.RXWM := True;
   end Enable_RX_Interrupt;

   -------------------------
   -- Enable_TX_Interrupt --
   -------------------------

   procedure Enable_TX_Interrupt (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.IE.TXWM := True;
   end Enable_TX_Interrupt;


   --------------------------
   -- Disable_RX_Interrupt --
   --------------------------

   procedure Disable_RX_Interrupt (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.IE.RXWM := False;
   end Disable_RX_Interrupt;

   --------------------------
   -- Disable_TX_Interrupt --
   --------------------------

   procedure Disable_TX_Interrupt (This : in out UART_Device) is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.IE.TXWM := False;
   end Disable_TX_Interrupt;

   ------------------------------
   -- Set_Interrupt_Thresholds --
   ------------------------------

   procedure Set_Interrupt_Thresholds (This   : in out UART_Device;
                                       RX, TX : UInt3)
   is
      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      Periph.TXCTRL.TXCNT := TX;
      Periph.RXCTRL.RXCNT := RX;
   end Set_Interrupt_Thresholds;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);

      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);
   begin
      for Elt of Data loop
         while Periph.TXDATA.FULL loop
            null;
         end loop;

         Periph.TXDATA.DATA := Elt;
      end loop;
      Status := Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
   begin
      raise Program_Error with "FE310 UART only support 8bit mode";
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);

      Periph : aliased UART_Peripheral
        with Import, Address => System'To_Address (This.Base_Address);

      Data_Reg : RXDATA_Register;
   begin
      for Elt of Data loop
         loop
            Data_Reg := Periph.RXDATA;
            exit when not Data_Reg.EMPTY;
         end loop;
         Elt := Data_Reg.DATA;
      end loop;
      Status := Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
   begin
      raise Program_Error with "FE310 UART only support 8bit mode";
   end Receive;

end SiFive.UART;
