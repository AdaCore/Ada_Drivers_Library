------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with STM32.Device; use STM32.Device;

package body Serial_IO is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Peripheral (Device : access Peripheral_Descriptor) is
      Configuration : GPIO_Port_Configuration;
      Device_Pins   : constant GPIO_Points := Device.Rx_Pin & Device.Tx_Pin;
   begin
      Enable_Clock (Device_Pins);
      Enable_Clock (Device.Transceiver.all);

      Configuration := (Mode           => Mode_AF,
                        AF             => Device.Transceiver_AF,
                        AF_Speed       => Speed_50MHz,
                        AF_Output_Type => Push_Pull,
                        Resistors      => Pull_Up);

      Configure_IO (Device_Pins, Configuration);
   end Initialize_Peripheral;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Device    : access Peripheral_Descriptor;
      Baud_Rate : Baud_Rates;
      Parity    : Parities     := No_Parity;
      Data_Bits : Word_Lengths := Word_Length_8;
      End_Bits  : Stop_Bits    := Stopbits_1;
      Control   : Flow_Control := No_Flow_Control)
   is
   begin
      Disable (Device.Transceiver.all);

      Set_Baud_Rate    (Device.Transceiver.all, Baud_Rate);
      Set_Mode         (Device.Transceiver.all, Tx_Rx_Mode);
      Set_Stop_Bits    (Device.Transceiver.all, End_Bits);
      Set_Word_Length  (Device.Transceiver.all, Data_Bits);
      Set_Parity       (Device.Transceiver.all, Parity);
      Set_Flow_Control (Device.Transceiver.all, Control);

      Enable (Device.Transceiver.all);
   end Configure;

end Serial_IO;
