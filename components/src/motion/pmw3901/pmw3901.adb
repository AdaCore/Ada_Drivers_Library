------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
--           Copyright (C) 2020, Simon Wright (simon@pushface.org)          --
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
--     3. Neither the name of Pixart Imaging nor the names of its           --
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

--  The algorithms used here are derived from Pimoroni's code at
--  https://github.com/pimoroni/pmw3901-python.

with Ada.Unchecked_Conversion;
with STM32.Device;

package body PMW3901 is

   --  Some registers
   Reg_ID             : constant := 16#00#;
   Reg_ID_Inverted    : constant := 16#5f#;
   Reg_Data_Ready     : constant := 16#02#;
   Reg_Motion_Burst   : constant := 16#16#;
   Reg_Power_Up_Reset : constant := 16#3a#;
   --  Reg_Orientation    : constant := 16#5b#;

   Power_Up_Reset_Key : constant := 16#5a#;

   function Read (This     : in out PMW3901_Flow_Sensor;
                  Register :        HAL.UInt8) return HAL.UInt8;

   procedure Write (This     : in out PMW3901_Flow_Sensor;
                    Register :        HAL.UInt8;
                    Value    :        HAL.UInt8);

   function Is_Valid (M : Motion) return Boolean
   is
      use type HAL.UInt8;
   begin
      return M.Motion_Occurred
        and not (M.S_Qual < 19 and M.Shutter_Upper = 16#1f#);
   end Is_Valid;

   function Is_Clocked (This : STM32.SPI.SPI_Port'Class) return Boolean;
   --  It'd be good if this sort of function was declared in STM32.Device

   function Is_Clocked (This : PMW3901_Flow_Sensor) return Boolean
   is (Is_Clocked (This.Port.all));

   procedure Initialize (This : in out PMW3901_Flow_Sensor)
   is
   begin
      STM32.Device.Enable_Clock (This.CS.all);
      STM32.GPIO.Configure_IO
        (This.CS.all,
         (Mode        => STM32.GPIO.Mode_Out,
          Resistors   => STM32.GPIO.Floating, -- ?
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_50MHz));

      This.CS.Drive (False);
      This.Timing.Delay_Milliseconds (50);
      This.CS.Drive (True);

      declare
         Chip_ID          : HAL.UInt8;
         Revision         : HAL.UInt8;
         Inverted_Chip_ID : HAL.UInt8;
         use type HAL.UInt8;
      begin
         Chip_ID := Read (This, Reg_ID);
         Revision := Read (This, Reg_ID + 1);
         Inverted_Chip_ID := Read (This, Reg_ID_Inverted);
         if Chip_ID /= 16#49#
           or Revision /= 16#00#
           or Inverted_Chip_ID /= 16#b6#
         then
            --  Can't initialize the wrong sort of chip!
            return;
         end if;
      end;

      Write (This, Reg_Power_Up_Reset, Power_Up_Reset_Key);
      This.Timing.Delay_Milliseconds (20);

      --  Read the registers once
      declare
         Buff : STM32.SPI.UInt8_Buffer (0 .. 4);
         use type HAL.UInt8;
      begin
         for J in Buff'Range loop
            Buff (J) := Read (This, Reg_Data_Ready + HAL.UInt8 (J));
         end loop;
         null;
      end;

      This.Initialized := True;
   end Initialize;

   procedure Calibrate (This : in out PMW3901_Flow_Sensor)
   is
      type Magic is record
         Register : HAL.UInt8;
         Value : HAL.UInt8;
      end record;
      type Magics is array (Natural range <>) of Magic;
      procedure Write_Magics (Data : Magics);
      procedure Write_Magics (Data : Magics) is
      begin
         for J in Data'Range loop
            Write (This, Data (J).Register, Data (J).Value);
         end loop;
      end Write_Magics;
      use type HAL.UInt8;
   begin
      Write_Magics (((16#7f#, 16#00#),
                     (16#55#, 16#01#),
                     (16#50#, 16#07#),
                     (16#7f#, 16#0e#),
                     (16#43#, 16#10#)));
      declare
         Check : constant HAL.UInt8 := Read (This, 16#67#);
      begin
         if (Check and 2#1000_0000#) /= 0 then
            Write (This, 16#48#, 16#04#);
         else
            Write (This, 16#48#, 16#02#);
         end if;
      end;
      if Read (This, 16#73#) = 0 then
         declare
            C1, C2 : HAL.UInt8;
         begin
            C1 := Read (This, 16#70#);
            C2 := Read (This, 16#71#);
            if C1 <= 28 then
               C1 := C1 + 14;
            end if;
            if C1 > 28 then
               C1 := C1 + 11;
            end if;
            C2 := (C2 * 45) / 100;
            Write_Magics (((16#7f#, 16#00#),
                           (16#51#, 16#7b#),
                           (16#50#, 16#00#),
                           (16#55#, 16#00#),
                           (16#7f#, 16#0E#),
                           (16#70#, C1),
                           (16#71#, C2)));
         end;
      end if;
      Write_Magics (((16#7f#, 16#00#),
                     (16#61#, 16#ad#),
                     (16#7f#, 16#03#),
                     (16#40#, 16#00#),
                     (16#7f#, 16#05#),
                     (16#41#, 16#b3#),
                     (16#43#, 16#f1#),
                     (16#45#, 16#14#),
                     (16#5b#, 16#32#),
                     (16#5f#, 16#34#),
                     (16#7b#, 16#08#),
                     (16#7f#, 16#06#),
                     (16#44#, 16#1b#),
                     (16#40#, 16#bf#),
                     (16#4e#, 16#3f#),
                     (16#7f#, 16#08#),
                     (16#65#, 16#20#),
                     (16#6a#, 16#18#),
                     (16#7f#, 16#09#),
                     (16#4f#, 16#af#),
                     (16#5f#, 16#40#),
                     (16#48#, 16#80#),
                     (16#49#, 16#80#),
                     (16#57#, 16#77#),
                     (16#60#, 16#78#),
                     (16#61#, 16#78#),
                     (16#62#, 16#08#),
                     (16#63#, 16#50#),
                     (16#7f#, 16#0a#),
                     (16#45#, 16#60#),
                     (16#7f#, 16#00#),
                     (16#4d#, 16#11#),
                     (16#55#, 16#80#),
                     (16#74#, 16#21#),
                     (16#75#, 16#1f#),
                     (16#4a#, 16#78#),
                     (16#4b#, 16#78#),
                     (16#44#, 16#08#),
                     (16#45#, 16#50#),
                     (16#64#, 16#ff#),
                     (16#65#, 16#1f#),
                     (16#7f#, 16#14#),
                     (16#65#, 16#67#),
                     (16#66#, 16#08#),
                     (16#63#, 16#70#),
                     (16#7f#, 16#15#),
                     (16#48#, 16#48#),
                     (16#7f#, 16#07#),
                     (16#41#, 16#0d#),
                     (16#43#, 16#14#),
                     (16#4b#, 16#0e#),
                     (16#45#, 16#0f#),
                     (16#44#, 16#42#),
                     (16#4c#, 16#80#),
                     (16#7f#, 16#10#),
                     (16#5b#, 16#02#),
                     (16#7f#, 16#07#),
                     (16#40#, 16#41#),
                     (16#70#, 16#00#)));
      This.Timing.Delay_Milliseconds (16#0a#);
      Write_Magics (((16#32#, 16#44#),
                     (16#7f#, 16#07#),
                     (16#40#, 16#40#),
                     (16#7f#, 16#06#),
                     (16#62#, 16#f0#),
                     (16#63#, 16#00#),
                     (16#7f#, 16#0d#),
                     (16#48#, 16#c0#),
                     (16#6f#, 16#d5#),
                     (16#7f#, 16#00#),
                     (16#5b#, 16#a0#),
                     (16#4e#, 16#a8#),
                     (16#5a#, 16#50#),
                     (16#40#, 16#80#)));
      This.Timing.Delay_Milliseconds (16#f0#);
      --  enable LED pulsing???
      Write_Magics (((16#7f#, 16#14#),
                     (16#6f#, 16#1c#),
                     (16#7f#, 16#00#)));
   end Calibrate;

   function Read_Motion  (This : in out PMW3901_Flow_Sensor) return Motion is
      subtype Buffer is STM32.SPI.UInt8_Buffer (1 .. 12);
      function Convert is new Ada.Unchecked_Conversion (Buffer, Motion);
      Dummy : HAL.UInt8;
      Buff : Buffer := (others => 0);
   begin
      This.CS.Drive (False);
      This.Port.Transmit_Receive (Outgoing => Reg_Motion_Burst,
                                 Incoming => Dummy);
      pragma Warnings (Off, "referenced before it has a value");
      This.Port.Transmit_Receive (Outgoing => Buff,
                                 Incoming => Buff,
                                 Size => 12);
      pragma Warnings (On, "referenced before it has a value");
      This.CS.Drive (True);
      return Convert (Buff);
   end Read_Motion;

   function Read (This     : in out PMW3901_Flow_Sensor;
                  Register : HAL.UInt8) return HAL.UInt8
   is
      use type HAL.UInt8;
      Register_For_Read : constant HAL.UInt8 := Register and 16#7f#;
      Dummy : HAL.UInt8 := 42;
      Data : HAL.UInt8 := 43;
   begin
      This.CS.Drive (False);
      This.Port.Transmit_Receive (Outgoing => Register_For_Read,
                                  Incoming => Dummy);
      This.Port.Transmit_Receive (Outgoing => Dummy,
                                  Incoming => Data);
      This.CS.Drive (True);
      return Data;
   end Read;

   procedure Write (This     : in out PMW3901_Flow_Sensor;
                    Register :        HAL.UInt8;
                    Value    :        HAL.UInt8)
   is
      use type HAL.UInt8;
      Register_For_Write : constant HAL.UInt8 := Register or 16#80#;
      Dummy : HAL.UInt8 := 42;
   begin
      This.CS.Drive (False);
      This.Port.Transmit_Receive (Outgoing => Register_For_Write,
                                  Incoming => Dummy);
      This.Port.Transmit_Receive (Outgoing => Value,
                                 Incoming => Dummy);
      This.CS.Drive (True);
   end Write;

   --  => STM32.Device?
   function Is_Clocked (This : STM32.SPI.SPI_Port'Class) return Boolean
   is separate;

end PMW3901;
