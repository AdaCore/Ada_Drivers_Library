------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
--         Copyright (C) 2020-2021, Simon Wright (simon@pushface.org)       --
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

--  The algorithms used here are derived from Pimoroni's code at
--  https://github.com/pimoroni/pmw3901-python.
--  Calibration data from PX4: https://github.com/PX4/PX4-Autopilot/
--   blob/master/src/drivers/optical_flow/pmw3901/PMW3901.cpp

with Ada.Unchecked_Conversion;

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

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (M : Motion) return Boolean
   is
      use type HAL.UInt8;
   begin
      return M.Motion_Occurred
        and not (M.S_Qual < 19 and M.Shutter_Upper = 16#1f#);
   end Is_Valid;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out PMW3901_Flow_Sensor)
   is
   begin
      This.CS.Clear;
      This.Timing.Delay_Milliseconds (50);
      This.CS.Set;

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
            raise SPI_Error with "no PMW3901 found";
         end if;
      end;

      Write (This, Reg_Power_Up_Reset, Power_Up_Reset_Key);
      This.Timing.Delay_Milliseconds (5);

      --  Read the registers once
      declare
         Buff : HAL.SPI.SPI_Data_8b (0 .. 4);
         use type HAL.UInt8;
      begin
         for J in Buff'Range loop
            Buff (J) := Read (This, Reg_Data_Ready + HAL.UInt8 (J));
         end loop;
      end;
      This.Timing.Delay_Milliseconds (1);

      This.Initialized := True;
   end Initialize;

   ---------------
   -- Calibrate --
   ---------------

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

      Write_Magics (((16#7f#, 16#00#),
                     (16#51#, 16#7b#),
                     (16#50#, 16#00#),
                     (16#55#, 16#00#),
                     (16#7f#, 16#0e#)));

      if Read (This, 16#73#) = 0 then
         declare
            C1, C2 : HAL.UInt8;
         begin
            C1 := Read (This, 16#70#);
            if C1 <= 28 then
               C1 := C1 + 14;
            else
               C1 := C1 + 11;
            end if;
            C1 := HAL.UInt8'Min (16#3f#, C1);
            C2 := Read (This, 16#71#);
            C2 := (C2 * 45) / 100;
            Write_Magics (((16#7f#, 16#00#),
                           (16#61#, 16#ad#),
                           (16#51#, 16#70#),
                           (16#7f#, 16#0E#),
                           (16#70#, C1),
                           (16#71#, C2)));
         end;
      end if;

      Write_Magics (((16#7F#, 16#00#),
                     (16#61#, 16#AD#),
                     (16#7F#, 16#03#),
                     (16#40#, 16#00#),
                     (16#7F#, 16#05#),
                     (16#41#, 16#B3#),
                     (16#43#, 16#F1#),
                     (16#45#, 16#14#),
                     (16#5B#, 16#32#),
                     (16#5F#, 16#34#),
                     (16#7B#, 16#08#),
                     (16#7F#, 16#06#),
                     (16#44#, 16#1B#),
                     (16#40#, 16#BF#),
                     (16#4E#, 16#3F#),
                     (16#7F#, 16#08#),
                     (16#65#, 16#20#),
                     (16#6A#, 16#18#),
                     (16#7F#, 16#09#),
                     (16#4F#, 16#AF#),
                     (16#5F#, 16#40#),
                     (16#48#, 16#80#),
                     (16#49#, 16#80#),
                     (16#57#, 16#77#),
                     (16#60#, 16#78#),
                     (16#61#, 16#78#),
                     (16#62#, 16#08#),
                     (16#63#, 16#50#),
                     (16#7F#, 16#0A#),
                     (16#45#, 16#60#),
                     (16#7F#, 16#00#),
                     (16#4D#, 16#11#),
                     (16#55#, 16#80#),
                     (16#74#, 16#21#),
                     (16#75#, 16#1F#),
                     (16#4A#, 16#78#),
                     (16#4B#, 16#78#),
                     (16#44#, 16#08#),
                     (16#45#, 16#50#),
                     (16#64#, 16#FF#),
                     (16#65#, 16#1F#),
                     (16#7F#, 16#14#),
                     (16#65#, 16#67#),
                     (16#66#, 16#08#),
                     (16#63#, 16#70#),
                     (16#7F#, 16#15#),
                     (16#48#, 16#48#),
                     (16#7F#, 16#07#),
                     (16#41#, 16#0D#),
                     (16#43#, 16#14#),
                     (16#4B#, 16#0E#),
                     (16#45#, 16#0F#),
                     (16#44#, 16#42#),
                     (16#4C#, 16#80#),
                     (16#7F#, 16#10#),
                     (16#5B#, 16#02#),
                     (16#7F#, 16#07#),
                     (16#40#, 16#41#),
                     (16#70#, 16#00#)));

      This.Timing.Delay_Milliseconds (10);

      Write_Magics (((16#32#, 16#44#),
                     (16#7F#, 16#07#),
                     (16#40#, 16#40#),
                     (16#7F#, 16#06#),
                     (16#62#, 16#F0#),
                     (16#63#, 16#00#),
                     (16#7F#, 16#0D#),
                     (16#48#, 16#C0#),
                     (16#6F#, 16#D5#),
                     (16#7F#, 16#00#),
                     (16#5B#, 16#A0#),
                     (16#4E#, 16#A8#),
                     (16#5A#, 16#50#),
                     (16#40#, 16#80#)));
   end Calibrate;

   -----------------
   -- Read_Motion --
   -----------------

   function Read_Motion  (This : in out PMW3901_Flow_Sensor) return Motion is
      subtype Buffer is HAL.SPI.SPI_Data_8b (1 .. 12);
      function Convert is new Ada.Unchecked_Conversion (Buffer, Motion);
      Buff : Buffer := (others => 0);
      Status : HAL.SPI.SPI_Status;
      use all type HAL.SPI.SPI_Status;
   begin
      This.CS.Clear;
      This.Port.Transmit (HAL.SPI.SPI_Data_8b'((1 => Reg_Motion_Burst)),
                          Status);
      if Status /= Ok then
         raise SPI_Error with "PMW3901 SPI transmit failure";
      end if;
      This.Port.Receive (Buff, Status);
      if Status /= Ok then
         raise SPI_Error with "PMW3901 SPI receive burst failure";
      end if;
      This.CS.Set;
      return Convert (Buff);
   end Read_Motion;

   ----------
   -- Read --
   ----------

   function Read (This     : in out PMW3901_Flow_Sensor;
                  Register : HAL.UInt8) return HAL.UInt8
   is
      use type HAL.UInt8;
      Register_For_Read : constant HAL.UInt8 := Register and 16#7f#;
      Data   : HAL.SPI.SPI_Data_8b (0 .. 0);
      Status : HAL.SPI.SPI_Status;
      use all type HAL.SPI.SPI_Status;
   begin
      This.CS.Clear;
      This.Port.Transmit (HAL.SPI.SPI_Data_8b'(1 => Register_For_Read),
                          Status);
      if Status /= Ok then
         raise SPI_Error with "PMW3901 SPI transmit failure";
      end if;
      This.Port.Receive (Data, Status);
      if Status /= Ok then
         raise SPI_Error with "PMW3901 SPI receive failure";
      end if;
      This.CS.Set;
      return Data (Data'First);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (This     : in out PMW3901_Flow_Sensor;
                    Register :        HAL.UInt8;
                    Value    :        HAL.UInt8)
   is
      use type HAL.UInt8;
      Register_For_Write : constant HAL.UInt8 := Register or 16#80#;
      Status : HAL.SPI.SPI_Status;
      use all type HAL.SPI.SPI_Status;
   begin
      This.CS.Clear;
      This.Port.Transmit (HAL.SPI.SPI_Data_8b'(1 => Register_For_Write),
                          Status);
      if Status /= Ok then
         raise SPI_Error with "PMW3901 SPI transmit failure";
      end if;
      This.Port.Transmit (HAL.SPI.SPI_Data_8b'(1 => Value),
                          Status);
      if Status /= Ok then
         raise SPI_Error with "PMW3901 SPI transmit failure";
      end if;
      This.CS.Set;
   end Write;

end PMW3901;
