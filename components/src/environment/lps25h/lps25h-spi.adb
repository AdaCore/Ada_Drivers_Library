------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2021, AdaCore                           --
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

with Ada.Unchecked_Conversion;
with Interfaces;

package body LPS25H.SPI is

   use HAL;

   --  SPI direction and multiplicity masks: 'or' these into the
   --  register to control the characteristics of the required
   --  transfer with the LPS25H.
   Read_Request       : constant := 2#1000_0000#;
   Transfer_Multiples : constant := 2#0100_0000#;

   --  Utility specs  --

   generic
      Register : HAL.UInt8;
      type Register_Type is private;
   procedure Write_Register (This   :     LPS25H_Barometric_Sensor_SPI;
                             Value  :     Register_Type;
                             Status : out Boolean);

   procedure Write
     (This   : LPS25H_Barometric_Sensor_SPI;
      Index  : HAL.UInt8;
      Data   : HAL.UInt8;
      Status : out Boolean);

   procedure Read
     (This   : LPS25H_Barometric_Sensor_SPI;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean);
   procedure Read
     (This   : LPS25H_Barometric_Sensor_SPI;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt8;
      Status : out Boolean);

   --------------
   -- Get_Data --
   --------------

   overriding
   procedure Get_Data
     (This   : in out LPS25H_Barometric_Sensor_SPI;
      Press  :    out Pressure;
      Temp   :    out Temperature;
      Asl    :    out Altitude;
      Status :    out Boolean)
   is
      Buf : HAL.UInt8_Array (0 .. 2) := (others => 0);
   begin
      --  Pressure
      declare
         type Integer_24 is range -(2 ** 23) .. 2 ** 23 - 1
         with Size => 24;
         subtype Buffer_3 is HAL.UInt8_Array (Buf'Range);
         function Convert is new Ada.Unchecked_Conversion
           (Buffer_3, Integer_24);
      begin
         --  bit 6 => read multiple bytes
         This.Read (PRESS_OUT_XL or Transfer_Multiples,
                    Buf,
                    Status);
         if not Status then
            return;
         end if;
         Press := Float (Convert (Buf)) / 4096.0;
      end;

      --  Temperature
      declare
         subtype Buffer_2 is HAL.UInt8_Array (0 .. 1);
         function Convert is new Ada.Unchecked_Conversion
           (Buffer_2, Interfaces.Integer_16);
      begin
         --  bit 6 => read multiple bytes
         This.Read (TEMP_OUT_L or Transfer_Multiples,
                    Buf (0 .. 1),
                    Status);
         if not Status then
            return;
         end if;
         Temp := 42.5 + Float (Convert (Buf (0 .. 1))) / 480.0;
      end;

      --  See Wikipedia, "Barometric formula": The pressure drops
      --  approximately by 11.3 pascals per meter in first 1000 meters
      --  above sea level.

      --  See Wikipedia, "Atmospheric pressure": the standard atmosphere is
      --  1013.25 mbar.

      --  1 Pascal = 0.01 mbar
      Asl := (1013.25 - Press) * (100.0 / 11.3);
   end Get_Data;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out LPS25H_Barometric_Sensor_SPI)
   is
      Data : UInt8;
      Status : Boolean;
   begin
      This.Timing.Delay_Milliseconds (5); -- ?
      This.Read (WHO_AM_I, Data, Status);
      if not Status then
         return;
      end if;
      if Data /= WAI_ID then
         return;
      end if;
      declare
         procedure Write_Ctrl_Reg1 is new Write_Register
           (CTRL_REG1, Ctrl_Reg1_Register);
      begin
         Write_Ctrl_Reg1 (This,
                          (PD => 1, ODR => Hz_25, BDU => 1, others => <>),
                          Status);
         if not Status then
            return;
         end if;
      end;
      This.Initialized := True;
   end Initialize;

   --  Utilities  --

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (This   :     LPS25H_Barometric_Sensor_SPI;
                             Value  :     Register_Type;
                             Status : out Boolean) is
      pragma Assert (Register_Type'Size = 8);
      function Convert is new Ada.Unchecked_Conversion
        (Register_Type, HAL.UInt8);
   begin
      Write (This,
             Index  => Register,
             Data   => Convert (Value),
             Status => Status);
   end Write_Register;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : LPS25H_Barometric_Sensor_SPI;
      Index  : HAL.UInt8;
      Data   : HAL.UInt8;
      Status : out Boolean)
   is
      Outcome : HAL.SPI.SPI_Status;
      use all type HAL.SPI.SPI_Status;
      Buf : constant HAL.SPI.SPI_Data_8b := (Index, Data);
   begin
      This.CS.Clear;
      This.Port.Transmit (Data   => Buf,
                          Status => Outcome);
      Status := Outcome = Ok;
      This.CS.Set;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : LPS25H_Barometric_Sensor_SPI;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean)
   is
      Outcome : HAL.SPI.SPI_Status;
      Buf : HAL.SPI.SPI_Data_8b (Data'Range);
      use all type HAL.SPI.SPI_Status;
   begin
      This.CS.Clear;
      --  bit 7 => read
      This.Port.Transmit
        (Data   => HAL.SPI.SPI_Data_8b'((1 => Index or Read_Request)),
         Status => Outcome);
      Status := Outcome = Ok;
      if Status then
         This.Port.Receive (Data   => Buf,
                            Status => Outcome);
         Status := Outcome = Ok;
         if Status then
            for J in Buf'Range loop
               Data (J) := Buf (J);
            end loop;
         end if;
      end if;
      This.CS.Set;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : LPS25H_Barometric_Sensor_SPI;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt8;
      Status : out Boolean)
   is
      Buf : UInt8_Array (1 .. 1);
   begin
      This.Read (Index  => Index,
                 Data   => Buf,
                 Status => Status);
      if Status then
         Data := Buf (1);
      end if;
   end Read;

end LPS25H.SPI;
