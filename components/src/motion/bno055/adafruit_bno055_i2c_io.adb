------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with STM32.Device;   use STM32.Device;
with Ada.Real_Time;  use Ada.Real_Time;
with HAL;            use HAL;

package body AdaFruit_BNO055_I2C_IO is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (BNO055_Device : HAL.I2C.I2C_Address := BNO055_Primary_Address;
      Port          : access I2C_Port;
      SCL           : GPIO_Point;
      SCL_AF        : GPIO_Alternate_Function;
      SDA           : GPIO_Point;
      SDA_AF        : GPIO_Alternate_Function;
      Reset         : GPIO_Point)
   is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (SCL);
      Enable_Clock (SDA);
      Enable_Clock (Reset);

      Enable_Clock (Port.all);

      STM32.Device.Reset (Port.all);

      GPIO_Conf.Speed       := Speed_25MHz;
      GPIO_Conf.Mode        := Mode_Out;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors   := Floating;
      Configure_IO (Reset, GPIO_Conf);

      Configure_Alternate_Function (SCL, SCL_AF);
      Configure_Alternate_Function (SDA, SDA_AF);

      GPIO_Conf.Speed       := Speed_100MHz;
      GPIO_Conf.Mode        := Mode_AF;
      GPIO_Conf.Output_Type := Open_Drain;
      GPIO_Conf.Resistors   := Pull_Up;
      Configure_IO (SCL, GPIO_Conf);
      Configure_IO (SDA, GPIO_Conf);

      AdaFruit_BNO055_I2C_IO.Port := Port;
      AdaFruit_BNO055_I2C_IO.Reset := Reset;
      AdaFruit_BNO055_I2C_IO.Device := BNO055_Device * 2;
      --  shift left one bit since we're using 7-bit addresses
      --  TODO: change the library to do this shift automatically!

      STM32.I2C.Configure
        (Port.all,
         (Clock_Speed              => Selected_Clock_Speed,
          Addressing_Mode          => Addressing_Mode_7bit,
          General_Call_Enabled     => False,
          Clock_Stretching_Enabled => True,
          Own_Address              => 16#00#,
          others                   => <>));

      Set_State (Port.all, Enabled => True);
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read (Register : Register_Address;  Value : out Unsigned_8) is
      Incoming : HAL.I2C.I2C_Data (1 .. 1);
      Result   : HAL.I2C.I2C_Status;
      use type HAL.I2C.I2C_Status;
   begin
      Mem_Read (This          => Port.all,
                Addr          => Device,
                Mem_Addr      => UInt16 (Register),
                Mem_Addr_Size => HAL.I2C.Memory_Size_8b,
                Data          => Incoming,
                Status        => Result);
      if Result /= HAL.I2C.Ok then
         raise Program_Error with Result'Img;
      end if;
      Value := Incoming (1);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Register : Register_Address;  Value : Unsigned_8) is
      Outgoing : constant HAL.I2C.I2C_Data (1 .. 1) := (1 => Value);
      Result   : HAL.I2C.I2C_Status;
      use type HAL.I2C.I2C_Status;
   begin
      Mem_Write (This          => Port.all,
                 Addr          => Device,
                 Mem_Addr      => UInt16 (Register),
                 Mem_Addr_Size => HAL.I2C.Memory_Size_8b,
                 Data          => Outgoing,
                 Status        => Result);
      if Result /= HAL.I2C.Ok then
         raise Program_Error with "I2C write error:" & Result'Img;
      end if;
   end Write;

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer
     (Register : Register_Address;
      Value    : out HAL.I2C.I2C_Data)
   is
      Result   : HAL.I2C.I2C_Status;
      use type HAL.I2C.I2C_Status;
   begin
      Mem_Read (This          => Port.all,
                Addr          => Device,
                Mem_Addr      => UInt16 (Register),
                Mem_Addr_Size => HAL.I2C.Memory_Size_8b,
                Data          => Value,
                Status        => Result);
      if Result /= HAL.I2C.Ok then
         raise Program_Error with "I2C read error:" & Result'Img;
      end if;
   end Read_Buffer;

   -------------------------------
   -- Reset_BNO055_Via_Hardware --
   -------------------------------

   procedure Reset_BNO055_Via_Hardware is
   begin
      --  reset is active low
      STM32.GPIO.Clear (Reset);
      --  the BNO055 Datasheet, section 3.2, says 20ns is required
      delay until Clock + Milliseconds (1);
      STM32.GPIO.Set (Reset);
      delay until Clock + Milliseconds (650);  --  essential
      --  the time required after reset is the Power On Reset (POR) time
      --  specified in the Datasheet, table 0-2, "From Reset to Config mode"
   end Reset_BNO055_Via_Hardware;

end AdaFruit_BNO055_I2C_IO;
