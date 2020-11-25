------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2018-2019, AdaCore                      --
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

with MicroBit.I2C;
with MicroBit.Display;
with MicroBit.Display.Symbols;
with MicroBit.Time;

package body MicroBit.Accelerometer is

   Acc  : MMA8653.MMA8653_Accelerometer (MicroBit.I2C.Controller);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not MicroBit.I2C.Initialized then
         MicroBit.I2C.Initialize;
      end if;

      if not Acc.Check_Device_Id then

         --  The expected accelerometer is not deteced. Maybe this is running on
         --  a micro:bit v1.5 with a different accelerometer.

         --  Show a blinking frown face...
         Display.Clear;
         loop
            Display.Symbols.Frown;
            Time.Delay_Ms (500);
            Display.Clear;
            Time.Delay_Ms (500);
         end loop;
      end if;

      Acc.Configure (MMA8653.Two_G,
                     MMA8653.High_Resolution,
                     MMA8653.High_Resolution);
   end Initialize;

   ----------
   -- Data --
   ----------

   function Data return MMA8653.All_Axes_Data
   is (Acc.Read_Data);

begin
   Initialize;
end MicroBit.Accelerometer;
