------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2023, AdaCore                         --
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

with STM32.Board;
with STM32.Device;
with STM32.GPIO;

with Ravenscar_Time;

package body Display_ILI9341 is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Display) is
   begin
      STM32.Device.Enable_Clock (STM32.Board.TFT_BLK);

      STM32.Board.TFT_BLK.Configure_IO
        ((STM32.GPIO.Mode_Out,
         Resistors   => STM32.GPIO.Floating,
         Output_Type => STM32.GPIO.Push_Pull,
         Speed       => STM32.GPIO.Speed_100MHz));

      STM32.Board.TFT_BLK.Set;  --  Turn LCD backlight on

      STM32.Board.Initialize_FSMC (STM32.Board.TFT_Pins);

      STM32.FSMC.Configure
        (Bank_1 =>
           (1 =>  --  TFT is connected to sub-bank 1
              (Is_Set => True,
               Value  =>
                 (Write_Enable  => True,
                  Bus_Width     => STM32.FSMC.Half_Word,
                  Memory_Type   => STM32.FSMC.SRAM,
                  Bus_Turn      => 15,  --  90ns
                  Data_Setup    => 57, --  342ns
                  Address_Setup => 0,
                  Extended      =>
                    (STM32.FSMC.Mode_A,
                     Write_Bus_Turn      => 3,  --  18ns
                     Write_Data_Setup    => 2,  --  12ns
                     Write_Address_Setup => 0),
                  others        => <>)),
            others => <>));

      This.Device.Initialize (Ravenscar_Time.Delays);
      This.Set_Orientation (HAL.Framebuffer.Default);
   end Initialize;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (This        : in out Display;
      Orientation : HAL.Framebuffer.Display_Orientation) is
   begin
      This.Device.Set_Orientation
        (case Orientation is
            when HAL.Framebuffer.Landscape =>
              ILI9341_Device.Landscape_1,
            when others => ILI9341_Device.Portrait_2);
   end Set_Orientation;

end Display_ILI9341;
