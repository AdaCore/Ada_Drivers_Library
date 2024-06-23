------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2024, AdaCore                     --
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

pragma Ada_2022;

with ILI9341.Regs; use ILI9341.Regs;

package body ILI9341.Device is

   ---------------------
   -- Disable_Display --
   ---------------------

   procedure Disable_Display (This : in out ILI9341_Device) is
   begin
      Send_Command (Connector, ILI9341_DISPLAY_OFF, []);
   end Disable_Display;

   --------------------
   -- Enable_Display --
   --------------------

   procedure Enable_Display (This : in out ILI9341_Device) is
   begin
      Send_Command (Connector, ILI9341_DISPLAY_ON, []);
   end Enable_Display;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : ILI9341_Device;
      Time : not null HAL.Time.Any_Delays)
   is
      Data renames Connector;
   begin
      Send_Command (Data, ILI9341_RESET, []);
      Time.Delay_Milliseconds (5);

      Send_Command
        (Data, ILI9341_POWERA, [16#39#, 16#2C#, 16#00#, 16#34#, 16#02#]);

      Send_Command (Data, ILI9341_POWERB, [16#00#, 16#C1#, 16#30#]);

      Send_Command (Data, ILI9341_DTCA, [16#85#, 16#00#, 16#78#]);
      Send_Command (Data, ILI9341_DTCB, [16#00#, 16#00#]);
      Send_Command (Data, ILI9341_POWER_SEQ, [16#64#, 16#03#, 16#12#, 16#81#]);
      Send_Command (Data, ILI9341_PRC, [16#20#]);
      Send_Command (Data, ILI9341_POWER1, [16#23#]);
      Send_Command (Data, ILI9341_POWER2, [16#10#]);
      Send_Command (Data, ILI9341_VCOM1, [16#3E#, 16#28#]);
      Send_Command (Data, ILI9341_VCOM2, [16#86#]);
      Send_Command (Data, ILI9341_MAC, [16#C8#]);
      --  FIXME: Doesn't match Portrait_1 ???
      Send_Command (Data, ILI9341_FRC, [16#00#, 16#18#]);

      case Connection is
         when RGB =>
            Send_Command (Data, ILI9341_RGB_INTERFACE, [16#C2#]);
            Send_Command (Data, ILI9341_INTERFACE, [16#01#, 16#00#, 16#06#]);
            Send_Command (Data, ILI9341_DFC, [16#0A#, 16#A7#, 16#27#, 16#04#]);
         when Serial | Parallel =>
            Send_Command (Data, ILI9341_PIXEL_FORMAT, [16#55#]);
            Send_Command (Data, ILI9341_DFC, [16#08#, 16#82#, 16#27#]);
      end case;

      Send_Command (Data, ILI9341_3GAMMA_EN, [16#00#]);

      Send_Command
        (Data, ILI9341_COLUMN_ADDR, [16#00#, 16#00#, 16#00#, 16#EF#]);

      Send_Command (Data, ILI9341_PAGE_ADDR, [16#00#, 16#00#, 16#01#, 16#3F#]);
      Send_Command (Data, ILI9341_GAMMA, [16#01#]);
      Send_Command
        (Data,
         ILI9341_PGAMMA,
         [16#0F#, 16#31#, 16#2B#, 16#0C#, 16#0E#, 16#08#, 16#4E#, 16#F1#,
          16#37#, 16#07#, 16#10#, 16#03#, 16#0E#, 16#09#, 16#00#]);

      Send_Command
        (Data,
         ILI9341_NGAMMA,
         [16#00#, 16#0E#, 16#14#, 16#03#, 16#11#, 16#07#, 16#31#, 16#C1#,
          16#48#, 16#08#, 16#0F#, 16#0C#, 16#31#, 16#36#, 16#0F#]);

      Send_Command (Data, ILI9341_SLEEP_OUT, []);

      Time.Delay_Milliseconds (150);

      Send_Command (Data, ILI9341_DISPLAY_ON, []);

      case Connection is
         when RGB =>
            Send_Command (Data, ILI9341_GRAM, []);
         when Serial | Parallel =>
            null;
      end case;
   end Initialize;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (This : in out ILI9341_Device;
      To   : Orientations)
   is
      Map : constant array (Orientations) of HAL.UInt8 :=
        [Portrait_1  => 16#58#,
         Portrait_2  => 16#88#,
         Landscape_1 => 16#28#,
         Landscape_2 => 16#E8#];

   begin
      if This.Orientation = To then
         return;
      end if;

      Send_Command (Connector, ILI9341_MAC, [Map (To)]);
      This.Orientation := To;
   end Set_Orientation;

end ILI9341.Device;
