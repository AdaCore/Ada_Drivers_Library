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

with HAL;        use HAL;
with HAL.UART;
with HAL.Time;
with HAL.Bitmap;

package AdaFruit.Thermal_Printer is

   type TP_Device (Port : not null HAL.UART.Any_UART_Port;
                   Time : not null HAL.Time.Any_Delays) is
     tagged private;

   --  The baud rate for this printers is usually 19_200 but some printers
   --  use 9_600.

   --  Sets the line spacing to n dots. The default value is 32
   procedure Set_Line_Spacing (This : in out TP_Device; Spacing : UInt8);

   type Text_Align is (Right, Center, Left);
   procedure Set_Align (This : in out TP_Device; Align : Text_Align);

   procedure Set_Font_Enlarge (This : in out TP_Device; Height, Width : Boolean);

   procedure Set_Bold (This : in out TP_Device; Bold : Boolean);

   procedure Set_Double_Width (This : in out TP_Device; Double : Boolean);

   procedure Set_UpDown (This : in out TP_Device; UpDown : Boolean);

   procedure Set_Reversed (This : in out TP_Device; Reversed : Boolean);

   subtype Underline_Height is UInt8 range 0 .. 2;
   procedure Set_Underline_Height (This : in out TP_Device; Height : Underline_Height);

   type Character_Set is (USA, France, Germany, UK, Denmark1, Sweden, Italy,
                          Spain1, Japan, Norway, Denmark2, Spain2,
                          Latin_Americam, Korea);
   procedure Set_Character_Set (This : in out TP_Device; Set : Character_Set);

   procedure Feed (This : in out TP_Device; Rows : UInt8);

   type Thermal_Printer_Bitmap is array (Natural range <>,
                                         Natural range <>) of Boolean
     with Pack;

   procedure Print_Bitmap (This : in out TP_Device;
                           BM   : Thermal_Printer_Bitmap)
     with Pre => BM'Length (1) = 384;

   procedure Print_Bitmap (This : in out TP_Device;
                           BM   : not null HAL.Bitmap.Any_Bitmap_Buffer)
     with Pre => BM.Width = 384;

   procedure Wake (This : in out TP_Device);
   procedure Reset (This : in out TP_Device);

   procedure Print (This : in out TP_Device; Text : String);

   type Printer_Status is record
      Paper       : Boolean;
      Voltage     : Integer;
      Temperature : Integer;
   end record;
   function Get_Status (This : in out TP_Device) return Printer_Status;

   type Printer_Settings is record
      --  Unit: 8 dots,  Default: 7 (64 dots))
      Max_Printing_Dots : UInt8;
      --  Unit 10us, Default: 80 (800us)
      Heating_Time      : UInt8;
      --  Unit 10us, Default: 2 (20us)
      Heating_Interval  : UInt8;
   end record;

   procedure Set_Density (This : in out TP_Device; Density, Breaktime : UInt8);
   procedure Set_Printer_Settings (This : in out TP_Device; Settings : Printer_Settings);

   procedure Print_Test_Page (This : in out TP_Device);

private
   type TP_Device (Port : not null HAL.UART.Any_UART_Port;
                   Time : not null HAL.Time.Any_Delays) is
     tagged null record;

end AdaFruit.Thermal_Printer;
