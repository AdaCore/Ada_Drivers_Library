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

with Ada.Unchecked_Conversion;
with HAL.UART;                 use HAL.UART;
with HAL.Bitmap;               use HAL.Bitmap;

package body AdaFruit.Thermal_Printer is

   function To_Char is new Ada.Unchecked_Conversion (Source => UInt8,
                                                     Target => Character);
   function To_UInt8 is new Ada.Unchecked_Conversion (Source => Character,
                                                     Target => UInt8);

   procedure Write (This : in out TP_Device; Cmd : String);
   procedure Read (This : in out TP_Device; Str : out String);

   -----------
   -- Write --
   -----------

   procedure Write (This : in out TP_Device; Cmd : String) is
      Status : UART_Status;
      Data : UART_Data_8b (Cmd'Range);
   begin

      for Index in Cmd'Range loop
         Data (Index) := To_UInt8 (Cmd (Index));
      end loop;

      This.Port.Transmit (Data, Status);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
      --  This.Time.Delay_Microseconds ((11 * 1000000 / 19_2000) + Cmd'Length);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out TP_Device; Str : out String) is
      Status : UART_Status;
      Data : UART_Data_8b (Str'Range);
   begin
      This.Port.Receive (Data, Status);
      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      for Index in Str'Range loop
         Str (Index) := To_Char (Data (Index));
      end loop;
   end Read;

   ----------------------
   -- Set_Line_Spacing --
   ----------------------

   procedure Set_Line_Spacing (This : in out TP_Device; Spacing : UInt8) is
   begin
      Write (This, ASCII.ESC & '3' & To_Char (Spacing));
   end Set_Line_Spacing;

   ---------------
   -- Set_Align --
   ---------------

   procedure Set_Align (This : in out TP_Device; Align : Text_Align) is
      Mode : Character;
   begin
      case Align is
         when Left   => Mode := '0';
         when Center => Mode := '1';
         when Right  => Mode := '2';
      end case;
      Write (This, ASCII.ESC & 'a' & Mode);
   end Set_Align;

   ----------------------
   -- Set_Font_Enlarge --
   ----------------------

   procedure Set_Font_Enlarge (This : in out TP_Device; Height, Width : Boolean) is
      Data : UInt8 := 0;
   begin
      if Height then
         Data := Data or 16#01#;
      end if;
      if Width then
         Data := Data or 16#10#;
      end if;
      Write (This, ASCII.GS & '!' & To_Char (Data));
   end Set_Font_Enlarge;

   --------------
   -- Set_Bold --
   --------------

   procedure Set_Bold (This : in out TP_Device; Bold : Boolean) is
   begin
      Write (This, ASCII.ESC & 'E' & To_Char (if Bold then 1 else 0));
   end Set_Bold;

   ----------------------
   -- Set_Double_Width --
   ----------------------

   procedure Set_Double_Width (This : in out TP_Device; Double : Boolean) is
   begin
      if Double then
         Write (This, ASCII.ESC & ASCII.SO);
      else
         Write (This, ASCII.ESC & ASCII.DC4);
      end if;
   end Set_Double_Width;

   ----------------
   -- Set_UpDown --
   ----------------

   procedure Set_UpDown (This : in out TP_Device; UpDown : Boolean) is
   begin
      Write (This, ASCII.ESC & '{' & To_Char (if UpDown then 1 else 0));
   end Set_UpDown;

   ------------------
   -- Set_Reversed --
   ------------------

   procedure Set_Reversed (This : in out TP_Device; Reversed : Boolean) is
   begin
      Write (This, ASCII.GS & 'B' & To_Char (if Reversed then 1 else 0));
   end Set_Reversed;

   --------------------------
   -- Set_Underline_Height --
   --------------------------

   procedure Set_Underline_Height (This : in out TP_Device; Height : Underline_Height) is
   begin
      Write (This, ASCII.ESC & '-' & To_Char (Height));
   end Set_Underline_Height;

   -----------------------
   -- Set_Character_Set --
   -----------------------

   procedure Set_Character_Set (This : in out TP_Device; Set : Character_Set) is
   begin
      Write (This, ASCII.ESC & 't' & To_Char (Character_Set'Pos (Set)));
   end Set_Character_Set;

   ----------
   -- Feed --
   ----------

   procedure Feed (This : in out TP_Device; Rows : UInt8) is
   begin
      Write (This, ASCII.ESC & 'd' & To_Char (Rows));
   end Feed;

   ------------------
   -- Print_Bitmap --
   ------------------

   procedure Print_Bitmap (This : in out TP_Device;
                           BM   : Thermal_Printer_Bitmap)
   is
      Nbr_Of_Rows    : constant Natural := BM'Length (2);
      Nbr_Of_Columns : constant Natural := BM'Length (1);
      Str : String (1 .. Nbr_Of_Columns / 8);
   begin

      Write (This, ASCII.DC2 & 'v' &
               To_Char (UInt8 (Nbr_Of_Rows rem 256)) &
               To_Char (UInt8 (Nbr_Of_Rows / 256)));

      for Row in 0 .. Nbr_Of_Rows - 1 loop
         for Colum in 0 .. (Nbr_Of_Columns / 8) - 1 loop
            declare
               BM_Index  : constant Natural := BM'First (1) + Colum * 8;
               Str_Index : constant Natural := Str'First + Colum;
               B : UInt8 := 0;
            begin
               for X in 0 .. 7 loop
                  B := B or (if BM (BM_Index + X,
                                    BM'First (2) + Row)
                             then 2**X else 0);
               end loop;
               Str (Str_Index) := To_Char (B);
            end;
         end loop;
         Write (This, Str);

         This.Time.Delay_Microseconds (600 * Str'Length);
      end loop;
   end Print_Bitmap;

   ------------------
   -- Print_Bitmap --
   ------------------

   procedure Print_Bitmap (This : in out TP_Device;
                           BM   : not null HAL.Bitmap.Any_Bitmap_Buffer)
   is
      Nbr_Of_Rows    : constant Natural := BM.Height;
      Nbr_Of_Columns : constant Natural := BM.Width;
      Str : String (1 .. Nbr_Of_Columns / 8);
   begin

      Write (This, ASCII.DC2 & 'v' &
               To_Char (UInt8 (Nbr_Of_Rows rem 256)) &
               To_Char (UInt8 (Nbr_Of_Rows / 256)));

      for Row in 0 .. Nbr_Of_Rows - 1 loop
         for Colum in 0 .. (Nbr_Of_Columns / 8) - 1 loop
            declare
               B : UInt8 := 0;
               Str_Index : constant Natural := Str'First + Colum;
            begin
               for X in 0 .. 7 loop
                  B := B or (if BM.Pixel (((Colum * 8) + X, Row)).Red < 127
                             then 2**X else 0);
               end loop;
               Str (Str_Index) := To_Char (B);
            end;
         end loop;
         Write (This, Str);

         This.Time.Delay_Microseconds (600 * Str'Length);
      end loop;
   end Print_Bitmap;

   ----------
   -- Wake --
   ----------

   procedure Wake (This : in out TP_Device) is
   begin
      Write (This, ASCII.ESC & '@');
   end Wake;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out TP_Device) is
   begin
      Write (This, "" & To_Char (16#FF#));
      This.Time.Delay_Milliseconds (50);

      Write (This, ASCII.ESC & '8' & ASCII.NUL & ASCII.NUL);

      for X in 1 .. 10 loop
         This.Time.Delay_Microseconds (10_000);
         Write (This, "" & ASCII.NUL);
      end loop;

   end Reset;

   -----------
   -- Print --
   -----------

   procedure Print (This : in out TP_Device; Text : String) is
   begin
      Write (This, Text);
   end Print;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (This : in out TP_Device) return Printer_Status is
      Ret : Printer_Status;
      Status : String := "P1V72T30";
   begin
      Ret.Paper := False;
      Ret.Voltage := 0;
      Ret.Temperature := 0;
      Write (This, ASCII.ESC & 'v');
      Read (This, Status);

      --  Parse status here
      --  P<Paper>V<Voltage>T<Degree>
      --  Example: P1V72T30 Mean:Paper Ready,Current voltage 7.2V,Printer degree:30

      return Ret;
   end Get_Status;

   --------------------------
   -- Set_Printer_Settings --
   --------------------------

   procedure Set_Printer_Settings (This : in out TP_Device; Settings : Printer_Settings) is
   begin
      Write (This, ASCII.ESC & '7'
             & To_Char (Settings.Max_Printing_Dots)
             & To_Char (if Settings.Heating_Time < 3
               then 3 else Settings.Heating_Time)
             & To_Char (Settings.Heating_Interval));
   end Set_Printer_Settings;

   -----------------
   -- Set_Density --
   -----------------

   procedure Set_Density (This : in out TP_Device; Density, Breaktime : UInt8) is
   begin
      Write (This,
             ASCII.DC2 & '#' & To_Char (Shift_Left (Breaktime, 5) or Density));
   end Set_Density;

   ---------------------
   -- Print_Test_Page --
   ---------------------

   procedure Print_Test_Page (This : in out TP_Device) is
   begin
      Write (This, ASCII.DC2 & 'T');
   end Print_Test_Page;

end AdaFruit.Thermal_Printer;
