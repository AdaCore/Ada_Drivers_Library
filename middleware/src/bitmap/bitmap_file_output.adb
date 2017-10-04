------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Interfaces; use Interfaces;
with HAL;        use HAL;

package body Bitmap_File_Output is

   type Header (As_Array : Boolean := True) is record
      case As_Array is
         when True =>
            Arr : UInt8_Array (1 .. 14);
         when False =>
            Signature : Integer_16;
            Size      : Integer_32; --  File size
            Reserved1 : Integer_16;
            Reserved2 : Integer_16;
            Offset    : Integer_32; --  Data offset
      end case;
   end record with Unchecked_Union, Pack, Size => 14 * 8;

   type Info (As_Array : Boolean := True) is record
      case As_Array is
         when True =>
            Arr : UInt8_Array (1 .. 40);
         when False =>
            Struct_Size   : Integer_32;
            Width         : Integer_32; -- Image width in pixels
            Height        : Integer_32; -- Image hieght in pixels
            Planes        : Integer_16;
            Pixel_Size    : Integer_16; -- Bits per pixel
            Compression   : Integer_32; -- Zero means no compression
            Image_Size    : Integer_32; -- Size of the image data in UInt8s
            PPMX          : Integer_32; -- Pixels per meter in x led
            PPMY          : Integer_32; -- Pixels per meter in y led
            Palette_Size  : Integer_32; -- Number of colors
            Important     : Integer_32;
      end case;
   end record with Unchecked_Union, Pack, Size => 40 * 8;

   --------------------
   -- Write_BMP_File --
   --------------------

   procedure Write_BMP_File (File   : in out File_Descriptor;
                             Bitmap : Bitmap_Buffer'Class)
   is
      Hdr         : Header;
      Inf         : Info;
      Row_Size    : constant Integer_32 := Integer_32 (Bitmap.Width * 24);
      Row_Padding : constant Integer_32 := (32 - (Row_Size mod 32)) mod 32 / 8;
      Data_Size   : constant Integer_32 := (Row_Size + Row_Padding) * Integer_32 (Bitmap.Height);

      RGB_Pix : Bitmap_Color;
      Pix_Out : UInt8_Array (1 .. 3);
      Padding : constant UInt8_Array (1 .. Integer (Row_Padding)) := (others => 0);

--        function Write is new Generic_Write (Header);
--        function Write is new Generic_Write (Info);
   begin
      Hdr.Signature := 16#4D42#;
      Hdr.Size      := (Data_Size + 54) / 4;
      Hdr.Offset    := 54;

      --  Set the reserved fields to a known value to avoid "random" data in
      --  the output file. This is necessary because we use this output in the
      --  test-suite where we can't afford to have different outputs between
      --  two execution of the tests.
      Hdr.Reserved1 := 0;
      Hdr.Reserved2 := 0;

      Inf.Struct_Size := 40;
      Inf.Width := Integer_32 (Bitmap.Width);
      Inf.Height := Integer_32 (Bitmap.Height);
      Inf.Planes := 1;
      Inf.Pixel_Size := 24;
      Inf.Compression := 0;
      Inf.Image_Size := Data_Size / 4;
      Inf.PPMX := 2835;
      Inf.PPMY := 2835;
      Inf.Palette_Size := 0;
      Inf.Important := 0;


      if Write (File, Hdr'Address, Hdr'Size / 8) /= (Hdr'Size  / 8) then
         raise Program_Error;
      end if;

      if Write (File, Inf'Address, Inf'Size / 8) /= (Inf'Size  / 8) then
         raise Program_Error;
      end if;

      for Y in reverse 0 .. Bitmap.Height - 1 loop
         for X in 0 .. Bitmap.Width - 1 loop

            RGB_Pix := Bitmap.Pixel ((X, Y));

            Pix_Out (1) := RGB_Pix.Blue;
            Pix_Out (2) := RGB_Pix.Green;
            Pix_Out (3) := RGB_Pix.Red;

            if Write (File, Pix_Out'Address, Pix_Out'Length) /= Pix_Out'Length
            then
               raise Program_Error;
            end if;
         end loop;

         if Write (File, Padding'Address, Padding'Length) /= Padding'Length
         then
            raise Program_Error;
         end if;
      end loop;
   end Write_BMP_File;

end Bitmap_File_Output;
