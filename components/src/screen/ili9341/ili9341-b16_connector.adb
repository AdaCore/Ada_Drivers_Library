------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2023, AdaCore                           --
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

with Bitmap_Color_Conversion;

package body ILI9341.B16_Connector is

   --  Colors order in RGB_888 buffer
   R : constant := 3;
   G : constant := 2;
   B : constant := 1;

   -----------------
   -- Read_Pixels --
   -----------------

   procedure Read_Pixels
     (This    : ILI9341_Connector;
      Cmd     : HAL.UInt8;
      Address : System.Address;
      Count   : Positive)
   is
      type Raw_Buffer is array (1 .. Count, 1 .. 3) of HAL.UInt8
        with Pack;

      Raw_Pixels : Raw_Buffer
        with Import, Address => Address;

      Command : HAL.UInt16
        with Import, Address => This.Command, Volatile;

      RAM : HAL.UInt16
        with Import, Address => This.RAM, Volatile;

      Ignore : HAL.UInt16;
   begin
      Command := HAL.UInt16 (Cmd);
      Ignore := RAM;  --  First read returns invalid value

      for J in 1 .. (Count + 1) / 2 loop
         declare
            use HAL;
            Raw : UInt16 := RAM;
         begin
            Raw_Pixels (2 * J - 1, R) := UInt8 (Shift_Right (Raw, 8));
            Raw_Pixels (2 * J - 1, G) := UInt8 (Raw and 255);
            Raw := RAM;
            Raw_Pixels (2 * J - 1, B) := UInt8 (Shift_Right (Raw, 8));

            if 2 * J <= Count then
               Raw_Pixels (2 * J, R) := UInt8 (Raw and 255);
               Raw := RAM;
               Raw_Pixels (2 * J, G) := UInt8 (Shift_Right (Raw, 8));
               Raw_Pixels (2 * J, B) := UInt8 (Raw and 255);
            end if;
         end;
      end loop;
   end Read_Pixels;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (This : ILI9341_Connector;
      Cmd  : HAL.UInt8;
      Data : HAL.UInt8_Array)
   is
      Command : HAL.UInt16
        with Import, Address => This.Command, Volatile;
      RAM : HAL.UInt16
        with Import, Address => This.RAM, Volatile;
   begin
      Command := HAL.UInt16 (Cmd);

      for X of Data loop
         RAM := HAL.UInt16 (X);
      end loop;
   end Send_Command;

   ------------------
   -- Write_Pixels --
   ------------------

   procedure Write_Pixels
     (This    : ILI9341_Connector;
      Mode    : HAL.Bitmap.Bitmap_Color_Mode;
      Address : System.Address;
      Count   : Positive;
      Repeat  : Positive)
   is
      RAM : HAL.UInt16
        with Import, Address => This.RAM, Volatile;
   begin
      case Mode is
         when HAL.Bitmap.RGB_565 =>
            --  Native format for parallel interface
            declare
               subtype Raw_Buffer is HAL.UInt16_Array (1 .. Count);

               Raw_Pixels : Raw_Buffer
                 with Import, Address => Address;
            begin
               for Step in 1 .. Repeat loop
                  for Raw of Raw_Pixels loop
                     RAM := Raw;
                  end loop;
               end loop;
            end;

         when HAL.Bitmap.RGB_888 =>
            declare
               --  subtype Raw_Buffer is HAL.UInt8_Array (1 .. 3 * Count);
               type Raw_Buffer is array (1 .. Count, 1 .. 3) of HAL.UInt8
                 with Pack;

               Raw_Pixels : Raw_Buffer
                 with Import, Address => Address;
            begin
               if Count = 1 then
                  --  Shortcut for Fill, Fill_Rect, Vertical_Line, etc.
                  declare
                     Color : constant HAL.Bitmap.Bitmap_Color :=
                       (Alpha => 255,
                        Red   => Raw_Pixels (1, R),
                        Green => Raw_Pixels (1, G),
                        Blue  => Raw_Pixels (1, B));

                     Word : HAL.UInt16 := HAL.UInt16
                       (Bitmap_Color_Conversion.Bitmap_Color_To_Word
                         (HAL.Bitmap.RGB_565, Color));
                  begin
                     Write_Pixels
                       (This, HAL.Bitmap.RGB_565, Word'Address, 1, Repeat);

                     return;
                  end;
               end if;

               for Step in 1 .. Repeat loop
                  for Index in 1 .. Count loop
                     declare
                        Color : constant HAL.Bitmap.Bitmap_Color :=
                          (Alpha => 255,
                           Red   => Raw_Pixels (Index, R),
                           Green => Raw_Pixels (Index, G),
                           Blue  => Raw_Pixels (Index, B));
                     begin
                        RAM := HAL.UInt16
                          (Bitmap_Color_Conversion.Bitmap_Color_To_Word
                            (HAL.Bitmap.RGB_565, Color));
                     end;
                  end loop;
               end loop;
            end;

         when HAL.Bitmap.ARGB_8888 =>
            declare
               subtype Raw_Buffer is HAL.UInt32_Array (1 .. Count);

               Raw_Pixels : Raw_Buffer
                 with Import, Address => Address;
            begin
               for Step in 1 .. Repeat loop
                  for Raw of Raw_Pixels loop
                     declare
                        Color : constant HAL.Bitmap.Bitmap_Color :=
                          Bitmap_Color_Conversion.Word_To_Bitmap_Color
                            (Mode, Raw);
                     begin
                        RAM := HAL.UInt16
                          (Bitmap_Color_Conversion.Bitmap_Color_To_Word
                            (HAL.Bitmap.RGB_565, Color));
                     end;
                  end loop;
               end loop;
            end;

         when HAL.Bitmap.ARGB_1555 | HAL.Bitmap.ARGB_4444 | HAL.Bitmap.AL_88 =>
            declare
               subtype Raw_Buffer is HAL.UInt16_Array (1 .. Count);

               Raw_Pixels : Raw_Buffer
                 with Import, Address => Address;
            begin
               for Step in 1 .. Repeat loop
                  for Raw of Raw_Pixels loop
                     declare
                        Color : constant HAL.Bitmap.Bitmap_Color :=
                          Bitmap_Color_Conversion.Word_To_Bitmap_Color
                            (Mode, HAL.UInt32 (Raw));
                     begin
                        RAM := HAL.UInt16
                          (Bitmap_Color_Conversion.Bitmap_Color_To_Word
                            (HAL.Bitmap.RGB_565, Color));
                     end;
                  end loop;
               end loop;
            end;

         when HAL.Bitmap.L_8 | HAL.Bitmap.AL_44 =>
            declare
               subtype Raw_Buffer is HAL.UInt8_Array (1 .. Count);

               Raw_Pixels : Raw_Buffer
                 with Import, Address => Address;
            begin
               for Step in 1 .. Repeat loop
                  for Raw of Raw_Pixels loop
                     declare
                        Color : constant HAL.Bitmap.Bitmap_Color :=
                          Bitmap_Color_Conversion.Word_To_Bitmap_Color
                            (Mode, HAL.UInt32 (Raw));
                     begin
                        RAM := HAL.UInt16
                          (Bitmap_Color_Conversion.Bitmap_Color_To_Word
                            (HAL.Bitmap.RGB_565, Color));
                     end;
                  end loop;
               end loop;
            end;

         when others =>
            raise Program_Error;
      end case;
   end Write_Pixels;

end ILI9341.B16_Connector;
