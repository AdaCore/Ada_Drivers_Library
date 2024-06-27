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

with Bitmap_Color_Conversion;

with ILI9341.Regs;

package body ILI9341.SPI_Connector is

   -----------------
   -- Read_Pixels --
   -----------------

   procedure Read_Pixels
     (This    : ILI9341_Connector;
      Cmd     : HAL.UInt8;
      Address : System.Address;
      Count   : Positive)
   is
      use all type HAL.SPI.SPI_Status;

      subtype Raw_Buffer is HAL.SPI.SPI_Data_8b (1 .. 3 * Count);

      Raw_Pixels : Raw_Buffer
        with Import, Address => Address;

      Status : HAL.SPI.SPI_Status;
   begin
      This.WRX.Clear;
      This.Chip_Select.Clear;
      This.Port.Transmit (HAL.SPI.SPI_Data_8b'(1 => Cmd), Status);

      if Status /= Ok then
         raise Program_Error;
      end if;

      This.WRX.Set;
      This.Port.Receive (Raw_Pixels, Status);

      This.Chip_Select.Set;
   end Read_Pixels;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (This : ILI9341_Connector;
      Cmd  : HAL.UInt8;
      Data : HAL.UInt8_Array)
   is
      use HAL.SPI;

      Status : SPI_Status;
   begin
      This.WRX.Clear;
      This.Chip_Select.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Cmd), Status);

      if Status /= Ok then
         raise Program_Error;
      end if;

      if Data'Length > 0 then
         This.WRX.Set;
         This.Port.Transmit (SPI_Data_8b (Data), Status);

         if Status /= Ok then
            raise Program_Error;
         end if;
      end if;

      if Cmd not in ILI9341.Regs.ILI9341_GRAM then
         This.Chip_Select.Set;
      end if;
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

      procedure Transmit (Color : HAL.Bitmap.Bitmap_Color);
      --  Transmit RGB on a single pixel

      --------------
      -- Transmit --
      --------------

      procedure Transmit (Color : HAL.Bitmap.Bitmap_Color) is
         Status : HAL.SPI.SPI_Status;

         Pixel : HAL.SPI.SPI_Data_8b (1 .. 4)
           with Import, Address => Color'Address;
      begin
         This.Port.Transmit (Pixel (1 .. 3), Status);
      end Transmit;

   begin
      This.WRX.Set;

      case Mode is
         when HAL.Bitmap.RGB_888 =>
            --  Native format for SPI interface
            declare
               subtype Raw_Buffer is HAL.SPI.SPI_Data_8b (1 .. 3 * Count);

               Raw_Pixels : Raw_Buffer
                 with Import, Address => Address;

               Status : HAL.SPI.SPI_Status;
            begin
               for Step in 1 .. Repeat loop
                  This.Port.Transmit (Raw_Pixels, Status);
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
                        Transmit (Color);
                     end;
                  end loop;
               end loop;
            end;

         when HAL.Bitmap.RGB_565 |
              HAL.Bitmap.ARGB_1555 |
              HAL.Bitmap.ARGB_4444 |
              HAL.Bitmap.AL_88 =>
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
                        Transmit (Color);
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
                        Transmit (Color);
                     end;
                  end loop;
               end loop;
            end;

         when others =>
            raise Program_Error;
      end case;

      This.Chip_Select.Set;
   end Write_Pixels;

end ILI9341.SPI_Connector;
