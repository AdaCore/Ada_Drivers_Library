------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with System;

package body W25Q16 is
   use type HAL.SPI.SPI_Status;
   use type HAL.UInt32;

   type Status_Register_1 is record
      Busy : Boolean;  --  Erase, Program or Write cycle in progress
      WEL  : Boolean;  --  Write Enable Latch
      BP   : Natural range 0 .. 7;  --  Block protect
      TB   : Boolean;  --  Top/Bottom protect
      SEC  : Boolean;  --  Sector protect
      SPR0 : Boolean;  --  Status regirter protect 0
   end record
     with Size => 8,
          Bit_Order => System.Low_Order_First;

   for Status_Register_1 use record
      Busy at 0 range 0 .. 0;
      WEL  at 0 range 1 .. 1;
      BP   at 0 range 2 .. 4;
      TB   at 0 range 5 .. 5;
      SEC  at 0 range 6 .. 6;
      SPR0 at 0 range 7 .. 7;
   end record;

   function Cast is new Ada.Unchecked_Conversion
     (HAL.UInt8, Status_Register_1);

   type Status_Register_2 is record
      SPR1     : Boolean;  --  Status regirter protect 0
      QE       : Boolean;  --  Quad Enable
      Reserved : Natural range 0 .. 0 := 0;
      SUS      : Boolean;  --  Suspend status
   end record
     with Size => 8,
          Bit_Order => System.Low_Order_First;

   for Status_Register_2 use record
      SPR1     at 0 range 0 .. 0;
      QE       at 0 range 1 .. 1;
      Reserved at 0 range 2 .. 6;
      SUS      at 0 range 7 .. 7;
   end record;

   pragma Unreferenced (Status_Register_2);

   Masks : constant array (1 .. 3) of HAL.UInt32 :=
     (2 ** 12 - 1, 2 ** 15 - 1, 2 ** 16 - 1);
   --  Masks for 4K, 32K and 64K erasable regions

   Read_JEDEC_ID : constant HAL.SPI.SPI_Data_8b :=
     (1 => 16#9F#);  --  Read JEDEC ID (9Fh)

   Read_Status_Register_1 : constant HAL.SPI.SPI_Data_8b :=
     (1 => 16#05#);  --  Read Status Register-1 (05h)

   Write_Enable : constant HAL.SPI.SPI_Data_8b :=
     (1 => 16#06#);  --  Write Enable (06h)

   Write_Disable : constant HAL.SPI.SPI_Data_8b :=
     (1 => 16#04#);  --  Write Disable (04h)

   Chip_Erase : constant HAL.SPI.SPI_Data_8b :=
     (1 => 16#60#);  --  Chip Erase (C7h / 60h)

   --------------------
   -- Check_JEDEC_ID --
   --------------------

   procedure Check_JEDEC_ID
     (This : in out Flash_Memory;
      Ok   : out Boolean)
   is
      use type HAL.SPI.SPI_Data_8b;

      Bytes  : HAL.SPI.SPI_Data_8b (1 .. 3) := (others => 0);
      Status : HAL.SPI.SPI_Status;
   begin
      This.CS.Clear;

      This.SPI.Transmit (Read_JEDEC_ID, Status);

      if Status = HAL.SPI.Ok then
         This.SPI.Receive (Bytes, Status);
      end if;

      Ok := Status = HAL.SPI.Ok
        and then Bytes = (16#ef#, 16#40#, 16#15#);

      This.CS.Set;
   end Check_JEDEC_ID;

   ---------------------
   -- Erasable_Region --
   ---------------------

   overriding function Erasable_Region
     (This   : in out Flash_Memory;
      Region : HAL.Flash.Memory_Region)
       return HAL.Flash.Memory_Region
   is
      From : constant HAL.UInt32 := HAL.UInt32 (Region.From);
      To   : constant HAL.UInt32 := HAL.UInt32 (Region.To);
   begin
      for Mask of Masks loop
         if (From and not Mask) = (To and not Mask) then
            return (From => Natural (From and not Mask),
                    To   => Natural (From or Mask));
         end if;
      end loop;

      return (0, Chip_Size - 1);  --  Chip erase
   end Erasable_Region;

   -----------
   -- Erase --
   -----------

   overriding procedure Erase
     (This    : in out Flash_Memory;
      Region  : HAL.Flash.Memory_Region;
      Success : out Boolean)
   is
      Status : HAL.SPI.SPI_Status;

      From : constant HAL.UInt32 := HAL.UInt32 (Region.From);
      To   : constant HAL.UInt32 := HAL.UInt32 (Region.To);

      Commands : constant array (Masks'Range) of HAL.UInt8 :=
        (16#20#,   --  Sector Erase (20h)
         16#52#,   --  32KB Block Erase (52h):
         16#D8#);  --  64KB Block Erase (D8h)

      Erase_Command : HAL.SPI.SPI_Data_8b :=
        (0,  --  To be set
         HAL.UInt8 (From / 2 ** 16 mod 256),  --  Addr 24
         HAL.UInt8 (From / 2 ** 8 mod 256),   --  Addr 16
         HAL.UInt8 (From mod 256));           --  Addr 8

   begin
      Success := False;

      if This.Is_Busy then
         return;
      end if;

      This.CS.Clear;
      This.SPI.Transmit (Write_Enable, Status);
      This.CS.Set;

      if Status /= HAL.SPI.Ok then
         return;
      end if;

      This.CS.Clear;

      for J in reverse Masks'Range loop
         if From = (From and not Masks (J))  --  From is aligned on a sector
           and To = (From or Masks (J))  --  To is at the end of the sector
         then
            Erase_Command (Erase_Command'First) := Commands (J);
            This.SPI.Transmit (Erase_Command, Status);
            Success := True;

            exit;
         end if;
      end loop;

      if Success then
         --  We have found a sector that can be erased, check SPI status
         Success := Status = HAL.SPI.Ok;
      elsif From = 0 and To = Chip_Size then
         --  Erase entire chip
         This.SPI.Transmit (Chip_Erase, Status);
         Success := Status = HAL.SPI.Ok;
      else
         --  Region has not correct value, disable write
         This.SPI.Transmit (Write_Disable, Status);
         Success := False;
      end if;

      --  CS Deselect Time after Erase/Program is >= 40ns

      This.CS.Set;
   end Erase;

   -------------
   -- Is_Busy --
   -------------

   overriding function Is_Busy (This : Flash_Memory) return Boolean is
      Status : HAL.SPI.SPI_Status;
      Byte   : HAL.SPI.SPI_Data_8b (1 .. 1);
   begin
      This.CS.Clear;
      This.SPI.Transmit (Read_Status_Register_1, Status);
      This.SPI.Receive (Byte, Status);
      This.CS.Set;

      return Status = HAL.SPI.Ok and Cast (Byte (1)).Busy;
   end Is_Busy;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : out HAL.UInt8_Array;
      Success : out Boolean)
   is
      Status    : HAL.SPI.SPI_Status;

      Fast_Read : constant HAL.SPI.SPI_Data_8b :=
        (16#0B#,  --  Fast Read (0Bh)
         HAL.UInt8 (Offset / 2 ** 16 mod 256),  --  Addr 24
         HAL.UInt8 (Offset / 2 ** 8 mod 256),   --  Addr 16
         HAL.UInt8 (Offset mod 256),            --  Addr 8
         0);                                    --  Skip ony input byte
   begin
      This.CS.Clear;
      This.SPI.Transmit (Fast_Read, Status);

      Success := Status = HAL.SPI.Ok;

      if Success then
         This.SPI.Receive (HAL.SPI.SPI_Data_8b (Data), Status);
         Success := Status = HAL.SPI.Ok;
      end if;

      This.CS.Set;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : HAL.UInt8_Array;
      Success : out Boolean)
   is
      function End_Of_Page (X : Natural) return Positive is
         (Positive (HAL.UInt32 (X) or 255));

      From   : Natural := Offset;
      Status : HAL.SPI.SPI_Status;

   begin
      Success := False;

      if This.Is_Busy then
         return;
      end if;

      loop
         This.CS.Clear;
         This.SPI.Transmit (Write_Enable, Status);
         This.CS.Set;

         if Status /= HAL.SPI.Ok then
            Success := False;
            return;
         end if;

         declare
            To : constant Natural :=
              Natural'Min (Offset + Data'Length - 1, End_Of_Page (From));

            Page_Program : constant HAL.SPI.SPI_Data_8b :=
              (16#02#,  --  Page Program (02h)
               HAL.UInt8 (From / 2 ** 16 mod 256),  --  Addr 24
               HAL.UInt8 (From / 2 ** 8 mod 256),   --  Addr 16
               HAL.UInt8 (From mod 256));           --  Addr 8
         begin
            This.CS.Clear;

            This.SPI.Transmit (Page_Program, Status);
            Success := Status = HAL.SPI.Ok;

            if Success then
               This.SPI.Transmit
                 (HAL.SPI.SPI_Data_8b
                    (Data (Data'First + From - Offset
                           ..
                           Data'First + To - Offset)),
                  Status);
               Success := Status = HAL.SPI.Ok;
            end if;

            This.CS.Set;

            From := To + 1;
         end;

         exit when From >= Offset + Data'Length or not Success;

         while This.Is_Busy loop
            null;
         end loop;
      end loop;
   end Write;

end W25Q16;
