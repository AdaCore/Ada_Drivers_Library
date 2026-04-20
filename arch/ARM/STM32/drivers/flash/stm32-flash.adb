------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2026, AdaCore                        --
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

with Memory_Barriers;

with STM32_SVD.FLASH;

package body STM32.Flash is

   FLASH_Periph : STM32_SVD.FLASH.FLASH_Peripheral renames
     STM32_SVD.FLASH.FLASH_Periph;

   Whole_Flash : constant HAL.Flash.Memory_Region := (0, 16#F_FFFF#);

   subtype Memory_Region is HAL.UInt8_Array (1 .. 16#F_FFFF#);

   procedure Wait_Flash;
   --  Spin CPU while the flash is busy

   ---------------------
   -- Erasable_Region --
   ---------------------

   overriding function Erasable_Region
     (This   : in out Flash_Memory;
      Region : HAL.Flash.Memory_Region)
      return HAL.Flash.Memory_Region
   is
      Result : HAL.Flash.Memory_Region;
   begin
      case Region.From is
         when 0 .. 16#FFFF# =>
            Result.From := Region.From / (16 * 1024) * 16 * 1024;
            Result.To := Region.From + 16 * 1024 - 1;
         when 16#1_0000# .. 16#1_FFFF# =>
            Result.From := 16#1_0000#;
            Result.To := 16#1_FFFF#;
         when 16#2_0000# .. 16#F_FFFF# =>
            Result.From := Region.From / (128 * 1024) * 128 * 1024;
            Result.To := Region.From + 128 * 1024 - 1;
         when others =>
            Result := Whole_Flash;
      end case;

      return
        (if Region.To in Result.From .. Result.To
         then Result
         else Whole_Flash);
   end Erasable_Region;

   -----------
   -- Erase --
   -----------

   overriding procedure Erase
     (This    : in out Flash_Memory;
      Region  : HAL.Flash.Memory_Region;
      Success : out Boolean)
   is
      Sector : Natural range 0 .. 27;
      Size   : Positive;
   begin
      if Region = Whole_Flash then
         raise Program_Error with "Unimplemented";
      end if;

      case Region.From is
         when 16#0_0000# .. 16#0_FFFF# =>
            Size := 16 * 1024;
            Sector := (Region.From - 16#0_0000#) / Size;

         when 16#1_0000# .. 16#1_FFFF# =>
            Size := 64 * 1024;
            Sector := 4;

         when 16#2_0000# .. 16#F_FFFF# =>
            Size := 128 * 1024;
            Sector := 5 + (Region.From - 16#2_0000#) / Size;

         when others =>
            Success := False;
            return;
      end case;

      Sector := Sector + (This.Bank - 1) * 16;

      Unlock;

      Wait_Flash;

      FLASH_Periph.SR.EOP := True;  --  Clear interrupt flag

      FLASH_Periph.CR :=
        (LOCK   => False,  --  (No-)Lock
         SER    => True,  --  Sector erase
         SNB    => STM32_SVD.FLASH.CR_SNB_Field (Sector),
         PSIZE  => 2,  --  Parallelism size x32
         STRT   => True,  --  Start
         EOPIE  => False,  --  interrupt on EOP bit = 1
         ERRIE  => False,
         others => <>);

      Success := True;
   end Erase;

   overriding function Is_Busy (This : Flash_Memory) return Boolean is
     (FLASH_Periph.SR.BSY);

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : HAL.UInt8_Array;
      Success : out Boolean)
   is
      function Get_Word (J : Natural) return HAL.UInt32 is
        (HAL.UInt32 (Data (Data'First + 4 * J)) +
         HAL.Shift_Left (HAL.UInt32 (Data (Data'First + 4 * J + 1)), 8) +
         HAL.Shift_Left (HAL.UInt32 (Data (Data'First + 4 * J + 2)), 16) +
         HAL.Shift_Left (HAL.UInt32 (Data (Data'First + 4 * J + 3)), 24));

      Memory : Memory_Region
        with
          Import,
          Address => (if This.Bank = 1 then First_Bank else Second_Bank);

      Memory_32 : HAL.UInt32_Array (0 .. 16#F_FFFF# / 4)
        with
          Import,
          Address => (if This.Bank = 1 then First_Bank else Second_Bank);
   begin
      Unlock;

      if Data'Length mod 4 = 0 then
         for J in 0 .. Data'Length / 4 - 1 loop
            Programming (By_Word => True);
            Memory_32 (Offset + J) := Get_Word (J);
         end loop;
      else
         for J in Data'Range loop
            Programming (By_Word => False);
            Memory (Offset + J - Data'First) := Data (J);
         end loop;
      end if;

      Lock;

      Success := True;
   end Write;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      FLASH_Periph.CR :=
        (LOCK   => True,  --  Lock
         others => <>);
   end Lock;

   -----------------
   -- Programming --
   -----------------

   procedure Programming (By_Word : Boolean) is
   begin
      Wait_Flash;

      FLASH_Periph.CR :=
        (LOCK   => False,  --  (No-)Lock
         PG     => True,  --  Programming
         PSIZE  => (if By_Word then 2 else 0),  --  Parallelism size x32/x8
         others => <>);
   end Programming;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : out HAL.UInt8_Array;
      Success : out Boolean)
   is
      Memory : Memory_Region
        with
          Import,
          Address => (if This.Bank = 1 then First_Bank else Second_Bank);
   begin
      Success := Offset in Memory'Range;

      if Success then
         Data := Memory (Offset .. Offset + Data'Length - 1);
      end if;
   end Read;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      FLASH_Periph.KEYR := 16#4567_0123#;
      Memory_Barriers.Data_Synchronization_Barrier;
      FLASH_Periph.KEYR := 16#CDEF_89AB#;
      Memory_Barriers.Data_Synchronization_Barrier;
      Wait_Flash;
   end Unlock;

   ----------------
   -- Wait_Flash --
   ----------------

   procedure Wait_Flash is
   begin
      while FLASH_Periph.SR.BSY loop
         null;
      end loop;
   end Wait_Flash;

end STM32.Flash;
