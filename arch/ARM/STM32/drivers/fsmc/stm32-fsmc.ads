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

--  Flexible Static Memory Controller in STM32F40x/41x family

with System;

package STM32.FSMC is

   subtype Clock_Cycle_Count is Natural;
   --  Number of AHB clock cycles (HCLK)

   --  Bank 1 supports NOR Flash, PSRAM and SRAM.
   type Memory_Type is (SRAM, PSRAM, NOR_Flash);
   type Memory_Bus_Width is (Byte, Half_Word);  --  8 or 16 bit
   type Signal_Kind is (None, Positive, Negative);

   type Asynchronous_Extended_Mode is (Mode_A, Mode_B, Mode_C, Mode_D, None);
   --  See extended mode definitions in STM32F40x/41x datasheet.
   --  None corresponds to Mode 1 or Mode 2 depending on the type of memory.
   --  Extended modes have a dedicated settings for write operations, while
   --  Mode 1, 2 have shared settings for read/write.

   type Asynchronous_Extended_Configuration
     (Mode : Asynchronous_Extended_Mode := None) is
   record
      case Mode is
         when None =>
            null;

         when others =>
            Write_Bus_Turn      : Clock_Cycle_Count range 0 .. 15;
            Write_Data_Setup    : Clock_Cycle_Count range 1 .. 256;
            Write_Address_Setup : Clock_Cycle_Count range 0 .. 15;

            case Mode is
               when Mode_D =>
                  Read_Address_Hold   : Clock_Cycle_Count range 0 .. 15;
                  Write_Address_Hold  : Clock_Cycle_Count range 0 .. 15;

               when others =>
                  null;
            end case;
      end case;
   end record;

   type Asynchronous_Configuration is record
      Wait_Signal   : Signal_Kind := None;
      Write_Enable  : Boolean := False;
      Bus_Width     : Memory_Bus_Width := Byte;
      Memory_Type   : FSMC.Memory_Type;
      Bus_Turn      : Clock_Cycle_Count range 0 .. 15 := 15;
      Data_Setup    : Clock_Cycle_Count range 1 .. 255 := 255;
      Address_Setup : Clock_Cycle_Count range 0 .. 15 := 15;

      Extended      : Asynchronous_Extended_Configuration := (Mode => None);
      --  Additional settings
   end record;
   --  Configuration of the subbank of Bank 1. Not every combination of memory
   --  type and extended mode is allowed. Corresponding predicate is:
   --  with Predicate =>
   --  (if Memory_Type = NOR_Flash then Extended.Mode /= Mode_A
   --   else Extended.Mode not in Mode_B | Mode_C);

   type NOR_PSRAM_Configuration (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Asynchronous_Configuration;
         when False =>
            null;
      end case;
   end record;

   subtype Subbank_Index is Integer range 1 .. 4;
   --  Bank 1 has 4 sub-banks

   type Bank_1_Configuration is array (Subbank_Index) of NOR_PSRAM_Configuration;

   type NAND_PC_Card_Configuration (Is_Set : Boolean := False) is record
      null;  --  TBD
   end record;
   --  Bank 2, 3 supports NAND flash, Bank 4 - PC Card
   --  These are not yet implemented

   procedure Configure
     (Bank_1 : Bank_1_Configuration := (1 .. 4 => (Is_Set => False));
      Bank_2 : NAND_PC_Card_Configuration := (Is_Set => False);
      Bank_3 : NAND_PC_Card_Configuration := (Is_Set => False);
      Bank_4 : NAND_PC_Card_Configuration := (Is_Set => False));
   --  Configure memory banks

   function Bank_1_Start
     (Subbank : Subbank_Index := 1) return System.Address is
       (case Subbank is
          when 1 => System'To_Address (16#6000_0000#),
          when 2 => System'To_Address (16#6400_0000#),
          when 3 => System'To_Address (16#6800_0000#),
          when 4 => System'To_Address (16#6C00_0000#));

   Bank_2_Start : constant System.Address :=
     System'To_Address (16#7000_0000#);

   Bank_3_Start : constant System.Address :=
     System'To_Address (16#8000_0000#);

   Bank_4_Start : constant System.Address :=
     System'To_Address (16#9000_0000#);

end STM32.FSMC;
