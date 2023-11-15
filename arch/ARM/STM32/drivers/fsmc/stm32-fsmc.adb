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

with STM32_SVD.FSMC;

package body STM32.FSMC is

   type BCR_BTR is record
      BCR  : STM32_SVD.FSMC.BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register
      BTR  : STM32_SVD.FSMC.BTR_Register;
      --  SRAM/NOR-Flash chip-select control register
   end record
     with Volatile;

   for BCR_BTR use record
      BCR at 16#0# range 0 .. 31;
      BTR at 16#4# range 0 .. 31;
   end record;

   type BCR_BTR_Array is array (1 .. 4) of BCR_BTR;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Bank_1 : Bank_1_Configuration := (1 .. 4 => (Is_Set => False));
      Bank_2 : NAND_PC_Card_Configuration := (Is_Set => False);
      Bank_3 : NAND_PC_Card_Configuration := (Is_Set => False);
      Bank_4 : NAND_PC_Card_Configuration := (Is_Set => False))
   is
      pragma Unreferenced (Bank_2, Bank_3, Bank_4);
      BCR_BTR_List : BCR_BTR_Array
        with Import, Address => STM32_SVD.FSMC.FSMC_Periph.BCR1'Address;

      BWTR_List : array (1 .. 4) of STM32_SVD.FSMC.BWTR_Register
        with Import, Address => STM32_SVD.FSMC.FSMC_Periph.BWTR1'Address;
   begin
      for X in Bank_1'Range loop
         declare
            pragma Warnings (Off,  "is not referenced");
            --  GNAT GCC 12.2 gives a wrong warning here
            BCR  : STM32_SVD.FSMC.BCR_Register renames BCR_BTR_List (X).BCR;
            BTR  : STM32_SVD.FSMC.BTR_Register renames BCR_BTR_List (X).BTR;
            pragma Warnings (On,  "is not referenced");
            BWTR : STM32_SVD.FSMC.BWTR_Register renames BWTR_List (X);
         begin
            if Bank_1 (X).Is_Set then
               declare
                  Value : Asynchronous_Configuration renames Bank_1 (X).Value;
               begin
                  BCR :=
                    (MBKEN          => True,
                     MUXEN          => False,
                     MTYP           => Memory_Type'Pos (Value.Memory_Type),
                     MWID           => Memory_Bus_Width'Pos (Value.Bus_Width),
                     FACCEN         => Value.Memory_Type = NOR_Flash,
                     Reserved_7_7   => 1,
                     BURSTEN        => False,
                     WAITPOL        => Value.Wait_Signal = Positive,
                     WRAPMOD        => False,
                     WAITCFG        => False,
                     WREN           => Value.Write_Enable,
                     WAITEN         => False,
                     EXTMOD         => Value.Extended.Mode /= None,
                     ASYNCWAIT      => Value.Wait_Signal /= None,
                     Reserved_16_18 => 0,
                     CBURSTRW       => False,
                     Reserved_20_31 => 0);

                  BTR :=
                    (ADDSET         => HAL.UInt4 (Value.Address_Setup),
                     ADDHLD         =>
                       (if Value.Extended.Mode = Mode_D
                        then HAL.UInt4 (Value.Extended.Read_Address_Hold)
                        else 0),
                     DATAST         => HAL.UInt8 (Value.Data_Setup),
                     BUSTURN        => HAL.UInt4 (Value.Bus_Turn),
                     CLKDIV         => 0,
                     DATLAT         => 0,
                     ACCMOD         =>
                       (if Value.Extended.Mode = None then 0
                        else Asynchronous_Extended_Mode'Pos
                          (Value.Extended.Mode)),
                     Reserved_30_31 => 3);

                  if Value.Extended.Mode /= None then
                     BWTR :=
                       (ADDSET =>
                            HAL.UInt4 (Value.Extended.Write_Address_Setup),
                        ADDHLD =>
                          (if Value.Extended.Mode = Mode_D
                           then HAL.UInt4 (Value.Extended.Write_Address_Hold)
                           else 0),
                        DATAST =>
                          HAL.UInt8 (Value.Extended.Write_Data_Setup),
                        Reserved_16_19 => 16#F#,
                        CLKDIV         => 0,
                        DATLAT         => 0,
                        ACCMOD         => 0,
                        Reserved_30_31 => 0);
                  end if;
               end;
            else
               BCR.MBKEN := False;
            end if;
         end;
      end loop;
   end Configure;

end STM32.FSMC;
