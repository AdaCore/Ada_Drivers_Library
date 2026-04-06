------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

--  A driver for the Cyclic Redundancy Check CRC-32 calculation processor.
--  The CPU transfers the data to the CRC processor.

--  Note this API is for the STM32 F4x family. Other STM MCUs have additional
--  CRC capabilities.

private with STM32_SVD.CRC;

package STM32.CRC is
   pragma Elaborate_Body;

   type CRC_32 is limited private;

   procedure Reset_Calculator (This : in out CRC_32) with
     Post => Value (This) = 16#FFFF_FFFF#;
   --  Reset the unit's calculator value to 16#FFFF_FFFF#. All previous
   --  calculations due to calls to Update_CRC are lost. Does not affect
   --  the contents of the unit's independent data.

   function Value (This : CRC_32) return UInt32;
   --  Returns the currently calculated CRC value, reflecting any prior calls
   --  to Update_CRC. This is the same value returned via the Update_CRC.Output
   --  parameter.

   --  The Update_CRC routines can be called multiple times, back-to-back,
   --  presumably with different input blocks, in order to update the value
   --  of the calculated CRC checksum within the CRC processor with differing
   --  input values.

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : UInt32;
      Output : out UInt32);
   --  Updates the 32-bit CRC value in This from the 32-bit Input value and any
   --  previously-calculated CRC value. Output is the resulting CRC-32 value.

   type Block_32 is array (Positive range <>) of UInt32
     with Component_Size => 32;

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : Block_32;
      Output : out UInt32);
   --  Updates the 32-bit CRC value in This from the 32-bit Input values and
   --  any previously-calculated CRC value. Output is the resulting CRC-32
   --  value.

   type Block_16 is array (Positive range <>) of UInt16
     with Component_Size => 16;

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : Block_16;
      Output : out UInt32);
   --  Updates the 32-bit CRC value in This from the 16-bit Input values and any
   --  previously-calculated CRC value. Output is the resulting CRC-32 value.

   type Block_8 is array (Positive range <>) of UInt8
     with Component_Size => 8;

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : Block_8;
      Output : out UInt32);
   --  Updates the 32-bit CRC value in This from the 8-bit Input values and any
   --  previously-calculated CRC value. Output is the resulting CRC-32 value.

   procedure Set_Independent_Data
     (This  : in out CRC_32;
      Value : UInt8);
   --  Assign the internal "independent data" value. This value is an 8-bit
   --  quantity that can be used for arbitrary purposes, eg temporary storage.
   --  It is not affected by resetting the unit, nor is is affected by the CRC
   --  calculations invoked by calls to Update_CRC, hence the "independent"
   --  nature.

   function Independent_Data (This : CRC_32) return UInt8;
   --  Returns the current "independent data" value.

private

   type CRC_32 is new STM32_SVD.CRC.CRC_Peripheral;

end STM32.CRC;
