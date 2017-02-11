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

package body STM32.CRC is

   ----------------------
   -- Reset_Calculator --
   ----------------------

   procedure Reset_Calculator (This : in out CRC_32) is
   begin
      This.CR.CR := True;
   end Reset_Calculator;

   -----------
   -- Value --
   -----------

   function Value (This : CRC_32) return UInt32 is
      (This.DR);

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : UInt32;
      Output : out UInt32)
   is
   begin
      This.DR := Input;
      --  Each write operation into the data register causes the device to
      --  calculate a new CRC value based on the previous CRC value and Input,
      --  so although this looks like a useless sequence that would just return
      --  the Input value, it is necessary.
      Output := This.DR;
   end Update_CRC;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : Block_32;
      Output : out UInt32)
   is
   begin
      for K in Input'Range loop
         This.DR := Input (K);
      end loop;
      Output := This.DR;
   end Update_CRC;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : Block_16;
      Output : out UInt32)
   is
   begin
      for K in Input'Range loop
         This.DR := UInt32 (Input (K));
      end loop;
      Output := This.DR;
   end Update_CRC;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This   : in out CRC_32;
      Input  : Block_8;
      Output : out UInt32)
   is
   begin
      for K in Input'Range loop
         This.DR := UInt32 (Input (K));
      end loop;
      Output := This.DR;
   end Update_CRC;

   --------------------------
   -- Set_Independent_Data --
   --------------------------

   procedure Set_Independent_Data
     (This  : in out CRC_32;
      Value : UInt8)
   is
   begin
      This.IDR.IDR := Value;
   end Set_Independent_Data;

   ----------------------
   -- Independent_Data --
   ----------------------

   function Independent_Data (This : CRC_32) return UInt8 is
     (This.IDR.IDR);

end STM32.CRC;
