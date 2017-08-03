------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body File_Block_Drivers is

   ----------
   -- Open --
   ----------

   function Open (This : in out File_Block_Driver;
                  Path : String;
                  Mode : File_Mode)
                  return Status_Code
   is
   begin
      This.Mode := Mode;
      return Open (This.FD, Path, Mode);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out File_Block_Driver) is
   begin
      Close (This.FD);
   end Close;


   ----------
   -- Read --
   ----------

   overriding
   function Read
     (This         : in out File_Block_Driver;
      Block_Number : UInt64;
      Data         : out Block)
      return Boolean
   is
      Amount : File_Size := File_Size (Block_Number * 512);
   begin

      if This.Mode = Write_Only then
         return False;
      end if;

      if Seek (This.FD, From_Start, Amount) /= OK then
         return False;
      end if;

      Amount := Data'Length;
      return Read (This.FD, Data'Address, Amount) = Amount;
   end Read;

   ----------
   -- Read --
   ----------

   overriding
   function Write
     (This         : in out File_Block_Driver;
      Block_Number : UInt64;
      Data         : Block)
      return Boolean
   is
      Amount : File_Size := File_Size (Block_Number * 512);
   begin

      if This.Mode = Read_Only then
         return False;
      end if;

      if Seek (This.FD, From_Start, Amount) /= OK then
         return False;
      end if;

      Amount := Data'Length;
      return Write (This.FD, Data'Address, Amount) = Amount;
   end Write;

end File_Block_Drivers;
