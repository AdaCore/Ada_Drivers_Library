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

with Hex_Dump;

package body Monitor.Block_Drivers is

   ----------
   -- Read --
   ----------

   overriding function Read
     (This         : in out Block_Driver_Monitor;
      Block_Number : UInt64;
      Data         : out Block)
      return Boolean
   is
      Ret : Boolean;
   begin
      if This.Enabled then
         This.Put_Line ("[START] read from block device at block #" &
                          Block_Number'Img & " Size:" & Data'Length'Img);
      end if;

      Ret := This.Driver_Under_Monitoring.Read (Block_Number, Data);

      if This.Enabled then
         if Ret then
            This.Put_Line ("[OK] read from block device at block #" &
                             Block_Number'Img & " - Data:");
            Hex_Dump.Hex_Dump (Data,
                               This.Put_Line,
                               Base_Addr => Block_Number * 512);
         else
            This.Put_Line ("[FAIL] read from block device at block #" &
                             Block_Number'Img);
         end if;
      end if;

      return Ret;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This         : in out Block_Driver_Monitor;
      Block_Number : UInt64;
      Data         : Block)
      return Boolean
   is
      Ret : Boolean;
   begin
      if This.Enabled then
         This.Put_Line ("[START] write to block device at block #" &
                          Block_Number'Img & " Size:" & Data'Length'Img);
      end if;

      Ret := This.Driver_Under_Monitoring.Write (Block_Number, Data);

      if This.Enabled then
         if Ret then
            This.Put_Line ("[OK] write to block device at block #" &
                             Block_Number'Img & " - Data:");
         else
            This.Put_Line ("[FAIL] write to block device at block #" &
                             Block_Number'Img & " - Data:");
         end if;
         Hex_Dump.Hex_Dump (Data,
                            This.Put_Line,
                            Base_Addr => Block_Number * 512);
      end if;
      return Ret;
   end Write;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Block_Driver_Monitor) is
   begin
      This.Enabled := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Block_Driver_Monitor) is
   begin
      This.Enabled := False;
   end Disable;

end Monitor.Block_Drivers;
