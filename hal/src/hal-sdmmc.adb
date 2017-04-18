------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2016-2017, AdaCore                        --
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

package body HAL.SDMMC is

   --------------
   -- Send_Cmd --
   --------------

   procedure Send_Cmd
     (This   : in out SDMMC_Driver'Class;
      Cmd    : SD_Command;
      Arg    : UInt32;
      Status : out SD_Error) is
   begin
      Send_Cmd (This, Cmd_Desc (Cmd), Arg, Status);
   end Send_Cmd;

   ---------------
   -- Send_ACmd --
   ---------------

   procedure Send_ACmd
     (This   : in out SDMMC_Driver'Class;
      Cmd    : SD_Specific_Command;
      Rca    : UInt16;
      Arg    : UInt32;
      Status : out SD_Error)
   is
      S_Arg : constant UInt32 :=
                Shift_Left (UInt32 (Rca), 16);
   begin
      Send_Cmd (This, Cmd_Desc (App_Cmd), S_Arg, Status);

      if Status /= OK then
         return;
      end if;

      Send_Cmd (This, Acmd_Desc (Cmd), Arg, Status);
   end Send_ACmd;

end HAL.SDMMC;
