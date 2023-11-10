------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2023, AdaCore                     --
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

package body ILI9341.SPI_Connector is

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

      This.Chip_Select.Set;
   end Send_Command;

end ILI9341.SPI_Connector;
