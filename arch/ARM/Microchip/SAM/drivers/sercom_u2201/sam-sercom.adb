------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

package body SAM.SERCOM is

   ----------------
   -- Configured --
   ----------------

   function Configured (This : SERCOM_Device) return Boolean is
   begin
      return This.Config_Done;
   end Configured;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : SERCOM_Device) return Boolean is
   begin
      return This.Periph.SERCOM_SPI.CTRLA.ENABLE;
   end Enabled;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out SERCOM_Device) is
   begin
      This.Periph.SERCOM_SPI.CTRLA.ENABLE := True;

      --  Wait for enable synchronization signal
      while This.Periph.SERCOM_SPI.SYNCBUSY.ENABLE loop
         null;
      end loop;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out SERCOM_Device) is
   begin
      This.Periph.SERCOM_SPI.CTRLA.ENABLE := False;
   end Disable;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SERCOM_Device) is
   begin
      This.Periph.SERCOM_SPI.CTRLA.SWRST := True;

      --  Wait for reset synchronization signal
      while This.Periph.SERCOM_SPI.SYNCBUSY.SWRST loop
         null;
      end loop;

      This.Config_Done := False;
   end Reset;

   ---------------------
   -- Debug_Stop_Mode --
   ---------------------

   procedure Debug_Stop_Mode
     (This : in out SERCOM_Device; Enabled : Boolean := True)
   is
   begin
      This.Periph.SERCOM_SPI.DBGCTRL.DBGSTOP := Enabled;
   end Debug_Stop_Mode;

end SAM.SERCOM;
