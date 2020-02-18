------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2018-2020, AdaCore                      --
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

with nRF.Device;
with nRF.TWI;

package body MicroBit.I2C is

   Init_Done : Boolean := False;

   Device : nRF.TWI.TWI_Master renames nRF.Device.TWI_0;
   --  This device should not conflict with the device used in MicroBit.SPI.
   --  See nRF Series Reference Manual, chapter Memory.Instantiation.

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean
   is (Init_Done);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : Speed := S400kbps) is
   begin
      Device.Configure
        (SCL   => MB_SCL.Pin,
         SDA   => MB_SDA.Pin,
         Speed => (case S is
                      when S100kbps => nRF.TWI.TWI_100kbps,
                      when S250kbps => nRF.TWI.TWI_250kbps,
                      when S400kbps => nRF.TWI.TWI_400kbps)
        );

      Device.Enable;
      Init_Done := True;
   end Initialize;

   ----------------
   -- Controller --
   ----------------

   function Controller return not null Any_I2C_Port
   is (Device'Access);

end MicroBit.I2C;
