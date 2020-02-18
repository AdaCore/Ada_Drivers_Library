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
with nRF.SPI_Master; use nRF.SPI_Master;

package body MicroBit.SPI is

   Init_Done : Boolean := False;

   Device : SPI_Master renames nRF.Device.SPI_Master_1;
   --  This device should not conflict with the device used in MicroBit.I2C
   --  See nRF Series Reference Manual, chapter Memory.Instantiation.

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean
   is (Init_Done);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (S    : Speed := S1Mbps;
      Mode : SPI_Mode := Mode_0)
   is
   begin
      Device.Configure
        (SCK       => MB_SCK.Pin,
         MOSI      => MB_MOSI.Pin,
         MISO      => MB_MISO.Pin,

         Speed     => (case S is
                          when S125kbps => SPI_125kbps,
                          when S250kbps => SPI_250kbps,
                          when S500kbps => SPI_500kbps,
                          when S1Mbps => SPI_1Mbps,
                          when S2Mbps => SPI_2Mbps,
                          when S4Mbps => SPI_4Mbps,
                          when S8Mbps => SPI_8Mbps),

         Bit_Order => Most_Significant_First,

         Polarity  => (case Mode is
                          when Mode_0 | Mode_1 => Active_High,
                          when Mode_2 | Mode_3 => Active_Low),

         Phase     => (case Mode is
                          when Mode_0 | Mode_2 => Sample_Leading_Edge,
                          when Mode_1 | Mode_3 => Sample_Trailing_Edge));

      Device.Enable;

      Init_Done := True;
   end Initialize;

   ----------------
   -- Controller --
   ----------------

   function Controller return not null Any_SPI_Port
   is (Device'Access);

end MicroBit.SPI;
