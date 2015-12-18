------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

with STM32_SVD; use STM32_SVD;

package SDRAM_Reg is

   type SDRAM_Burst_Length is
     (Burst_Length_1,
      Burst_Length_2,
      Burst_Length_4,
      Burst_Length_8,
      Burst_Length_Page)
     with Size => 3;
   for SDRAM_Burst_Length use
     (Burst_Length_1    => 2#000#,
      Burst_Length_2    => 2#001#,
      Burst_Length_4    => 2#010#,
      Burst_Length_8    => 2#011#,
      Burst_Length_Page => 2#111#);

   type SDRAM_Burst_Type is
     (Burst_Sequential,
      Burst_Interleaved)
     with Size => 1;

   type SDRAM_CAS_Latency is
     (CAS_Latency_2,
      CAS_Latency_3)
     with Size => 3;
   for SDRAM_CAS_Latency use
     (CAS_Latency_2 => 2#010#,
      CAS_Latency_3 => 2#011#);

   type SDRAM_Write_Burst_Mode is
     (Burst_Mode_Programmed_Length,
      Burst_Mode_Single_Location_Access)
     with Size => 2;

   type SDRAM_Mode_Register is record
      Burst_Length     : SDRAM_Burst_Length;
      Burst_Type       : SDRAM_Burst_Type;
      Latency_Mode     : SDRAM_CAS_Latency;
      Operating_Mode   : UInt2 := 0;
      Write_Burst_Mode : SDRAM_Write_Burst_Mode;
   end record with Size => 10;
   for SDRAM_Mode_Register use record
      Burst_Length     at 0 range 0 .. 2;
      Burst_Type       at 0 range 3 .. 3;
      Latency_Mode     at 0 range 4 .. 6;
      Operating_Mode   at 0 range 7 .. 8;
      Write_Burst_Mode at 0 range 9 .. 9;
   end record;

end SDRAM_Reg;
