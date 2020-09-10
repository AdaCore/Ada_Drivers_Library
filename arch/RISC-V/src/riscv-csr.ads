------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with RISCV.CSR_Generic; use RISCV.CSR_Generic;

package RISCV.CSR is

   function Mcycle is new Read_CSR_64 ("mcycle");
   function Minstret is new Read_CSR_64 ("minstret");

   --  Hardware performance event counters --

   function Mhpmcounter3 is new Read_CSR_64 ("mhpmcounter3");
   function Mhpmcounter4 is new Read_CSR_64 ("mhpmcounter4");
   function Mhpmcounter5 is new Read_CSR_64 ("mhpmcounter5");
   function Mhpmcounter6 is new Read_CSR_64 ("mhpmcounter6");
   function Mhpmcounter7 is new Read_CSR_64 ("mhpmcounter7");
   function Mhpmcounter8 is new Read_CSR_64 ("mhpmcounter8");
   function Mhpmcounter9 is new Read_CSR_64 ("mhpmcounter9");
   function Mhpmcounter10 is new Read_CSR_64 ("mhpmcounter10");
   function Mhpmcounter11 is new Read_CSR_64 ("mhpmcounter11");
   function Mhpmcounter12 is new Read_CSR_64 ("mhpmcounter12");
   function Mhpmcounter13 is new Read_CSR_64 ("mhpmcounter13");
   function Mhpmcounter14 is new Read_CSR_64 ("mhpmcounter14");
   function Mhpmcounter15 is new Read_CSR_64 ("mhpmcounter15");
   function Mhpmcounter16 is new Read_CSR_64 ("mhpmcounter16");
   function Mhpmcounter17 is new Read_CSR_64 ("mhpmcounter17");
   function Mhpmcounter18 is new Read_CSR_64 ("mhpmcounter18");
   function Mhpmcounter19 is new Read_CSR_64 ("mhpmcounter19");
   function Mhpmcounter20 is new Read_CSR_64 ("mhpmcounter20");
   function Mhpmcounter21 is new Read_CSR_64 ("mhpmcounter21");
   function Mhpmcounter22 is new Read_CSR_64 ("mhpmcounter22");
   function Mhpmcounter23 is new Read_CSR_64 ("mhpmcounter23");
   function Mhpmcounter24 is new Read_CSR_64 ("mhpmcounter24");
   function Mhpmcounter25 is new Read_CSR_64 ("mhpmcounter25");
   function Mhpmcounter26 is new Read_CSR_64 ("mhpmcounter26");
   function Mhpmcounter27 is new Read_CSR_64 ("mhpmcounter27");
   function Mhpmcounter28 is new Read_CSR_64 ("mhpmcounter28");
   function Mhpmcounter29 is new Read_CSR_64 ("mhpmcounter29");
   function Mhpmcounter30 is new Read_CSR_64 ("mhpmcounter30");
   function Mhpmcounter31 is new Read_CSR_64 ("mhpmcounter31");

end RISCV.CSR;
