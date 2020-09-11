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

with HAL;
with RISCV.Types;       use RISCV.Types;
with RISCV.CSR_Generic; use RISCV.CSR_Generic;

package RISCV.CSR is

   -- Machine Information --
   package Mvendorid is new CSR_RO_Pck ("mvendorid", Unsigned_XLEN);
   package Marchid   is new CSR_RO_Pck ("marchid", Unsigned_XLEN);
   package Mimpid    is new CSR_RO_Pck ("mimpid", Unsigned_XLEN);
   package Mhardid   is new CSR_RO_Pck ("mhardid", Unsigned_XLEN);

   -- Machine Trap Setup --
   package Mstatus    is new CSR_RW_Pck ("mstatus", Unsigned_XLEN);
   package Misa       is new CSR_RW_Pck ("misa", Unsigned_XLEN);
   package Medeleg    is new CSR_RW_Pck ("medeleg", Unsigned_XLEN);
   package Mideleg    is new CSR_RW_Pck ("mideleg", Unsigned_XLEN);
   package Mie        is new CSR_RW_Pck ("mie", Unsigned_XLEN);
   package Mtvec      is new CSR_RW_Pck ("mtvec", Unsigned_XLEN);
   package Mcounteren is new CSR_RW_Pck ("mcounteren", Unsigned_XLEN);

   -- Machine Trap Handling --
   package Mscratch is new CSR_RW_Pck ("mscratch", Unsigned_XLEN);
   package Mepc     is new CSR_RW_Pck ("mepc", Unsigned_XLEN);
   package Mcause   is new CSR_RW_Pck ("mcause", Unsigned_XLEN);
   package Mtval    is new CSR_RW_Pck ("mtval", Unsigned_XLEN);
   package Mip      is new CSR_RW_Pck ("mip", Unsigned_XLEN);

   -- Machine Counter / Timers --
   package Mcycle        is new CSR_RW_64_Pck ("mcycle");
   package Minstret      is new CSR_RW_64_Pck ("minstret");
   package Mhpmcounter3  is new CSR_RW_64_Pck ("mhpmcounter3");
   package Mhpmcounter4  is new CSR_RW_64_Pck ("mhpmcounter4");
   package Mhpmcounter5  is new CSR_RW_64_Pck ("mhpmcounter5");
   package Mhpmcounter6  is new CSR_RW_64_Pck ("mhpmcounter6");
   package Mhpmcounter7  is new CSR_RW_64_Pck ("mhpmcounter7");
   package Mhpmcounter8  is new CSR_RW_64_Pck ("mhpmcounter8");
   package Mhpmcounter9  is new CSR_RW_64_Pck ("mhpmcounter9");
   package Mhpmcounter10 is new CSR_RW_64_Pck ("mhpmcounter10");
   package Mhpmcounter11 is new CSR_RW_64_Pck ("mhpmcounter11");
   package Mhpmcounter12 is new CSR_RW_64_Pck ("mhpmcounter12");
   package Mhpmcounter13 is new CSR_RW_64_Pck ("mhpmcounter13");
   package Mhpmcounter14 is new CSR_RW_64_Pck ("mhpmcounter14");
   package Mhpmcounter15 is new CSR_RW_64_Pck ("mhpmcounter15");
   package Mhpmcounter16 is new CSR_RW_64_Pck ("mhpmcounter16");
   package Mhpmcounter17 is new CSR_RW_64_Pck ("mhpmcounter17");
   package Mhpmcounter18 is new CSR_RW_64_Pck ("mhpmcounter18");
   package Mhpmcounter19 is new CSR_RW_64_Pck ("mhpmcounter19");
   package Mhpmcounter20 is new CSR_RW_64_Pck ("mhpmcounter20");
   package Mhpmcounter21 is new CSR_RW_64_Pck ("mhpmcounter21");
   package Mhpmcounter22 is new CSR_RW_64_Pck ("mhpmcounter22");
   package Mhpmcounter23 is new CSR_RW_64_Pck ("mhpmcounter23");
   package Mhpmcounter24 is new CSR_RW_64_Pck ("mhpmcounter24");
   package Mhpmcounter25 is new CSR_RW_64_Pck ("mhpmcounter25");
   package Mhpmcounter26 is new CSR_RW_64_Pck ("mhpmcounter26");
   package Mhpmcounter27 is new CSR_RW_64_Pck ("mhpmcounter27");
   package Mhpmcounter28 is new CSR_RW_64_Pck ("mhpmcounter28");
   package Mhpmcounter29 is new CSR_RW_64_Pck ("mhpmcounter29");
   package Mhpmcounter30 is new CSR_RW_64_Pck ("mhpmcounter30");
   package Mhpmcounter31 is new CSR_RW_64_Pck ("mhpmcounter31");

   -- Machine Counter Setup --
   package Mcountinhibit is new CSR_RW_Pck ("mcountinhibit", HAL.UInt32);
   package Mhpmevent3    is new CSR_RW_Pck ("mhpmevent3", Unsigned_XLEN);
   package Mhpmevent4    is new CSR_RW_Pck ("mhpmevent4", Unsigned_XLEN);
   package Mhpmevent5    is new CSR_RW_Pck ("mhpmevent5", Unsigned_XLEN);
   package Mhpmevent6    is new CSR_RW_Pck ("mhpmevent6", Unsigned_XLEN);
   package Mhpmevent7    is new CSR_RW_Pck ("mhpmevent7", Unsigned_XLEN);
   package Mhpmevent8    is new CSR_RW_Pck ("mhpmevent8", Unsigned_XLEN);
   package Mhpmevent9    is new CSR_RW_Pck ("mhpmevent9", Unsigned_XLEN);
   package Mhpmevent10   is new CSR_RW_Pck ("mhpmevent10", Unsigned_XLEN);
   package Mhpmevent11   is new CSR_RW_Pck ("mhpmevent11", Unsigned_XLEN);
   package Mhpmevent12   is new CSR_RW_Pck ("mhpmevent12", Unsigned_XLEN);
   package Mhpmevent13   is new CSR_RW_Pck ("mhpmevent13", Unsigned_XLEN);
   package Mhpmevent14   is new CSR_RW_Pck ("mhpmevent14", Unsigned_XLEN);
   package Mhpmevent15   is new CSR_RW_Pck ("mhpmevent15", Unsigned_XLEN);
   package Mhpmevent16   is new CSR_RW_Pck ("mhpmevent16", Unsigned_XLEN);
   package Mhpmevent17   is new CSR_RW_Pck ("mhpmevent17", Unsigned_XLEN);
   package Mhpmevent18   is new CSR_RW_Pck ("mhpmevent18", Unsigned_XLEN);
   package Mhpmevent19   is new CSR_RW_Pck ("mhpmevent19", Unsigned_XLEN);
   package Mhpmevent20   is new CSR_RW_Pck ("mhpmevent20", Unsigned_XLEN);
   package Mhpmevent21   is new CSR_RW_Pck ("mhpmevent21", Unsigned_XLEN);
   package Mhpmevent22   is new CSR_RW_Pck ("mhpmevent22", Unsigned_XLEN);
   package Mhpmevent23   is new CSR_RW_Pck ("mhpmevent23", Unsigned_XLEN);
   package Mhpmevent24   is new CSR_RW_Pck ("mhpmevent24", Unsigned_XLEN);
   package Mhpmevent25   is new CSR_RW_Pck ("mhpmevent25", Unsigned_XLEN);
   package Mhpmevent26   is new CSR_RW_Pck ("mhpmevent26", Unsigned_XLEN);
   package Mhpmevent27   is new CSR_RW_Pck ("mhpmevent27", Unsigned_XLEN);
   package Mhpmevent28   is new CSR_RW_Pck ("mhpmevent28", Unsigned_XLEN);
   package Mhpmevent29   is new CSR_RW_Pck ("mhpmevent29", Unsigned_XLEN);
   package Mhpmevent30   is new CSR_RW_Pck ("mhpmevent30", Unsigned_XLEN);
   package Mhpmevent31   is new CSR_RW_Pck ("mhpmevent31", Unsigned_XLEN);

end RISCV.CSR;
