------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2021, AdaCore                           --
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

--  This file provides NOP-compatible hint functions for devices using the
--  ARMv6-M, ARMv7-M, and ARMv8-M instruction sets.
--
--  Source:
--
--    ARMv6-M Architecture Reference Manual
--    A6.6 Hint Instructions
--    https://developer.arm.com/documentation/ddi0419/e

package Cortex_M.Hints is
   pragma Preelaborate;

   procedure Send_Event with Inline;
   --  A6.7.57 SEV
   --
   --  Causes an event to be signaled to all CPUs within a multiprocessor
   --  system.

   procedure Wait_For_Event with Inline;
   --  A6.7.75 WFE
   --
   --  Permits the processor to enter a low-power state until one of a number
   --  of events occurs, including events signaled by the SEV instruction on
   --  any processor in a multiprocessor system.

   procedure Wait_For_Interrupt with Inline;
   --  A6.7.76 WFI
   --
   --  Suspends execution until one of a number of events occurs.

   procedure Yield with Inline;
   --  A6.7.77 YIELD
   --
   --  Enables software with a multithreading capability to indicate to the
   --  hardware that it is performing a task, for example a spinlock, that
   --  could be swapped out to improve overall system performance.  Hardware
   --  can use this hint to suspend and resume multiple code threads if it
   --  supports the capability.

end Cortex_M.Hints;
