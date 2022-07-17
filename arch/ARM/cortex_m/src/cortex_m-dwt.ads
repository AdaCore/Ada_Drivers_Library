------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

--  This package provides utility routines for use with the Data Watchpoint
--  Trace (DWT) facility defined by ARM for Cortex M processors. As such it
--  should be vendor-independent.

with HAL;  use HAL;

package Cortex_M.DWT is  --  Data Watchpoint Trace
   pragma Elaborate_Body;

   --  The assumption is that application code will access the registers of
   --  the DWT directly, via the SVD-generated package Cortex_M_SVD.DWT,
   --  except when the convenience routines below are utilized.

   ----------------------------
   --  Convenience functions --
   ----------------------------

   --  DWT reset values. These constant are the control register considered
   --  as unsigned 32-bit values for convenient comparison using the function
   --  below. The values are just the NUMCOMP nibble and the boolean flags in
   --  the next nibble.
   No_DWT_Present                            : constant UInt32 := 0;
   Only_One_Comparator                       : constant UInt32 :=
      16#1000_0000#; --  268435456 dec
   One_Comparator_Watchpoints                : constant UInt32 :=
      16#1F00_0000#; --  520093696 dec
   Four_Comparators_Watchpoints_And_Triggers : constant UInt32 :=
      16#4000_0000#; -- 1073741824 dec
   Four_Comparators_Watchpoints_Only         : constant UInt32 :=
      16#4F00_0000#; -- 1325400064 dec

   function DWT_Reset_Value return UInt32 with Inline;
   --  Returns the value of the DWT.CTRL register as a word, for convenient
   --  comparison to the constants above.

   procedure Enable_DWT_Unit with
     Post => DWT_Unit_Enabled,
     Inline;
   --  Sets the trace enable bit (TRCENA) in the Debug Exception & Monitor Ctrl
   --  (DEMCR) register within the Cortex M Debug peripheral.

   procedure Disable_DWT_Unit with
     Post => not DWT_Unit_Enabled,
     Inline;
   --  Clears the trace enable bit (TRCENA) in the Debug Exception & Monitor
   --  Ctrl (DEMCR) register within the Cortex M Debug peripheral.

   function DWT_Unit_Enabled return Boolean with Inline;

end Cortex_M.DWT;
