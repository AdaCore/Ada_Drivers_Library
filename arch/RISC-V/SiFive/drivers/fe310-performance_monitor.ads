------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

package FE310.Performance_Monitor
is
   -----------------------------------
   --  See FE310 Manual section 3.8 --
   -----------------------------------

   --  FE310 supports counters mhpmcounter3 and mhpmcounter4 only
   type Counter_Id is range 3 .. 4;

   --  Instruction Commit Events

   --  Note: position and order matter here.
   type Commit_Events is (Exception_Taken,
                          Integer_Load,
                          Integer_Store,
                          Atomic_Operation,
                          System_Instruction,
                          Integer_Arithmetic,
                          Conditional_Branch,
                          JAL_Instruction,
                          JALR_Instruction,
                          Integer_Multiplication,
                          Integer_Division);


   type Set_Of_Commit_Events is array (Commit_Events) of Boolean;

   No_Commit_Events : constant Set_Of_Commit_Events := (others => False);

   procedure Set_Commit_Events (C : in Counter_Id;
                                S : in Set_Of_Commit_Events);


   --  Micro Architecture Events

   --  Note: position and order matter here.
   type Micro_Arch_Events is (Load_Use_Interlock,
                              Long_Latency_Interlock,
                              CSR_Read_Interlock,
                              Instruction_Cache_Busy,
                              Data_Cache_Busy,
                              Branch_Direction_Misprediction,
                              Branch_Target_Misprediction,
                              Pipeline_Flush_From_CSR_Write,
                              Pipeline_Flish_From_Other,
                              Integer_Multiplication_Interlock);

   type Set_Of_Micro_Arch_Events is
     array (Micro_Arch_Events) of Boolean;

   No_Micro_Arch_Events : constant Set_Of_Micro_Arch_Events :=
     (others => False);

   procedure Set_Micro_Arch_Events (C : in Counter_Id;
                                    S : in Set_Of_Micro_Arch_Events);


   --  Memory System Events

   type Memory_System_Events is (Instruction_Cache_Miss,
                                 Memory_Mapped_IO_Access);
   type Set_Of_Memory_System_Events is
     array (Memory_System_Events) of Boolean;
   No_Memory_System_Events : constant Set_Of_Memory_System_Events :=
     (others => False);

   procedure Set_Memory_System_Events (C : in Counter_Id;
                                       S : in Set_Of_Memory_System_Events);


   --  Counter reading and reset
   procedure Zero_Counter (C : in Counter_Id);
   function Read_Counter (C : in Counter_Id) return UInt64;

end FE310.Performance_Monitor;
