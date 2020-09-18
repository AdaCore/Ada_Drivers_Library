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

with RISCV.CSR;   use RISCV.CSR;
with RISCV.Types; use RISCV.Types;
package body FE310.Performance_Monitor is

   subtype Event_Group is Unsigned_XLEN range 0 .. 2;

   generic
      type Events is (<>);
      type Set_Of_Events is array (Events) of Boolean;
      Event_Group_Selector : Event_Group;
   function Compute_Event_Mask (S : in Set_Of_Events) return Unsigned_XLEN;
   --  Computes and returns the counter event selector value given a set of
   --  events and the group selector. See the FE310 Manual Section 3.8,
   --  Table 3.

   function Compute_Event_Mask (S : in Set_Of_Events) return Unsigned_XLEN
   is
      Event_Mask : Unsigned_XLEN := Event_Group_Selector;
   begin
      for I in Events loop
         if S (I) then
            --  Events'First has position number 0, but is
            --  enabled by bit 8 of the mask and so on.
            Event_Mask := Event_Mask or (2 ** (Events'Pos (I) + 8));
         end if;
      end loop;
      return Event_Mask;
   end Compute_Event_Mask;


   -----------------------
   -- Set_Commit_Events --
   -----------------------

   procedure Set_Commit_Events (C : in Counter_Id;
                                S : in Set_Of_Commit_Events)
   is
      function F is new Compute_Event_Mask (Commit_Events, Set_Of_Commit_Events, 0);
      Event_Mask : Unsigned_XLEN;
   begin
      Event_Mask := F (S);
      case C is
         when 3 =>
            Mhpmevent3.Write (Event_Mask);
         when 4 =>
            Mhpmevent4.Write (Event_Mask);
      end case;
   end Set_Commit_Events;

   ---------------------------
   -- Set_Micro_Arch_Events --
   ---------------------------

   procedure Set_Micro_Arch_Events
     (C : in Counter_Id;
      S : in Set_Of_Micro_Arch_Events)
   is
      function F is new Compute_Event_Mask (Micro_Arch_Events, Set_Of_Micro_Arch_Events, 1);
      Event_Mask : Unsigned_XLEN;
   begin
      Event_Mask := F (S);
      case C is
         when 3 =>
            Mhpmevent3.Write (Event_Mask);
         when 4 =>
            Mhpmevent4.Write (Event_Mask);
      end case;
   end Set_Micro_Arch_Events;

   ------------------------------
   -- Set_Memory_System_Events --
   ------------------------------

   procedure Set_Memory_System_Events
     (C : in Counter_Id;
      S : in Set_Of_Memory_System_Events)
   is
      function F is new Compute_Event_Mask (Memory_System_Events, Set_Of_Memory_System_Events, 2);
      Event_Mask : Unsigned_XLEN;
   begin
      Event_Mask := F (S);
      case C is
         when 3 =>
            Mhpmevent3.Write (Event_Mask);
         when 4 =>
            Mhpmevent4.Write (Event_Mask);
      end case;
   end Set_Memory_System_Events;

   ------------------
   -- Zero_Counter --
   ------------------

   procedure Zero_Counter (C : in Counter_Id)
   is
   begin
      case C is
         when 3 =>
            Mhpmcounter3.Write (0);
         when 4 =>
            Mhpmcounter4.Write (0);
      end case;
   end Zero_Counter;

   ------------------
   -- Read_Counter --
   ------------------

   function Read_Counter (C : in Counter_Id) return UInt64 is
   begin
      case C is
         when 3 =>
            return Mhpmcounter3.Read;
         when 4 =>
            return Mhpmcounter4.Read;
      end case;
   end Read_Counter;

end FE310.Performance_Monitor;
