------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with NRF51_SVD.PPI; use NRF51_SVD.PPI;
with nRF51.Events;  use nRF51.Events;
with nRF51.Tasks;   use nRF51.Tasks;

package body nRF51.PPI is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Chan    : Channel_ID;
      Evt_EP  : Event_Type;
      Task_EP : Task_Type)
   is
   begin
      PPI_Periph.CH (Chan).EEP := Get_Address (Evt_EP);
      PPI_Periph.CH (Chan).TEP := Get_Address (Task_EP);
   end Configure;

   --------------------
   -- Enable_Channel --
   --------------------

   procedure Enable_Channel (Chan : Channel_ID) is
      Arr : CHENSET_CH_Field_Array := (others => Chenset_Ch0_Field_Reset);
   begin
      Arr (Chan) := Set;
      PPI_Periph.CHENSET.CH.Arr := Arr;
   end Enable_Channel;

   ---------------------
   -- Disable_Channel --
   ---------------------

   procedure Disable_Channel (Chan : Channel_ID) is
      Arr : CHENCLR_CH_Field_Array := (others => Chenclr_Ch0_Field_Reset);
   begin
      Arr (Chan) := Clear;
      PPI_Periph.CHENCLR.CH.Arr := Arr;
   end Disable_Channel;

   ------------------
   -- Add_To_Group --
   ------------------

   procedure Add_To_Group
     (Chan  : Channel_ID;
      Group : Group_ID)
   is
   begin
      PPI_Periph.CHG (Group).CH.Arr (Chan) := Included;
   end Add_To_Group;

   -----------------------
   -- Remove_From_Group --
   -----------------------

   procedure Remove_From_Group
     (Chan  : Channel_ID;
      Group : Group_ID)
   is
   begin
      PPI_Periph.CHG (Group).CH.Arr (Chan) := Excluded;
   end Remove_From_Group;

   ------------------
   -- Enable_Group --
   ------------------

   procedure Enable_Group (Group : Group_ID) is
   begin
      PPI_Periph.TASKS_CHG (Group).EN := 1;
   end Enable_Group;

   -------------------
   -- Disable_Group --
   -------------------

   procedure Disable_Group (Group : Group_ID) is
   begin
      PPI_Periph.TASKS_CHG (Group).DIS := 1;
   end Disable_Group;

end nRF51.PPI;
