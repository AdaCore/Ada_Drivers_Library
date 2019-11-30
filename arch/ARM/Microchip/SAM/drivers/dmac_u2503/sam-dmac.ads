------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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
with System;

package SAM.DMAC is

   type Descriptor_Section;
   type Descriptor_Section_Access is access all Descriptor_Section;
   --  See definition below

   procedure Enable (Descriptors : not null Descriptor_Section_Access;
                     Write_Back  : not null Descriptor_Section_Access);

   procedure Disable;


   type Channel_Id is range 0 .. 31;

   type Priority_Level is range 0 .. 3;
   --  Priority level used for the DMA channel, where a high level has priority
   --  over a low level.

   type Trigger_Source is new HAL.UInt7;
   --  See child package SAM.DMAC.Sources

   type Trigger_Action is (Block, Burst, Transaction);
   --  Trigger action used for a transfer

   type FIFO_Threshold_Kind is (BEAT_1, BEAT_2, BEAT_4, BEAT_8);
   --  Threshold from which the DMA starts to write to the destination. These
   --  bits have no effect in the case of single beat transfers.

   type Burst_Length is range 1 .. 16;

   -- Channel actions --
   procedure Configure (Id          : Channel_Id;
                        Trig_Src    : Trigger_Source;
                        Trig_Action : Trigger_Action;
                        Priority    : Priority_Level;
                        Burst_Len   : Burst_Length;
                        Threshold   : FIFO_Threshold_Kind);

   procedure Enable (Id : Channel_Id);
   procedure Suspend (Id : Channel_Id);
   procedure Resume (Id : Channel_Id);
   procedure Software_Trigger (Id : Channel_Id);

   -- Channel Status --
   function Pending     (Id : Channel_Id) return Boolean;
   function Busy        (Id : Channel_Id) return Boolean;
   function Fetch_Error (Id : Channel_Id) return Boolean;
   function CRC_Error   (Id : Channel_Id) return Boolean;

   -- Channel Interrupts --
   type Interrupt_Kind is (Suspend, Transfer_Complete, Transfer_Error);
   procedure Enable (Id : Channel_Id; Int : Interrupt_Kind);
   procedure Disable (Id : Channel_Id; Int : Interrupt_Kind);
   procedure Clear (Id : Channel_Id; Int : Interrupt_Kind);
   function Set (Id : Channel_Id; Int : Interrupt_Kind) return Boolean;

   -- Transfer Descriptor --

   type Transfer_Descriptor;
   type Transfer_Descriptor_Access is access all Transfer_Descriptor;

   type Event_Output_Kind is
     (Disable, -- Event generation disabled
      Block,   -- Event strobe when block transfer complete
      Beat)    -- Event strobe when beat transfer complete
     with Size => 2;

   type Block_Action_Kind is
     (No_Action,
      --  Channel will be disabled if it is the last block transfer in the
      --  transaction.

      Interrupt,
      --  Channel will be disabled if it is the last block transfer in the
      --  transaction and block interrupt.

      Suspend,
      --  Channel suspend operation is completed

      Both
      --  Both channel suspend operation and block interrupt
     )
       with Size => 2;

   type Beat_Size_Kind is (B_8bit, B_16bit, B_32bit)
     with Size => 2;

   type Step_Selection_Kind is (Destination, Source)
     with Size => 1;

   type Step_Size_Kind is (X1, X2, X4, X8, X16, X32, X64, X128)
     with Size => 3;

   type Transfer_Descriptor is record
      Valid                : Boolean := False;
      Event_Output         : Event_Output_Kind;
      Block_Action         : Block_Action_Kind;
      Beat_Size            : Beat_Size_Kind;
      Src_Addr_Inc         : Boolean;
      Dst_Addr_Inc         : Boolean;
      Step_Selection       : Step_Selection_Kind;
      Step_Size            : Step_Size_Kind;
      Block_Transfer_Count : HAL.UInt16;
      Src_Addr             : System.Address;
      Dst_Addr             : System.Address;
      Next_Descriptor      : Transfer_Descriptor_Access := null;
   end record
     with Volatile, Size => 128;

   type Descriptor_Section is array (Channel_Id) of Transfer_Descriptor
     with Alignment => 128, Size => 32 * 128;

private

   for Trigger_Action use (Block      => 0,
                           Burst      => 2,
                           Transaction => 3);

   for FIFO_Threshold_Kind use (BEAT_1 => 0,
                                BEAT_2 => 2,
                                BEAT_4 => 3,
                                BEAT_8 => 4);

   for Event_Output_Kind use
     (Disable => 0,
      Block => 1,
      Beat => 3);

   for Block_Action_Kind use
     (No_Action => 0,
      Interrupt => 1,
      Suspend   => 2,
      Both      => 3);

   for Beat_Size_Kind use
     (B_8bit  => 0,
      B_16bit => 1,
      B_32bit => 2);

   for Step_Selection_Kind use
     (Destination => 0,
      Source      => 1);

   for Step_Size_Kind use
     (X1   => 0,
      X2   => 1,
      X4   => 2,
      X8   => 3,
      X16  => 4,
      X32  => 5,
      X64  => 6,
      X128 => 7);

   for Transfer_Descriptor use record
      Valid                at 16#0# range  0 ..  0;
      Event_Output         at 16#0# range  1 ..  2;
      Block_Action         at 16#0# range  3 ..  4;
      Beat_Size            at 16#0# range  8 ..  9;
      Src_Addr_Inc         at 16#0# range 10 .. 10;
      Dst_Addr_Inc         at 16#0# range 11 .. 11;
      Step_Selection       at 16#0# range 12 .. 12;
      Step_Size            at 16#0# range 13 .. 15;
      Block_Transfer_Count at 16#2# range  0 .. 15;
      Src_Addr             at 16#4# range  0 .. 31;
      Dst_Addr             at 16#8# range  0 .. 31;
      Next_Descriptor      at 16#C# range  0 .. 31;
   end record;

end SAM.DMAC;
