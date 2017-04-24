------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with System.Storage_Elements;  use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Ada.Real_Time;            use Ada.Real_Time;

with GNAT.IO;

with RPi.Regs.DMA;             use RPi.Regs.DMA;
with RPi.Firmware.GPU_Memory;  use RPi.Firmware.GPU_Memory;

package body RPi.DMA is

   Debug : constant Boolean := True;

   type SCB_Access is access all DMA_Control_Block;

   function SCB (Base  : System.Address;
                 Index : SCB_Index) return SCB_Access;

   function Get_Index (Base  : System.Address;
                   Block : System.Address) return SCB_Index;

   ---------
   -- SCB --
   ---------

   function SCB (Base  : System.Address;
                 Index : SCB_Index) return SCB_Access
   is
      Offset : constant Storage_Offset :=
                 (Storage_Offset (Index - 1) * DMA_Control_Block'Size / 8);
      function As_SCB is new Ada.Unchecked_Conversion
        (System.Address, SCB_Access);
   begin
      return As_SCB (Base + Offset);
   end SCB;

   -----------
   -- Index --
   -----------

   function Get_Index (Base  : System.Address;
                       Block : System.Address) return SCB_Index
   is
      Offset : Storage_Offset;
      Idx    : Natural;
   begin
      Offset := (Block - Base) * 8;
      Idx    := Integer (Offset) / DMA_Control_Block'Size;

      return SCB_Index (Idx + 1);
   exception
      when others =>
         GNAT.IO.Put_Line (Image8 (UInt32 (To_BUS (Base))));
         GNAT.IO.Put_Line (Image8 (UInt32 (To_BUS (Block))));
         loop
            null;
         end loop;
   end Get_Index;

   --------------------
   -- DMA_Controller --
   --------------------

   protected body DMA_Controller is

      -----------------
      -- Initialized --
      -----------------

      function Initialized return Boolean
      is
         use type System.Address;
      begin
         return DMA_SCB /= System.Null_Address;
      end Initialized;

      -----------------
      -- Set_DMA_SCB --
      -----------------

      procedure Set_DMA_SCB
        (Arr : System.Address;
         Num : Positive)
      is
      begin
         DMA_SCB := Arr;
         Num_SCB := SCB_Index (Num);
      end Set_DMA_SCB;

      -------------------
      -- Wait_Transfer --
      -------------------

      procedure Wait_Transfer is
      begin
         if not DMA_Started then
            return;
         end if;

         while not Device.CS.Ended loop
            null;
         end loop;

         --  Write 1 to clear
         Device.CS.Ended := True;

         DMA_Started := False;
      end Wait_Transfer;

      -------------------
      -- Take_Transfer --
      -------------------

      procedure Take_Transfer (Num_Control_Blocks : Natural;
                               Status             : out Boolean)
      is
         Tail      : SCB_Index;
         Prev      : SCB_Index;
         Next      : SCB_Index := 0;
      begin
         if New_Block /= 0 then
            --  Some other task is preparing a new DMA transfer. Cannot
            --  do anything until this is finished.
            Status := False;

            return;
         end if;

         DMA_Find_Free_SCB (Tail, New_Block);

         if New_Block /= 0 then
            Prev := New_Block;

            --  Check that enough blocks are available
            for J in 2 .. Num_Control_Blocks loop
               if Prev = Num_SCB then
                  Next := 1;
               else
                  Next := Prev + 1;
               end if;

               if Device.CONBLK_AD =
                 To_BUS (SCB (DMA_SCB, Next).all'Address)
               then
                  --  Not enough room
                  New_Block := 0;
                  exit;
               end if;
            end loop;
         end if;

         if New_Block = 0 then
            Status := False;
            return;
         end if;

         --  Pause the transfer
         Device.CS.Active := False;

         --  Update the chain
         Prev := Tail;

         for J in 1 .. Num_Control_Blocks loop
            if Prev = 0 then
               Next := 1;
            elsif Prev = Num_SCB then
               Next := 1;
            else
               Next := Prev + 1;
            end if;

            if Prev /= 0 then
               SCB (DMA_SCB, Prev).Next_CB :=
                 To_BUS (SCB (DMA_SCB, Next).all'Address);
            end if;

            Prev := Next;
         end loop;

         SCB (DMA_SCB, Next).Next_CB := 0;

         --  Setup New_Block for use when calling Start_Transfer
         Current_Block := New_Block;
         Status        := True;

      exception
         when others =>
            GNAT.IO.Put_Line ("!!! Exception in Take_Transfer");
      end Take_Transfer;

      ------------------------
      -- Fill_Control_Block --
      ------------------------

      procedure Fill_Control_Block (CB : RPi.Regs.DMA.DMA_Control_Block)
      is
         Current : constant SCB_Access := SCB (DMA_SCB, Current_Block);
         Next    : constant BUS_Address := Current.Next_CB;
      begin
         Current.all := CB;

         --  Set back the next block value
         Current.Next_CB := Next;

         if Next = 0 then
            Current_Block := 0;
         elsif Current_Block = Num_SCB then
            Current_Block := 1;
         else
            Current_Block := Current_Block + 1;
         end if;
      end Fill_Control_Block;

      ------------------------
      -- Fill_Control_Block --
      ------------------------

      procedure Fill_Control_Block
        (CB           : RPi.Regs.DMA.DMA_Control_Block;
         Constant_Src : UInt32)
      is
         Current : constant SCB_Access := SCB (DMA_SCB, Current_Block);
      begin
         Fill_Control_Block (CB);

         --  Use Reserved_7 to hold the constant to be transfered
         Current.Reserved_7 := Constant_Src;
         Current.Source_Address :=
           To_BUS (Current.Reserved_7'Address);
      end Fill_Control_Block;

      --------------------
      -- Start_Transfer --
      --------------------

      procedure Start_Transfer is
      begin
         DMA_Started := True;

         --  Prepair the peripheral for the transfer
         if Device.CONBLK_AD = 0 then
            Device.CONBLK_AD := To_BUS (SCB (DMA_SCB, New_Block).all'Address);
         elsif Device.NEXTCONB = 0 then
            Device.NEXTCONB := To_BUS (SCB (DMA_SCB, New_Block).all'Address);
         end if;

         --  Start/resume the transfer
         Device.CS.Active := True;

         --  Reset New_Block
         New_Block := 0;
      end Start_Transfer;

      -----------------------
      -- DMA_Find_Free_SCB --
      -----------------------

      procedure DMA_Find_Free_SCB
        (Tail      : out SCB_Index;
         Available : out SCB_Index)
      is
         Current_Block : constant BUS_Address := Device.CONBLK_AD;
         Index         : SCB_Index;
         Next_Index    : SCB_Index;
         Transfering   : SCB_Index := 0;
         Num           : Natural := 0;

      begin
         if Current_Block = 0 then
            --  Easy case: no DMA transfer is in progress
            Tail := 0;
            Available := 1;

            return;
         end if;

         Transfering := Get_Index (DMA_SCB, To_ARM (Current_Block));
         Index       := Transfering;

         loop
            if Index = Num_SCB then
               Next_Index := 1;
            else
               Next_Index := Index + 1;
            end if;

            Num := Num + 1;

            if Next_Index = Transfering then
               --  All blocks are in use
               Tail := 0;
               Available := 0;

               return;
            end if;

            --  Search for the terminal block in the chain of transfers
            if SCB (DMA_SCB, Index).Next_CB = 0 then
               --  return the values
               Tail := Index;
               Available := Next_Index;

               return;
            end if;

            --  Move on to the next block
            Index := Next_Index;
         end loop;
      end DMA_Find_Free_SCB;
   end DMA_Controller;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Controller        : in out DMA_Controller;
      Total_Number_SCBs : Positive)
   is
      Handle    : Memory_Handle;
      BUS_Addr  : BUS_Address;

   begin
      if Controller.Initialized then
         return;
      end if;

      Memory_Allocate
        (UInt32 (Total_Number_SCBs * DMA_Control_Block'Size / 8),
         Alignment => 32,
         Flags     => Mem_Flag_L1_Non_Allocating or
           Mem_Flag_Hint_Permalock or Mem_Flag_Zero,
         Handle    => Handle);

      if Handle = Invalid_Memory_Handle then
         GNAT.IO.Put_Line ("DNA: Cannot allocate GPU memory");
      end if;

      Memory_Lock (Handle, BUS_Addr);

      if Debug then
         GNAT.IO.Put_Line ("DMA SCBs BUS addr at 0x" &
                             Image8 (UInt32 (BUS_Addr)));
      end if;

      Controller.Set_DMA_SCB (To_ARM (BUS_Addr), Total_Number_SCBs);

      if Controller.Device = DMA_0'Access then
         DMA_Enable.Enable_0 := True;
      elsif Controller.Device = DMA_1'Access then
         DMA_Enable.Enable_1 := True;
      elsif Controller.Device = DMA_2'Access then
         DMA_Enable.Enable_2 := True;
      elsif Controller.Device = DMA_3'Access then
         DMA_Enable.Enable_3 := True;
      elsif Controller.Device = DMA_4'Access then
         DMA_Enable.Enable_4 := True;
      elsif Controller.Device = DMA_5'Access then
         DMA_Enable.Enable_5 := True;
      elsif Controller.Device = DMA_6'Access then
         DMA_Enable.Enable_6 := True;
      elsif Controller.Device = DMA_7'Access then
         DMA_Enable.Enable_7 := True;
      elsif Controller.Device = DMA_8'Access then
         DMA_Enable.Enable_8 := True;
      elsif Controller.Device = DMA_9'Access then
         DMA_Enable.Enable_9 := True;
      elsif Controller.Device = DMA_10'Access then
         DMA_Enable.Enable_10 := True;
      elsif Controller.Device = DMA_11'Access then
         DMA_Enable.Enable_11 := True;
      elsif Controller.Device = DMA_12'Access then
         DMA_Enable.Enable_12 := True;
      elsif Controller.Device = DMA_13'Access then
         DMA_Enable.Enable_13 := True;
      elsif Controller.Device = DMA_14'Access then
         DMA_Enable.Enable_14 := True;
      else
         GNAT.IO.Put_Line ("Unknown DMA peripheral");
         raise Constraint_Error with "Unknown DMA periperal";
      end if;

      delay until Clock + Milliseconds (20);
      Controller.Device.CS.Reset := True;
      delay until Clock + Milliseconds (200);
      Controller.Device.CS :=
        (Disable_Debug  => True,
         Ended         => True, --  Resets the ENDED flag
         others => <>);
   end Initialize;

   -------------------
   -- Take_Transfer --
   -------------------

   procedure Take_Transfer
     (Controller         : in out DMA_Controller;
      Num_Control_Blocks : Natural;
      Status             : out Boolean)
   is
   begin
      Controller.Take_Transfer (Num_Control_Blocks, Status);
   exception
      when others =>
         GNAT.IO.Put_Line ("!!! Exception in Take_Transfer (procedure)");
   end Take_Transfer;

   -----------------------
   -- Set_Control_Block --
   -----------------------

   procedure Set_Control_Block
     (Controller : in out DMA_Controller;
      SCB        : RPi.Regs.DMA.DMA_Control_Block)
   is
   begin
      Controller.Fill_Control_Block (SCB);
   end Set_Control_Block;

   -----------------------
   -- Set_Control_Block --
   -----------------------

   procedure Set_Control_Block
     (Controller   : in out DMA_Controller;
      SCB          : RPi.Regs.DMA.DMA_Control_Block;
      Constant_Src : UInt32)
   is
   begin
      Controller.Fill_Control_Block (SCB, Constant_Src);
   end Set_Control_Block;

   --------------------
   -- Start_Transfer --
   --------------------

   procedure Start_Transfer (Controller : in out DMA_Controller)
   is
   begin
      Controller.Start_Transfer;
   end Start_Transfer;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer (Controller : in out DMA_Controller)
   is
   begin
      Controller.Wait_Transfer;
   end Wait_Transfer;

end RPi.DMA;
