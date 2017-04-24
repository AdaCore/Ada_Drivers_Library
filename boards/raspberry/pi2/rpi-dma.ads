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

--  ??? Strictly speaking, this driver should be placed in the device library,
--  not in the RPi board-specific library. However, to ease the implementation
--  we're using GPU memory via services that are provided as part of the RPi
--  firmware, so can't put it in the broadcom generic drivers.

with RPi.Regs.DMA;

--  The Broadcom DMA is using a linked list of Control Blocks to operate.
--  This allows the serialization of DMA transfers without the CPU to be
--  in charge of anything.
--
--  This also means that we don't have to wait for the end of the current
--  DMA transfer to start a new one.
--
--  To make all this happen, the following sequence of calls need to be
--  performed:
--    loop
--       Take_Transfer (DMA, 2, Status);
--       exit when Status;
--    end loop;
--    Set_Control_Block (DMA, SCB_1);
--    Set_Control_Block (DMA, SCB_2); --  in case of two successive transfers
--    Start_Transfer;
--
--  If you need a synchronisation point to know when the transfer is done, you
--  can call Wait_Transfer.
--
--  IMPORTANT NOTE: the DMA transfer pauses as soon as Take_Transfer is
--  successful. A Call to Start_Transfer HAS to be performed afterwards to
--  resume the transfer. Also the less time between Take_Transfer and
--  Start_Transfer, the less time is lost in transfering the data, if any
--  transfer has been interrupted.
package RPi.DMA is

   type DMA_Controller
     (Device : access RPi.Regs.DMA.DMA_Peripheral) is limited private;

   procedure Initialize
     (Controller        : in out DMA_Controller;
      Total_Number_SCBs : Positive);
   --  The Total number of SCBs

   procedure Take_Transfer
     (Controller         : in out DMA_Controller;
      Num_Control_Blocks : Natural;
      Status             : out Boolean);

   procedure Set_Control_Block
     (Controller : in out DMA_Controller;
      SCB        : RPi.Regs.DMA.DMA_Control_Block);

   procedure Set_Control_Block
     (Controller   : in out DMA_Controller;
      SCB          : RPi.Regs.DMA.DMA_Control_Block;
      Constant_Src : UInt32);

   procedure Start_Transfer (Controller : in out DMA_Controller);

   procedure Wait_Transfer (Controller : in out DMA_Controller);

private

   type SCB_Index is new Natural;
   subtype Valid_SCB_Index is SCB_Index range 1 .. SCB_Index'Last;

   type SCB_Array is
     array (Valid_SCB_Index range <>) of aliased RPi.Regs.DMA.DMA_Control_Block;
   type SCB_Array_Access is access all SCB_Array;
   pragma No_Strict_Aliasing (SCB_Array_Access);

   --  We consider here only ravenscar runtimes. There's no point in having a
   --  zfp runtime on the Pi, in the context of a multicore CPU.
   --  So we use a protected type here to make sure that bad things do not
   --  happen.
   --  However, we don't use entries: in the standard ravenscar profile,
   --  entries have just one slot, so if two tasks end up requiring it, it'll
   --  blow.
   protected type DMA_Controller (Device : access RPi.Regs.DMA.DMA_Peripheral)
   is
      function Initialized return Boolean;

      procedure Set_DMA_SCB
        (Arr : System.Address;
         Num : Positive);

      procedure Wait_Transfer;

      procedure Take_Transfer
        (Num_Control_Blocks : Natural;
         Status             : out Boolean);

      procedure Fill_Control_Block
        (CB : RPi.Regs.DMA.DMA_Control_Block);

      procedure Fill_Control_Block
        (CB           : RPi.Regs.DMA.DMA_Control_Block;
         Constant_Src : UInt32);
      --  This is a trick to transfer a constant via DMA. It uses one of the
      --  reserved field of the DMA control block to ensure that the value
      --  remains constant during the transfer

      procedure Start_Transfer;

   private
      procedure DMA_Find_Free_SCB
        (Tail      : out SCB_Index;
         Available : out SCB_Index);
      --  Find the first free control block
      --  Tail: index of the last block in the current chain
      --  Available: index of the first available block

      DMA_Started     : Boolean := False;
      New_Block       : SCB_Index := 0;
      Current_Block   : SCB_Index := 0;
      DMA_SCB         : System.Address;
      Num_SCB         : SCB_Index := 0;
   end DMA_Controller;

end RPi.DMA;
