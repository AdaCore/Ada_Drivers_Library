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

with System;
with Interfaces;              use Interfaces;

package RPi.Regs.DMA is

   type DMA_Operation_Width is
     (Width_32bit,
      Width_128bit) with Size => 1;

   type DMA_INT_Status_Register is record
      Int0_Status : Boolean;
      Int1_Status : Boolean;
      Int2_Status : Boolean;
      Int3_Status : Boolean;
      Int4_Status : Boolean;
      Int5_Status : Boolean;
      Int6_Status : Boolean;
      Int7_Status : Boolean;
      Int8_Status : Boolean;
      Int9_Status : Boolean;
      Int10_Status : Boolean;
      Int11_Status : Boolean;
      Int12_Status : Boolean;
      Int13_Status : Boolean;
      Int14_Status : Boolean;
      Int15_Status : Boolean;
   end record with Size => 32, Volatile_Full_Access;

   for DMA_INT_Status_Register use record
      Int0_Status  at 0 range  0 ..  0;
      Int1_Status  at 0 range  1 ..  1;
      Int2_Status  at 0 range  2 ..  2;
      Int3_Status  at 0 range  3 ..  3;
      Int4_Status  at 0 range  4 ..  4;
      Int5_Status  at 0 range  5 ..  5;
      Int6_Status  at 0 range  6 ..  6;
      Int7_Status  at 0 range  7 ..  7;
      Int8_Status  at 0 range  8 ..  8;
      Int9_Status  at 0 range  9 ..  9;
      Int10_Status at 0 range 10 .. 10;
      Int11_Status at 0 range 11 .. 11;
      Int12_Status at 0 range 12 .. 12;
      Int13_Status at 0 range 13 .. 13;
      Int14_Status at 0 range 14 .. 14;
      Int15_Status at 0 range 15 .. 15;
   end record;

   type DMA_Enable_Register is record
      Enable_0 : Boolean;
      Enable_1 : Boolean;
      Enable_2 : Boolean;
      Enable_3 : Boolean;
      Enable_4 : Boolean;
      Enable_5 : Boolean;
      Enable_6 : Boolean;
      Enable_7 : Boolean;
      Enable_8 : Boolean;
      Enable_9 : Boolean;
      Enable_10 : Boolean;
      Enable_11 : Boolean;
      Enable_12 : Boolean;
      Enable_13 : Boolean;
      Enable_14 : Boolean;
      Enable_15 : Boolean;
   end record with Size => 32, Volatile_Full_Access;

   for DMA_Enable_Register use record
      Enable_0  at 0 range  0 ..  0;
      Enable_1  at 0 range  1 ..  1;
      Enable_2  at 0 range  2 ..  2;
      Enable_3  at 0 range  3 ..  3;
      Enable_4  at 0 range  4 ..  4;
      Enable_5  at 0 range  5 ..  5;
      Enable_6  at 0 range  6 ..  6;
      Enable_7  at 0 range  7 ..  7;
      Enable_8  at 0 range  8 ..  8;
      Enable_9  at 0 range  9 ..  9;
      Enable_10 at 0 range 10 .. 10;
      Enable_11 at 0 range 11 .. 11;
      Enable_12 at 0 range 12 .. 12;
      Enable_13 at 0 range 13 .. 13;
      Enable_14 at 0 range 14 .. 14;
      Enable_15 at 0 range 15 .. 15;
   end record;

   -----------------------------------
   --  DMA CONTROL BLOCK DEFINITION --
   -----------------------------------

   type DMA_Transfer_Information is record
      Interrupt_Enable : Boolean := False;
      Two_D_Mode       : Boolean := False;
      --  2D Mode interprets the TXFR_LEN register as YLENGTH number of
      --  transfers each of XLENGTH, and add the strides to the address after
      --  each transfer.
      --  If false, the linear mode iterpret the TXFR register as a single
      --  transfer of total length (YLENGTH, XLENGTH)

      Wait_Response    : Boolean := False;
      --  Makes the DMA wait until it receives the AXI write response for each
      --  write. This ensures that multiple writes cannot get stacked in the
      --  AXI bus pipeline.

      Reserved_2_2     : Boolean := False;

      Dest_Inc         : Boolean := False;
      --  Destination address increment after each write.
      --  The address will increment by 4 if Dest_Width is Width_32bit,
      --  else by 32 (128bit transfer).

      Dest_Width       : DMA_Operation_Width := Width_32bit;
      --  Destination transfer width

      Dest_Dreq        : Boolean := False;
      --  if set, the DREQ selected by PERMAP will gate the destination writes.

      Dest_Ignore      : Boolean := False;
      --  do not perform destination writes

      Src_Inc          : Boolean := False;
      Src_Width        : DMA_Operation_Width := Width_32bit;
      Src_Dreq         : Boolean := False;
      Src_Ignore       : Boolean := False;

      Burst_Length     : UInt4 := 0;
      --  Indicates the burst length of the DMA transfers. The DMA will attempt
      --  to transfer data as bursts of this number of words. A value of zero
      --  will produce a single transfer. Bursts are only produced for specific
      --  conditions.

      Permap           : UInt5 := 0;
      --  Indicates the peripheral number (1-31) whose ready signal shall be
      --  used to control the rate of the transfers, and whose panic signals
      --  will be output on the DMA AXI bus. Set to 0 for continuous un-paced
      --  transfer.

      Waits            : UInt5 := 0;
      --  This slows down the DMA throughput by setting the number of dummy
      --  cycles burnt after each DMA read or write operation is completed.
      --  A value of 0 means that no wait cycles are to be added.

      No_Wide_Bursts   : Boolean := False;
      --  This prevents the DMA from issuing wide writes as 2 beat AXI bursts.
      --  This is an inefficient access mode, so the default is to use the
      --  bursts.

      Reserved_27_31   : UInt5 := 0;
   end record;

   for DMA_Transfer_Information use record
      Interrupt_Enable at  0 range  0 ..  0;
      Two_D_Mode       at  0 range  1 ..  1;
      Reserved_2_2     at  0 range  2 ..  2;
      Wait_Response    at  0 range  3 ..  3;
      Dest_Inc         at  0 range  4 ..  4;
      Dest_Width       at  0 range  5 ..  5;
      Dest_Dreq        at  0 range  6 ..  6;
      Dest_Ignore      at  0 range  7 ..  7;
      Src_Inc          at  0 range  8 ..  8;
      Src_Width        at  0 range  9 ..  9;
      Src_Dreq         at  0 range 10 .. 10;
      Src_Ignore       at  0 range 11 .. 11;
      Burst_Length     at  0 range 12 .. 15;
      Permap           at  0 range 16 .. 20;
      Waits            at  0 range 21 .. 25;
      No_Wide_Bursts   at  0 range 26 .. 26;
      Reserved_27_31   at  0 range 27 .. 31;
   end record;

   type DMA_Transfer_Length (TD_Mode : Boolean := False) is record
      Reserved : UInt2 := 0;

      case TD_Mode is
         when False =>
            Length   : UInt30;
         when True =>
            X_Length : UInt16;
            Y_Length : UInt14;
      end case;
   end record with Pack, Unchecked_Union, Volatile_Full_Access, Size => 32;

   for DMA_Transfer_Length use record
      Length   at 0 range  0 .. 29;
      X_Length at 0 range  0 .. 15;
      Y_Length at 0 range 16 .. 29;
      Reserved at 0 range 30 .. 31;
   end record;

   type DMA_2D_Stride is record
      S_STRIDE : Integer_16 := 0;
      --  Signed (2s complement) byte increment to apply to the source address
      --  at the end of each row in 2D mode.
      D_STRIDE : Integer_16 := 0;
      --  Signed (2s complement) byte increment to apply to the destination
      --  address at the end of each row in 2D mode.
   end record;

   for DMA_2D_Stride use record
      S_STRIDE at 0 range 0 .. 15;
      D_STRIDE at 0 range 16 .. 31;
   end record;

   subtype DMA_Source_Address is BUS_Address;
   subtype DMA_Destination_Address is BUS_Address;
   subtype DMA_Next_Control_Block is BUS_Address;

   --  The DMA Control block tells the DMA peripheral what transfer is to
   --  be performed. They can be chained, so that several successive DMA
   --  transfers can be executed without the CPU to be involved.
   type DMA_Control_Block is record
      TI                  : DMA_Transfer_Information;
      Source_Address      : DMA_Source_Address;
      Destination_Address : DMA_Destination_Address;
      Transfer_Length     : DMA_Transfer_Length;
      Stride              : DMA_2D_Stride;
      Next_CB             : DMA_Next_Control_Block;
      Reserved_6          : UInt32 := 0;
      Reserved_7          : UInt32 := 0;
   end record;

   --  The DMA control block must be aligned on 256 bits.
   for DMA_Control_Block'Alignment use 32;

   for DMA_Control_Block use record
      TI                  at  0 range 0 .. 31;
      Source_Address      at  4 range 0 .. 31;
      Destination_Address at  8 range 0 .. 31;
      Transfer_Length     at 12 range 0 .. 31;
      Stride              at 16 range 0 .. 31;
      Next_CB             at 20 range 0 .. 31;
      Reserved_6          at 24 range 0 .. 31;
      Reserved_7          at 28 range 0 .. 31;
   end record;

   --------------------------------
   -- DMA PERIPHERALS DEFINITION --
   --------------------------------

   type DMA_Control_Status_Register is record
      Active                         : Boolean := False;
      Ended                          : Boolean := False;
      Int                            : Boolean := False;
      D_Req                          : Boolean := False;
      Paused                         : Boolean := False;
      D_Req_Stops_DMA                : Boolean := False;
      Waiting_For_Outstanding_Writes : Boolean := False;
      Reserved_7_7                   : Boolean := False;
      Error                          : Boolean := False;
      Reserved_9_15                  : UInt7   := 0;
      Priority                       : UInt4   := 0;
      Panic_Priority                 : UInt4   := 0;
      Reserved_24_27                 : UInt4   := 0;
      Wait_For_Outstanding_Writes    : Boolean := False;
      Disable_Debug                  : Boolean := False;
      Abort_Transfer                 : Boolean := False;
      Reset                          : Boolean := False;
   end record with Volatile_Full_Access, Size => 32;

   for DMA_Control_Status_Register use record
      Active                         at 0 range  0 .. 0;
      Ended                          at 0 range  1 .. 1;
      Int                            at 0 range  2 .. 2;
      D_Req                          at 0 range  3 .. 3;
      Paused                         at 0 range  4 .. 4;
      D_Req_Stops_DMA                at 0 range  5 .. 5;
      Waiting_For_Outstanding_Writes at 0 range  6 .. 6;
      Reserved_7_7                   at 0 range  7 .. 7;
      Error                          at 0 range  8 .. 8;
      Reserved_9_15                  at 0 range  9 .. 15;
      Priority                       at 0 range 16 .. 19;
      Panic_Priority                 at 0 range 20 .. 23;
      Reserved_24_27                 at 0 range 24 .. 27;
      Wait_For_Outstanding_Writes    at 0 range 28 .. 28;
      Disable_Debug                  at 0 range 29 .. 29;
      Abort_Transfer                 at 0 range 30 .. 30;
      Reset                          at 0 range 31 .. 31;
   end record;

   subtype DMA_Control_Block_Address_Register is BUS_Address
     with Dynamic_Predicate =>
       (DMA_Control_Block_Address_Register mod 32) = 0;

   type DMA_Debug_Register is record
      Read_Last_Not_Set_Error : Boolean := False;
      FIFO_Error              : Boolean := False;
      Read_Error              : Boolean := False;
      Reserved_3_3            : Bit     := 0;
      Outstanding_Writes      : UInt4   := 0;
      DMA_ID                  : UInt8   := 0;
      DMA_State               : UInt9   := 0;
      Version                 : UInt3   := 0;
      Lite                    : Boolean := False;
      Reserved_29_31          : UInt3   := 0;
   end record with Volatile_Full_Access, Size => 32;

   for DMA_Debug_Register use record
      Read_Last_Not_Set_Error at 0 range 0 .. 0;
      FIFO_Error              at 0 range 1 .. 1;
      Read_Error              at 0 range 2 .. 2;
      Reserved_3_3            at 0 range 3 .. 3;
      Outstanding_Writes      at 0 range 4 .. 7;
      DMA_ID                  at 0 range 8 .. 15;
      DMA_State               at 0 range 16 .. 24;
      Version                 at 0 range 25 .. 27;
      Lite                    at 0 range 28 .. 28;
      Reserved_29_31          at 0 range 29 .. 31;
   end record;

   type DMA_Peripheral is record
      CS        : DMA_Control_Status_Register; --  R/W
      CONBLK_AD : DMA_Control_Block_Address_Register; --  R/W
      TI        : DMA_Transfer_Information;
      SOURCE_AD : BUS_Address;
      DEST_AD   : BUS_Address;
      TXFR_LEN  : DMA_Transfer_Length;
      STRIDE    : DMA_2D_Stride;
      NEXTCONB  : DMA_Control_Block_Address_Register;
      DEBUG     : DMA_Debug_Register; --  ??? TODO
   end record with Volatile;

   for DMA_Peripheral use record
      CS        at 16#00# range 0 .. 31;
      CONBLK_AD at 16#04# range 0 .. 31;
      TI        at 16#08# range 0 .. 31;
      SOURCE_AD at 16#0C# range 0 .. 31;
      DEST_AD   at 16#10# range 0 .. 31;
      TXFR_LEN  at 16#14# range 0 .. 31;
      STRIDE    at 16#18# range 0 .. 31;
      NEXTCONB  at 16#1C# range 0 .. 31;
      DEBUG     at 16#20# range 0 .. 31;
   end record;

   DMA_0  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base);
   DMA_1  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#100#);
   DMA_2  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#200#);
   DMA_3  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#300#);
   DMA_4  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#400#);
   DMA_5  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#500#);
   DMA_6  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#600#);
   DMA_7  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#700#);
   DMA_8  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#800#);
   DMA_9  : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#900#);
   DMA_10 : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#A00#);
   DMA_11 : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#B00#);
   DMA_12 : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#C00#);
   DMA_13 : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#D00#);
   DMA_14 : aliased DMA_Peripheral
     with Import, Address => System'To_Address (DMA_Base + 16#E00#);

   DMA_INT_Status : aliased DMA_INT_Status_Register
     with Import, Address => System'To_Address (DMA_Base + 16#FE0#);
   DMA_Enable     : aliased DMA_Enable_Register
     with Import, Address => System'To_Address (DMA_Base + 16#FF0#);

end RPi.Regs.DMA;
