------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_dma.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of DMA HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the DMA controllers on the STM32F4 (ARM
--  Cortex M4F) microcontrollers from ST Microelectronics.

--  See Application Note AN4031: "Using the STM32F2 and STM32F4 DMA controller"
--  and Reference Manual RM0090: "STM32F405xx/07xx, STM32F415xx/17xx,
--  STM32F42xxx and STM32F43xxx advanced ARM-based 32-bit MCUs" In the
--  application note, see especially section four, titled "Tips and
--  warnings while programming the DMA controller"

--  The basic call sequence, given a Controller and a Stream, is as follows:

--  1) Configure

--     Configures the Controller and Stream per application requirements. This
--     is the primary setup call, specifying the static characteristics of all
--     the transfers to be performed on the stream, such as the direction, the
--     channel, and so forth. The Controller is disabled after the call.

--  2) Configure_Data_Flow

--     Sets the dynamic parameters of a given transfer, i.e., the source and
--     destination addresses and the number of data items to transfer.

--  3) Enable

--     Enables transfers on the Controller and Stream. Transfers will begin
--     immediately unless programmed otherwise.

--  You can enable some or all DMA interrupts prior to the call to Enable, if
--  required by your usage.

--  Ensure all the status flags are cleared prior to the call to Enable, since
--  a transfer will then begin. This can be accomplished by relying on the fact
--  that the board has just powered-up, by a call to Reset, or by a call to
--  Clear_All_Status.

--  Note that there are convenience routines that do steps two and three:
--     Start_Transfer
--     Start_Transfer_with_Interrupts

pragma Restrictions (No_Elaboration_Code);

with System;         use System;
with Ada.Real_Time;  use Ada.Real_Time;

private with STM32_SVD.DMA;

package STM32.DMA is

   type DMA_Controller is limited private;

   --  Do not change the order of the enumerals in the types in this package.
   --  The underlying canonical representation values are required.

   type DMA_Stream_Selector is
     (Stream_0,
      Stream_1,
      Stream_2,
      Stream_3,
      Stream_4,
      Stream_5,
      Stream_6,
      Stream_7);

   procedure Enable
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with Inline;
   --  Before enabling a stream to start a new transfer, the event status flags
   --  corresponding to the stream must be cleared. Note that the unit may not
   --  be enabled by the time the call returns.

   procedure Disable
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with
       Post => not Enabled (Unit, Stream),
       Inline;

   function Enabled
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean with Inline;

   procedure Reset
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
     with
       Post =>
         not Enabled (Unit, Stream)                               and
         Operating_Mode (Unit, Stream) = Normal_Mode              and
         Current_Counter (Unit, Stream) = 0                       and
         Selected_Channel (Unit, Stream) = Channel_0              and
         Transfer_Direction (Unit, Stream) = Peripheral_To_Memory and
         not Double_Buffered (Unit, Stream)                       and
         not Circular_Mode (Unit, Stream)                         and
         Memory_Data_Width (Unit, Stream) = Bytes                 and
         Peripheral_Data_Width (Unit, Stream) = Bytes             and
         Priority (Unit, Stream) = Priority_Low                   and
         Current_Memory_Buffer (Unit, Stream) = Memory_Buffer_0   and
         (for all Flag in DMA_Status_Flag =>
            not Status (Unit, Stream, Flag))                      and
         (for all Interrupt in DMA_Interrupt =>
            not Interrupt_Enabled (Unit, Stream, Interrupt));
       --  In addition,
       --  M_Burst = Memory_Burst_Single and
       --  P_Burst = Peripheral_Burst_Single and
       --  P_Inc_Offset_Size = 0 and
       --  M_Inc_Mode = False and
       --  P_Inc_Mode = False
   --  Also clears the FIFO control register bits except sets bits to show FIFO
   --  is empty, and to set the FIFO filling threshold selection to 1/2 full.

   procedure Configure_Data_Flow
     (Unit        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : Short)
     with
       Pre =>
         not Enabled (Unit, Stream) and
         Valid_Addresses (Source, Destination) and
         Compatible_Alignments (Unit, Stream, Source, Destination);
   --  Sets the source and destination arguments within the specified stream,
   --  based on the direction previously specified by a call to procedure
   --  Configure.
   --
   --  Sets the number of data items to be transferred (from 0 to 65535) on
   --  the specified stream in the next transfer. This is the volume of data to
   --  be transferred from source to destination. The number specified depends
   --  only on the peripheral data format, as specified by the record component
   --  Peripheral_Data_Format passed to a call to Configure. The value to be
   --  specified is computed as follows:
   --
   --     If the peripheral data format is in units of bytes, the value is
   --     equal to the total number of bytes contained in the data to be sent.
   --
   --     If the peripheral data format is in units of half-words, the value is
   --     1/2 the total number of bytes contained in the data to be sent.
   --
   --     If the peripheral data format is in units of words, the value is
   --     1/4 the total number of bytes contained in the data to be sent.
   --
   --  For example, to send a sequence of characters to a USART, the USART
   --  peripheral format will be in units of bytes so the Data_Count argument
   --  will be the number of characters (bytes) in the string to be sent.
   --  In contrast, on a memory-to-memory transfer the most efficient approach
   --  is to work in units of words. One would therefore specify word units for
   --  the source and destination formats and then specify 1/4 the total number
   --  of bytes involved (assuming a four-byte word).

   procedure Start_Transfer
     (Unit        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : Short)
     with
       Pre  =>
            Valid_Addresses (Source, Destination)
         and
            Compatible_Alignments (Unit, Stream, Source, Destination)
         and
            (for all Flag in DMA_Status_Flag =>
             (not Status (Unit, Stream, Flag)));
   --  Convenience routine: disables the stream, calls Configure_Data_Flow,
   --  and then enables the stream to start the transfer. DMA interrupts are
   --  not enabled by this routine, but could be enabled prior to the call.
   --  The requirement to clear the flags first is due to the fact that
   --  the transfer begins immediately at the end of this routine. The
   --  value specified for Data_Count is as described for procedure
   --  Configure_Data_Flow.

   type DMA_Interrupt is
     (Direct_Mode_Error_Interrupt,
      Transfer_Error_Interrupt,
      Half_Transfer_Complete_Interrupt,
      Transfer_Complete_Interrupt,
      FIFO_Error_Interrupt);

   type Interrupt_Selections is array (DMA_Interrupt) of Boolean;

   procedure Start_Transfer_with_Interrupts
     (Unit               : DMA_Controller;
      Stream             : DMA_Stream_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : Short;
      Enabled_Interrupts : Interrupt_Selections := (others => True))
     with
       Pre =>
             Valid_Addresses (Source, Destination)
          and
             Compatible_Alignments (Unit, Stream, Source, Destination)
          and
             (for all Flag in DMA_Status_Flag =>
              (not Status (Unit, Stream, Flag)));
   --  Convenience routine: disables the stream, calls Configure_Data_Flow,
   --  enables the selected DMA interrupts (by default, all of them), and
   --  then enables the stream to start the transfer. All the selected DMA
   --  interrupts are enabled, all the others are left unchanged. Interrupts
   --  are selected for enablement by having a True value in the array at their
   --  index location. The requirement to clear the flags first is due to the
   --  fact that the transfer begins immediately at the end of this routine.
   --  The value specified for Data_Count is as described for procedure
   --  Configure_Data_Flow.

   type DMA_Error_Code is
     (DMA_No_Error,
      DMA_Transfer_Error,
      DMA_FIFO_Error,
      DMA_Direct_Mode_Error,
      DMA_Timeout_Error,
      DMA_Device_Error);

   procedure Abort_Transfer
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Result : out DMA_Error_Code)
     with Post => not Enabled (Unit, Stream);
   --  Disables the specified stream and then waits until the request is
   --  effective. If a stream is disabled while a data transfer is ongoing, the
   --  current datum will be transferred and the stream will be disabled only
   --  after the transfer of this single datum completes.

   type DMA_Transfer_Level is
     (Full_Transfer,
      Half_Transfer);

   procedure Poll_For_Completion
     (Unit           : in out DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Expected_Level : DMA_Transfer_Level;
      Timeout        : Time_Span;
      Result         : out DMA_Error_Code);

   procedure Set_Counter
     (Unit       : DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Count : Short)
     with
       Pre  => not Enabled (Unit, Stream),
       Post => Current_Counter (Unit, Stream) = Data_Count,
       Inline;
   --  Sets the number of data items to be transferred on the stream.
   --  The Data_Count parameter specifies the number of data items to be
   --  transferred (from 0 to 65535) on the next transfer. The value is
   --  as described for procedure Configure_Data_Flow.

   function Current_Counter
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Short
     with Inline;
   --  Returns the number of remaining data units to be transferred

   function Circular_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
     with Inline;

   procedure Enable_Interrupt
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
     with
       Post => Interrupt_Enabled (Unit, Stream, Source);
   --  The postcondition should not be relied upon completely because it is
   --  possible, under just the wrong conditions, for the interrupt to be
   --  disabled immediately, prior to return from this routine

   procedure Disable_Interrupt
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
     with
       Post => not Interrupt_Enabled (Unit, Stream, Source);

   function Interrupt_Enabled
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
      return Boolean
     with Inline;

   type DMA_Status_Flag is
     (FIFO_Error_Indicated,
      Direct_Mode_Error_Indicated,
      Transfer_Error_Indicated,
      Half_Transfer_Complete_Indicated,
      Transfer_Complete_Indicated);

   procedure Clear_Status
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
     with
       Post => not Status (Unit, Stream, Flag),
       Inline;

   procedure Clear_All_Status
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
     with Post =>
       (for all Indicated in DMA_Status_Flag =>
          not Status (Unit, Stream, Indicated));

   function Status
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
      return Boolean
     with Inline;
   --  Returns whether the specified status flag is indicated

   type DMA_Channel_Selector is
     (Channel_0,
      Channel_1,
      Channel_2,
      Channel_3,
      Channel_4,
      Channel_5,
      Channel_6,
      Channel_7);

   function Selected_Channel
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Channel_Selector
     with Inline;

   type DMA_Data_Transfer_Direction is
     (Peripheral_To_Memory,
      Memory_To_Peripheral,
      Memory_To_Memory);
   --  Note that only DMA_2 is able to do Memory_To_Memory transfers, and that
   --  in this direction the circular mode is not allowed and the internal FIFO
   --  must be enabled.

   function Transfer_Direction
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Direction
     with Inline;

   type DMA_Data_Transfer_Widths is
     (Bytes,
      HalfWords,
      Words);

   function Peripheral_Data_Width
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
     with Inline;

   function Memory_Data_Width
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
     with Inline;

   type DMA_Mode is
     (Normal_Mode,
      Peripheral_Flow_Control_Mode,
      Circular_Mode);

   function Operating_Mode
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Mode
     with Inline;

   type DMA_Priority_Level is
     (Priority_Low,
      Priority_Medium,
      Priority_High,
      Priority_Very_High);

   function Priority
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Priority_Level
     with Inline;

   type Memory_Buffer_Target is (Memory_Buffer_0, Memory_Buffer_1);

   function Current_Memory_Buffer
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return Memory_Buffer_Target
     with Inline;

   procedure Select_Current_Memory_Buffer
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target)
     with Inline;

   procedure Set_Memory_Buffer
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target;
      To     : System.Address)
     with Inline;

   procedure Configure_Double_Buffered_Mode
     (Unit              : DMA_Controller;
      Stream            : DMA_Stream_Selector;
      Buffer_0_Value    : Address;
      Buffer_1_Value    : Address;
      First_Buffer_Used : Memory_Buffer_Target)
     with
       Pre  => not Enabled (Unit, Stream),
       Post => not Enabled (Unit, Stream) and
               Current_Memory_Buffer (Unit, Stream) = First_Buffer_Used;
   --  A convenience routine that in effect calls Set_Memory_Buffer
   --  once each for Buffer_1_Value and Buffer_2_Value, and then calls
   --  Select_Current_Memory_Buffer so that First_Buffer_Used is the
   --  buffer used first when the stream is enabled.

   procedure Enable_Double_Buffered_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with
       Pre => Circular_Mode (Unit, Stream) and
              Transfer_Direction (Unit, Stream) /= Memory_To_Memory,
       Post => Double_Buffered (Unit, Stream);

   procedure Disable_Double_Buffered_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with Post => not Double_Buffered (Unit, Stream);

   function Double_Buffered
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
     with Inline;

   type DMA_FIFO_Threshold_Level is
     (FIFO_Threshold_1_Quart_Full_Configuration,
      FIFO_Threshold_Half_Full_Configuration,
      FIFO_Threshold_3_Quarts_Full_Configuration,
      FIFO_Threshold_Full_Configuration);

   type DMA_FIFO_Filling_State is
     (FIFO_Less1QuarterFull,  --  less than 1 quarter full but not empty
      FIFO_1QuarterFull,      --  more than 1 quarter full
      FIFO_HalfFull,          --  more than 1 half full
      FIFO_3QuartersFull,     --  more than 3 quarters full
      FIFO_Empty,
      FIFO_Full);

   type DMA_Memory_Burst is
     (Memory_Burst_Single,
      Memory_Burst_Inc4,
      Memory_Burst_Inc8,
      Memory_Burst_Inc16);

   type DMA_Peripheral_Burst is
     (Peripheral_Burst_Single,
      Peripheral_Burst_Inc4,
      Peripheral_Burst_Inc8,
      Peripheral_Burst_Inc16);

   type DMA_Stream_Configuration is record
      --  These are the static, non-varying properties of the transactions
      --  occurring on the streams to which they are applied (by a call to
      --  Configure). Other, varying, properties are specified procedurally.
      --
      --  You are not required to specify a value for every component because
      --  some are only referenced depending on the values for others. Note,
      --  however, that the default values specified do not represent a valid
      --  configuration as a whole.

      Channel                      : DMA_Channel_Selector :=
                                       DMA_Channel_Selector'First;
      --  The channel in the multiplexed connections of controllers, streams,
      --  and peripherals. It is vital to note that not all peripherals can
      --  be connected to all streams. The possibilities are organized by
      --  channels, per controller, as specified by the ST Micro Reference
      --  Manual in the "DMA Request Mapping" tables.

      Direction                    : DMA_Data_Transfer_Direction :=
                                       DMA_Data_Transfer_Direction'First;

      Increment_Peripheral_Address : Boolean := False;
      --  Whether the peripheral address value should be incremented
      --  automatically after each transfer

      Increment_Memory_Address     : Boolean := False;
      --  Whether the memory address value should be incremented automatically
      --  after each transfer

      Peripheral_Data_Format       : DMA_Data_Transfer_Widths :=
                                       DMA_Data_Transfer_Widths'First;
      --  The units of data (the format) in which the peripheral side of the
      --  transaction is expressed. For example, a USART would work in terms
      --  of bytes. See the description in Configure_Data_Flow.

      Memory_Data_Format           : DMA_Data_Transfer_Widths :=
                                       DMA_Data_Transfer_Widths'First;
      --  The units of data (the format) in which the memory side of the
      --  transaction is expressed. See the description in Configure_Data_Flow.

      Operation_Mode               : DMA_Mode := DMA_Mode'First;
      --  Note that the circular buffer mode cannot be used if memory-to-memory
      --  data transfer is configured on the selected Stream

      Priority                     : DMA_Priority_Level :=
                                       DMA_Priority_Level'First;
      --  The relative priority of the given stream to all other streams

      FIFO_Enabled                 : Boolean := False;
      --  Specifies whether the internal FIFO will be used for the transactions
      --  occurring on the specified stream. By default the FIFO is disabled by
      --  the hardware, and so the unit works in the so-called "direct mode"
      --  instead. Per the Application Note, enabling the FIFO is highly
      --  advantageous. Note that the direct mode cannot be used if
      --  memory-to-memory data transfer is configured. The threshold and
      --  burst sizes are only considered if the FIFO is enabled, and the
      --  corresponding values are highly dependent upon one another!

      FIFO_Threshold               : DMA_FIFO_Threshold_Level :=
                                       DMA_FIFO_Threshold_Level'First;
      --  The threshold at which the FIFO is refilled. It is vital that the
      --  threshold and burst sizes, if specified, are compatible. See the
      --  Reference Manual and especially the Application Note.

      Memory_Burst_Size            : DMA_Memory_Burst :=
                                       DMA_Memory_Burst'First;
      --  Specifies the amount of data to be transferred in a single non-
      --  interruptible transaction. Note: The burst mode is possible only if
      --  the address increment mode is enabled.

      Peripheral_Burst_Size        : DMA_Peripheral_Burst :=
                                       DMA_Peripheral_Burst'First;
      --  Specifies the the amount of data to be transferred in
      --  a single non-interruptible transaction. Note :The burst mode is
      --  possible only if the address increment mode is enabled.
   end record;

   procedure Configure
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Config : DMA_Stream_Configuration)
     with Post => not Enabled (Unit, Stream);
   --  This is the primary stream configuration facility. All the static
   --  properties of the transfers for the given stream are specified here,
   --  and in some cases, nowhere else (such as the channel). The required
   --  relationships between the parameters specified in the record are
   --  not checked, other than by the hardware itself.
   --
   --  Note that not all required properties are specified here. In particular,
   --  because they can vary per transfer, the source and destination
   --  addresses, as well as the number of data items to be transferred,
   --  are specified procedurally via calls to Configure_Data_Flow.

   function Valid_Addresses (Source, Destination : Address) return Boolean is
     (Source /= Null_Address and Destination /= Null_Address and
      Source /= Destination);
   --  Basic sanity checking for the values

   function Aligned (This : Address;  Width : DMA_Data_Transfer_Widths)
      return Boolean with Inline;
   --  Returns whether the address is aligned on a word, half-word, or byte
   --  boundary

   function Compatible_Alignments
     (Unit           : DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Source         : Address;
      Destination    : Address)
      return Boolean is
     (case Transfer_Direction (Unit, Stream) is
         when Peripheral_To_Memory | Memory_To_Memory =>
            Aligned (Source, Peripheral_Data_Width (Unit, Stream))
            and
            Aligned (Destination, Memory_Data_Width (Unit, Stream)),
         when Memory_To_Peripheral =>
            Aligned (Source, Memory_Data_Width (Unit, Stream))
            and
            Aligned (Destination, Peripheral_Data_Width (Unit, Stream)));
   --  Based on Ref Manual Table 44 and associated text, checks the alignments
   --  of the addresses against the Peripheral_Data_Format (P_Data_Size) and
   --  Memory_Data_Format (M_Data_Size) values for the given stream. We use an
   --  expression function because the semantics are meant to be part of the
   --  spec of the package, visible as a precondition.

private

   type DMA_Controller is new STM32_SVD.DMA.DMA_Peripheral;

end STM32.DMA;
