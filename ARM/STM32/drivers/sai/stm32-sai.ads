------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--   @file    stm32f7xx_hal_sai.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.0.2                                                        --
--   @date    21-September-2015                                             --
--   @brief   This file provides firmware functions to manage the following --
--            functionalities of the Serial Audio Interface (SAI)           --
--            peripheral:                                                   --
--             + Initialization/de-initialization functions                 --
--             + I/O operation functions                                    --
--             + Peripheral Control functions                               --
--             + Peripheral State functions                                 --
--                                                                          --
--   COPYRIGHT(c) 2015 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32.Device;

package STM32.SAI is

   subtype SAI_Controller is STM32.Device.SAI_Port;
   type SAI_Block is (Block_A, Block_B);

   subtype Audio_Frequency is Word;

   type SAI_Mono_Stereo_Mode is
     (Stereo,
      Mono)
     with Size => 1;

   type SAI_Audio_Mode is
     (Master_Transmitter,
      Master_Receiver,
      Slave_Transmitter,
      Slave_Receiver)
     with Size => 2;
   --  When the audio mode is configured in SPDIF mode, the master transmitter
   --  mode is forced. In Master transmitter mode, the audio block starts
   --  generating the FS and the clocks immediately.

   type SAI_Protocol_Configuration is
     (Free_Protocol,
      SPDIF_Protocol,
      AC97_Protocol)
     with Size => 2;
   --  The Free protocol allows to use the powerful configuration of the
   --  audio block to address a specific audio protocol (such as I2S, LSB/MSB
   --  justified, TDM, PCM/DSP...) by setting most of the configuration
   --  register bits as well as frame configuration register.

   for SAI_Protocol_Configuration use
     (Free_Protocol  => 0,
      SPDIF_Protocol => 1,
      AC97_Protocol  => 2);

   type SAI_Data_Size is
     (Data_8b,
      Data_10b,
      Data_16b,
      Data_20b,
      Data_24b,
      Data_32b)
     with Size => 3;
   --  These bits are ignored when the SPDIF protocols are selected (bit
   --  PRTCFG[1:0]), because the frame and the data size are fixed in such
   --  case. When the companding mode is selected through COMP[1:0] bits,
   --  DS[1:0] are ignored since the data size is fixed to 8 bits by the
   --  algorithm.
   for SAI_Data_Size use
     (Data_8b  => 2,
      Data_10b => 3,
      Data_16b => 4,
      Data_20b => 5,
      Data_24b => 6,
      Data_32b => 7);

   type SAI_Endianness is
     (Data_MSB_First,
      Data_LSB_First)
     with Size => 1;

   type SAI_Clock_Strobing_Edge is
     (Clock_Strobing_Falling_Edge,
      Clock_Strobing_Rising_Edge)
     with Size => 1;
   --  This has no meanings in SPDIF audio mode,
   --
   --  Clock_Strobing_Falling_Edge:
   --  Signals generated by the SAI change on SCK rising edge, while
   --  signals received by the SAI are sampled on the SCK falling edge.
   --
   --  Clock_Strobing_Rising_Edge:
   --  Signals generated by the SAI change on SCK falling edge, while signals
   --  received by the SAI are sampled on the SCK rising edge.

   type SAI_Synchronization is
     (Asynchronous_Mode,
      Synchronous_Mode,
      Synchronous_Ext_Mode)
     with Size => 2;
   --  Asynchronous_Mode:
   --  audio sub-block in asynchronous mode.
   --  Synchronous_Mode:
   --  audio sub-block is synchronous with the other internal audio sub-block.
   --  In this case, the audio sub-block must be configured in slave mode.
   --  Synchronous_Ext_Mode:
   --  audio sub-block is synchronous with an external SAI embedded peripheral.
   --  In this case the audio sub-block should be configured in Slave mode.

   type SAI_Output_Drive is
     (Drive_On_SAI_Enabled,
      Drive_Immediate)
     with Size => 1;
   --  To be set before enabling the Audio Block and after the audio block
   --  configuration


   type SAI_Master_Clock_Divider is new UInt4;
   --  Meaningless when the audio block operates in slave mode. They have to be
   --  configured when the audio block is disabled.
   --  2#0000#: Divides by 1 the master clock input
   --  Others: the master clock frequency is calculated according to the
   --          following formula:
   --    F_SCK = F_SAI_CK / (Master_Clock_Divider x 2)

   type SAI_FIFO_Threshold is
     (FIFO_Empty,
      FIFO_1_Quarter_Full,
      FIFO_Half_Full,
      FIFO_3_Quarters_Full,
      FIFO_Full)
     with Size => 3;

   type SAI_Tristate_Management is
     (SD_Line_Driven,
      SD_Line_Released);
   --  Meaningful only if the audio block is configured as a transmitter. This
   --  bit is not used when the audio block is configured in SPDIF mode. It
   --  should be configured when SAI is disabled.
   --  SD_Line_Driven:
   --  SD output line is still driven by the SAI when a slot is inactive.
   --  SD_Line_Released:
   --  SD output line is released (HI-Z) at the end of the last data bit of the
   --  last active slot if the next one is inactive.

   type SAI_Companding_Mode is
     (No_Companding,
      M_Law_Algorithm,
      A_Law_Algorithm);
   --  Telecommunication applications can require to process the data to be
   --  transmitted or received using a data companding algorithm. The two
   --  companding modes supported are the μ-Law and the A-Law log which are
   --  a part of the CCITT G.711 recommendation.
   --

   type SAI_Frame_Synchronization is
     (FS_Frame,
      FS_Frame_And_Channel_Identification)
     with Size => 1;
   --  Meaningless and is not used in AC’97 or SPDIF audio block
   --  configuration. It must be configured when the audio block is disabled.
   --
   --  In case of FS_Frame_And_Channel_Identification, the number of slots
   --  defined in the SAI_xSLOTR register has to be even. It means that half of
   --  this number of slots will be dedicated to the left channel and the other
   --  slots for the right channel (e.g: this bit has to be set for I2S or
   --  MSB/LSB-justified protocols...).
   --
   --  FS_Frame: FS signal is a start frame signal.
   --  FS_Frame_And_Channel_Identification: FS signal is a start of frame
   --    signal + channel side identification.

   type SAI_Frame_Sync_Polarity is
     (FS_Active_Low,
      FS_Active_High)
     with Size => 1;
   --  It is used to configure the level of the start of frame on the FS
   --  signal. It is meaningless and is not used in AC’97 or SPDIF audio
   --  block configuration.
   --
   --  FS_Active_Low: FS is active low (falling edge)
   --  FS_Active_High: FS is active high (rising edge)

   type SAI_Frame_Sync_Offset is
     (First_Bit,
      Before_First_Bit)
     with Size => 1;
   --  Meaningless and is not used in AC’97 or SPDIF audio block
   --  configuration. This bit must be configured when the audio block
   --  is disabled.
   --  First_Bit: FS is asserted on the first bit of the slot 0.
   --  Before_First_Bit: FS is asserted one bit before the first bit of the
   --    slot 0.

   type SAI_Slot_Size is
     (Data_Size,
      Slot_16b,
      Slot_32b)
     with Size => 2;
   --  The slot size must be higher or equal to the data size. If this
   --  condition is not respected, the behavior of the SAI will be
   --  undetermined.
   --  Ignored in AC’97 or SPDIF mode.
   --  Data_Size: The slot size is equivalent to the data size (specified in
   --    DS[3:0] in the SAI_xCR1 register).

   type Slots_Number is range 1 .. 16;

   type SAI_Slots is new Short;

   Slot_0  : constant SAI_Slots := 2 ** 0;
   Slot_1  : constant SAI_Slots := 2 ** 1;
   Slot_2  : constant SAI_Slots := 2 ** 2;
   Slot_3  : constant SAI_Slots := 2 ** 3;
   Slot_4  : constant SAI_Slots := 2 ** 4;
   Slot_5  : constant SAI_Slots := 2 ** 5;
   Slot_6  : constant SAI_Slots := 2 ** 6;
   Slot_7  : constant SAI_Slots := 2 ** 7;
   Slot_8  : constant SAI_Slots := 2 ** 8;
   Slot_9  : constant SAI_Slots := 2 ** 9;
   Slot_10 : constant SAI_Slots := 2 ** 10;
   Slot_11 : constant SAI_Slots := 2 ** 11;
   Slot_12 : constant SAI_Slots := 2 ** 12;
   Slot_13 : constant SAI_Slots := 2 ** 13;
   Slot_14 : constant SAI_Slots := 2 ** 14;
   Slot_15 : constant SAI_Slots := 2 ** 15;

   procedure Deinitialize
     (Periph : SAI_Controller;
      Block  : SAI_Block);

   function Enabled
     (Periph : SAI_Controller;
      Block  : SAI_Block) return Boolean
     with Inline;

   procedure Enable
     (Periph : SAI_Controller;
      Block  : SAI_Block)
     with Inline;

   procedure Disable
     (Periph : SAI_Controller;
      Block  : SAI_Block)
     with Inline;

   procedure Enable_DMA
     (Periph : SAI_Controller;
      Block  : SAI_Block)
     with Inline;

   procedure Configure_Audio_Block
     (Periph          : SAI_Controller;
      Block           : SAI_Block;
      Frequency       : Audio_Frequency;
      Stereo_Mode     : SAI_Mono_Stereo_Mode;
      Mode            : SAI_Audio_Mode;
      MCD_Enabled     : Boolean;
      Protocol        : SAI_Protocol_Configuration;
      Data_Size       : SAI_Data_Size;
      Endianness      : SAI_Endianness;
      Clock_Strobing  : SAI_Clock_Strobing_Edge;
      Synchronization : SAI_Synchronization;
      Output_Drive    : SAI_Output_Drive;
      FIFO_Threshold  : SAI_FIFO_Threshold;
      Tristate_Mgt    : SAI_Tristate_Management := SD_Line_Driven;
      Companding_Mode : SAI_Companding_Mode := No_Companding);

   procedure Configure_Block_Frame
     (Periph          : SAI_Controller;
      Block           : SAI_Block;
      Frame_Length    : Byte;
      Frame_Active    : UInt7;
      Frame_Sync      : SAI_Frame_Synchronization;
      FS_Polarity     : SAI_Frame_Sync_Polarity;
      FS_Offset       : SAI_Frame_Sync_Offset);

   procedure Configure_Block_Slot
     (Periph           : SAI_Controller;
      Block            : SAI_Block;
      First_Bit_Offset : UInt5;
      Slot_Size        : SAI_Slot_Size;
      Number_Of_Slots  : Slots_Number;
      Enabled_Slots    : SAI_Slots);

end STM32.SAI;
