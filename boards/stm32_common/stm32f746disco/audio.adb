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
--  This file is based on:                                                  --
--   @file    stm32f746g_discovery_audio.c                                  --
--   @author  MCD Application Team                                          --
------------------------------------------------------------------------------

with HAL;          use HAL;
with STM32;        use STM32;
with STM32.Device; use STM32.Device;
with STM32.Board;  use STM32.Board;
with STM32.GPIO;   use STM32.GPIO;
with STM32.DMA;    use STM32.DMA;
with STM32.SAI;    use STM32.SAI;
with STM32.Setup;

package body Audio is

   Audio_SAI     : SAI_Controller renames SAI_2;
   SAI2_MCLK_A   : GPIO_Point renames PI4;
   SAI2_SCK_A    : GPIO_Point renames PI5;
   SAI2_SD_A     : GPIO_Point renames PI6;
   SAI2_SD_B     : GPIO_Point renames PG10;
   SAI2_FS_A     : GPIO_Point renames PI7;
   SAI_Pins      : constant GPIO_Points :=
                     (SAI2_MCLK_A, SAI2_SCK_A, SAI2_SD_A, SAI2_SD_B,
                      SAI2_FS_A);
   SAI_Pins_AF   : GPIO_Alternate_Function renames GPIO_AF_SAI2_10;

   --  SAI in/out conf
   SAI_Out_Block : SAI_Block renames Block_A;
--     SAI_In_Block  : SAI_Block renames Block_B;

   procedure Set_Audio_Clock (Freq : Audio_Frequency);
   procedure Initialize_Audio_Out_Pins;
   procedure Initialize_SAI_Out (Freq : Audio_Frequency);
   procedure Initialize_Audio_I2C;

   ---------------------
   -- Set_Audio_Clock --
   ---------------------

   procedure Set_Audio_Clock (Freq : Audio_Frequency)
   is
   begin
      --  Two groups of frequencies: the 44kHz family and the 48kHz family
      --  The Actual audio frequency is calculated then with the following
      --  formula:
      --  Master_Clock = 256 * FS = SAI_CK / Master_Clock_Divider
      --  We need to find a value of SAI_CK that allows such integer master
      --  clock divider
      case Freq is
         when Audio_Freq_11kHz | Audio_Freq_22kHz |
              Audio_Freq_32kHz | Audio_Freq_44kHz =>
            --  HSE/PLLM = 1MHz = PLLI2S VCO Input
            Configure_SAI_I2S_Clock
              (Audio_SAI,
               PLLI2SN    => 429,  --  VCO Output = 429MHz
               PLLI2SQ    => 2,    --  SAI Clk(First level) = 214.5 MHz
               PLLI2SDIVQ => 19);  --  I2S Clk = 215.4 / 19 = 11.289 MHz

         when Audio_Freq_8kHz  | Audio_Freq_16kHz |
              Audio_Freq_48kHz | Audio_Freq_96kHz =>
            Configure_SAI_I2S_Clock
              (Audio_SAI,
               PLLI2SN    => 344,  --  VCO Output = 344MHz
               PLLI2SQ    => 7,    --  SAI Clk(First level) = 49.142 MHz
               PLLI2SDIVQ => 1);  --  I2S Clk = 49.142 MHz
      end case;
   end Set_Audio_Clock;

   -------------------------------
   -- Initialize_Audio_Out_Pins --
   -------------------------------

   procedure Initialize_Audio_Out_Pins
   is
   begin
      Enable_Clock (Audio_SAI);
      Enable_Clock (SAI_Pins);

      Configure_IO
        (SAI_Pins,
         (Mode           => Mode_AF,
          AF             => SAI_Pins_AF,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_High,
          Resistors      => Floating));

      Enable_Clock (Audio_DMA);

      --  Configure the DMA channel to the SAI peripheral
      Disable (Audio_DMA, Audio_DMA_Out_Stream);
      Configure
        (Audio_DMA,
         Audio_DMA_Out_Stream,
         (Channel                      => Audio_DMA_Out_Channel,
          Direction                    => Memory_To_Peripheral,
          Increment_Peripheral_Address => False,
          Increment_Memory_Address     => True,
          Peripheral_Data_Format       => HalfWords,
          Memory_Data_Format           => HalfWords,
          Operation_Mode               => Circular_Mode,
          Priority                     => Priority_High,
          FIFO_Enabled                 => True,
          FIFO_Threshold               => FIFO_Threshold_Full_Configuration,
          Memory_Burst_Size            => Memory_Burst_Single,
          Peripheral_Burst_Size        => Peripheral_Burst_Single));
      Clear_All_Status (Audio_DMA, Audio_DMA_Out_Stream);
   end Initialize_Audio_Out_Pins;

   ------------------------
   -- Initialize_SAI_Out --
   ------------------------

   procedure Initialize_SAI_Out (Freq : Audio_Frequency)
   is
   begin
      STM32.SAI.Disable (Audio_SAI, SAI_Out_Block);
      STM32.SAI.Configure_Audio_Block
        (Audio_SAI,
         SAI_Out_Block,
         Frequency       => Audio_Frequency'Enum_Rep (Freq),
         Stereo_Mode     => Stereo,
         Mode            => Master_Transmitter,
         MCD_Enabled     => True,
         Protocol        => Free_Protocol,
         Data_Size       => Data_16b,
         Endianness      => Data_MSB_First,
         Clock_Strobing  => Clock_Strobing_Rising_Edge,
         Synchronization => Asynchronous_Mode,
         Output_Drive    => Drive_Immediate,
         FIFO_Threshold  => FIFO_1_Quarter_Full);
      STM32.SAI.Configure_Block_Frame
        (Audio_SAI,
         SAI_Out_Block,
         Frame_Length => 64,
         Frame_Active => 32,
         Frame_Sync   => FS_Frame_And_Channel_Identification,
         FS_Polarity  => FS_Active_Low,
         FS_Offset    => Before_First_Bit);
      STM32.SAI.Configure_Block_Slot
        (Audio_SAI,
         SAI_Out_Block,
         First_Bit_Offset => 0,
         Slot_Size        => Data_Size,
         Number_Of_Slots  => 4,
         Enabled_Slots    => Slot_0 or Slot_2);
      STM32.SAI.Enable (Audio_SAI, SAI_Out_Block);
   end Initialize_SAI_Out;

   --------------------------
   -- Initialize_Audio_I2C --
   --------------------------

   procedure Initialize_Audio_I2C
   is
   begin
      STM32.Setup.Setup_I2C_Master (Port        => Audio_I2C,
                                    SDA         => Audio_I2C_SDA,
                                    SCL         => Audio_I2C_SCL,
                                    SDA_AF      => Audio_I2C_AF,
                                    SCL_AF      => Audio_I2C_AF,
                                    Clock_Speed => 100_000);
   end Initialize_Audio_I2C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Audio_Out
     (This      : in out WM8994_Audio_Device;
      Volume    : Audio_Volume;
      Frequency : Audio_Frequency)
   is
   begin
      STM32.SAI.Deinitialize (Audio_SAI, SAI_Out_Block);

      Set_Audio_Clock (Frequency);

      --  Initialize the SAI
      Initialize_Audio_Out_Pins;
      Initialize_SAI_Out (Frequency);

      --  Initialize the I2C Port to send commands to the driver
      Initialize_Audio_I2C;

      if This.Device.Read_ID /= WM8994.WM8994_ID then
         raise Constraint_Error with "Invalid ID received from the Audio Code";
      end if;

      This.Device.Reset;
      This.Device.Init
        (Input     => WM8994.No_Input,
         Output    => WM8994.Auto,
         Volume    => UInt8 (Volume),
         Frequency =>
           WM8994.Audio_Frequency'Enum_Val
             (Audio_Frequency'Enum_Rep (Frequency)));
   end Initialize_Audio_Out;

   ----------
   -- Play --
   ----------

   procedure Play
     (This   : in out WM8994_Audio_Device;
      Buffer : Audio_Buffer)
   is
   begin
      This.Device.Play;

      Start_Transfer_with_Interrupts
        (This               => Audio_DMA,
         Stream             => Audio_DMA_Out_Stream,
         Source             => Buffer (Buffer'First)'Address,
         Destination        => Audio_SAI.ADR'Address,
         Data_Count         => Buffer'Length,
         Enabled_Interrupts => (Half_Transfer_Complete_Interrupt => True,
                                Transfer_Complete_Interrupt      => True,
                                others                           => False));

      Enable_DMA (Audio_SAI, SAI_Out_Block);

      if not Enabled (Audio_SAI, SAI_Out_Block) then
         Enable (Audio_SAI, SAI_Out_Block);
      end if;
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause (This : in out WM8994_Audio_Device) is
   begin
      This.Device.Pause;
      DMA_Pause (Audio_SAI, SAI_Out_Block);
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume (This : in out WM8994_Audio_Device)
   is
   begin
      This.Device.Resume;
      DMA_Resume (Audio_SAI, SAI_Out_Block);
   end Resume;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out WM8994_Audio_Device)
   is
   begin
      This.Device.Stop (WM8994.Stop_Power_Down_Sw);
      DMA_Stop (Audio_SAI, SAI_Out_Block);

      STM32.DMA.Disable (Audio_DMA, Audio_DMA_Out_Stream);
      STM32.DMA.Clear_All_Status (Audio_DMA, Audio_DMA_Out_Stream);
   end Stop;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume
     (This   : in out WM8994_Audio_Device;
      Volume : Audio_Volume)
   is
   begin
      This.Device.Set_Volume (UInt8 (Volume));
   end Set_Volume;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency
     (This      : in out WM8994_Audio_Device;
      Frequency : Audio_Frequency)
   is
      pragma Unreferenced (This);
   begin
      Set_Audio_Clock (Frequency);
      STM32.SAI.Disable (Audio_SAI, SAI_Out_Block);
      Initialize_SAI_Out (Frequency);
      STM32.SAI.Enable (Audio_SAI, SAI_Out_Block);
   end Set_Frequency;

end Audio;
