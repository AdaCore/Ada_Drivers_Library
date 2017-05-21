with STM32_SVD.RCC;        use STM32_SVD.RCC;

with STM32.Device;         use STM32.Device;
with STM32.Board;          use STM32.Board;
with STM32.GPIO;           use STM32.GPIO;
with STM32.DMA;            use STM32.DMA;
with STM32.I2C;            use STM32.I2C;


with cs32l22;               use cs32l22;

package body HAL.Audio is

   procedure Set_Audio_Clock (Freq : Audio_Frequency);
   procedure Initialize_Audio_Out_Pins;
   procedure Initialize_Audio_I2C;

   --  Communication with the Audio chip
   --  This does not exist in the f4 stm32-i2c.ads file
   --  Audio_I2C_Addr  : constant I2C_Address := 16#34#;

   --  Driver : cs32l22_Device (Port     => Audio_I2C'Access,
   --                        I2C_Addr => Audio_I2C_Addr);

   --  AUDIO OUT

   --  DMA_Out         : STM32.DMA.DMA_Controller renames DMA_2;
   --  DMA_Out_Stream  : DMA_Stream_Selector renames Stream_4;
   --  DMA_Out_Channel : DMA_Channel_Selector renames Channel_3;

   --  AUDIO IN
--     SAI_In          : SAI_Controller renames SAI_2;
--     SAI_In_Block    : SAI_Block renames Block_B;
--     DMA_In          : STM32.DMA.DMA_Controller renames DMA_2;
--     DMA_In_Sream    : DMA_Stream_Selector renames Stream_7;
--     DMA_In_Channel  : DMA_Channel_Selector renames Channel_0;


   --------------------
   -- DMA_Out_Status --
   --------------------

   function DMA_Out_Status
     (Flag : STM32.DMA.DMA_Status_Flag) return Boolean
   is
   begin
      --  fixme: need to create real test
      return True;
   end DMA_Out_Status;

   --------------------------
   -- DMA_Out_Clear_Status --
   --------------------------

   procedure DMA_Out_Clear_Status
     (Flag : STM32.DMA.DMA_Status_Flag)
   is
   begin
      null;
   end DMA_Out_Clear_Status;

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
         when Audio_Freq_11kHz | Audio_Freq_22kHz | Audio_Freq_44kHz =>
            --  HSE/PLLM = 1MHz = PLLI2S VCO Input
            null;

         when Audio_Freq_8kHz  | Audio_Freq_16kHz |
              Audio_Freq_48kHz | Audio_Freq_96kHz =>
            null;
      end case;
   end Set_Audio_Clock;

   -------------------------------
   -- Initialize_Audio_Out_Pins --
   -------------------------------

   procedure Initialize_Audio_Out_Pins
   is

   begin
      null;
   end Initialize_Audio_Out_Pins;


   --------------------------
   -- Initialize_Audio_I2C --
   --------------------------

   procedure Initialize_Audio_I2C
   is
   begin
      null;
   end Initialize_Audio_I2C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Audio_Out
     (Volume    : Audio_Volume;
      Frequency : Audio_Frequency)
   is
   begin

      Set_Audio_Clock (Frequency);

      --  Initialize the SAI
      Initialize_Audio_Out_Pins;

      --  Initialize the I2C Port to send commands to the driver
      Initialize_Audio_I2C;


   end Initialize_Audio_Out;

   ----------
   -- Play --
   ----------

   procedure Play
     (Buffer : Audio_Buffer)
   is
   begin
      null;
   end Play;

   -------------------
   -- Change_Buffer --
   -------------------

   procedure Change_Buffer
     (Buffer : Audio_Buffer)
   is
   begin
      null;
   end Change_Buffer;

   -----------
   -- Pause --
   -----------

   procedure Pause is
   begin

      null;
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume
   is
   begin
      null;
   end Resume;

   ----------
   -- Stop --
   ----------

   procedure Stop
   is
   begin
      null;
   end Stop;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume
     (Volume : Audio_Volume)
   is
   begin
      null;
   end Set_Volume;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency
     (Frequency : Audio_Frequency)
   is
   begin
      Set_Audio_Clock (Frequency);

   end Set_Frequency;

end HAL.Audio;
