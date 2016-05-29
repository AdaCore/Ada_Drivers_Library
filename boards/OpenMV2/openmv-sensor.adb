with STM32.DCMI;
with STM32.DMA;     use STM32.DMA;
with Ada.Real_Time; use Ada.Real_Time;
with STM32.PWM;     use STM32.PWM;
with OV2640;        use OV2640;
with Interfaces;    use Interfaces;
with HAL.I2C;       use HAL.I2C;
with HAL.Bitmap;    use HAL.Bitmap;
with HAL;           use HAL;

package body OpenMV.Sensor is

   package DCMI renames STM32.DCMI;
   function Probe (Cam_Addr : out UInt10) return Boolean;

   REG_PID : constant := 16#0A#;
   --  REG_VER : constant := 16#0B#;

   CLK_PWM_Mod : PWM_Modulator;
   Camera      : OV2640_Cam (Sensor_I2C'Access);
   Is_Initialized : Boolean := False;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is (Is_Initialized);

   -----------
   -- Probe --
   -----------

   function Probe (Cam_Addr : out UInt10) return Boolean is
      Status : I2C_Status;
   begin
      for Addr in UInt10 range 0 .. 126 loop
         Sensor_I2C.Master_Transmit (Addr    => Addr,
                                     Data    => (0 => 0),
                                     Status  => Status,
                                     Timeout => 10_000);
         if Status = Ok then
            Cam_Addr := Addr;
            return True;
         end if;
      end loop;
      return False;
   end Probe;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Initialize_Clock;
      procedure Initialize_Camera;
      procedure Initialize_IO;
      procedure Initialize_DCMI;
      procedure Initialize_DMA;

      ----------------------
      -- Initialize_Clock --
      ----------------------

      procedure Initialize_Clock is
      begin
         Initialise_PWM_Modulator
           (This                => CLK_PWM_Mod,
            Requested_Frequency => Float (SENSOR_CLK_FREQ),
            PWM_Timer           => SENSOR_CLK_TIM'Access,
            PWM_AF              => SENSOR_CLK_AF);

         Attach_PWM_Channel (This    => CLK_PWM_Mod,
                             Channel => SENSOR_CLK_CHAN,
                             Point   => SENSOR_CLK_IO);

         Set_Duty_Cycle (This    => CLK_PWM_Mod,
                         Channel => SENSOR_CLK_CHAN,
                         Value   => 50);
         Enable_PWM_Channel (CLK_PWM_Mod, SENSOR_CLK_CHAN);
      end Initialize_Clock;

      -----------------------
      -- Initialize_Camera --
      -----------------------

      procedure Initialize_Camera is
         Cam_Addr : UInt10;
         Data : I2C_Data (1 .. 1);
         Status : I2C_Status;
         PID : HAL.Byte;
      begin

         --  Power cycle
         Set (DCMI_PWDN);
         delay until Clock + Milliseconds (10);
         Clear (DCMI_PWDN);
         delay until Clock + Milliseconds (10);

         Initialize_Clock;

         Set (DCMI_RST);
         delay until Clock + Milliseconds (10);
         Clear (DCMI_RST);
         delay until Clock + Milliseconds (10);

         if  not Probe (Cam_Addr) then

            --  Retry with reversed reset polarity
            Clear (DCMI_RST);
            delay until Clock + Milliseconds (10);
            Set (DCMI_RST);
            delay until Clock + Milliseconds (10);

            if  not Probe (Cam_Addr) then
               raise Program_Error;
            end if;
         end if;

         --  Select sensor bank
         Sensor_I2C.Mem_Write (Addr          => Cam_Addr,
                               Mem_Addr      => 16#FF#,
                               Mem_Addr_Size => Memory_Size_8b,
                               Data          => (0 => 1),
                               Status        => Status);
         if Status /= Ok then
            raise Program_Error;
         end if;

         Sensor_I2C.Mem_Read (Addr          => Cam_Addr,
                              Mem_Addr      => REG_PID,
                              Mem_Addr_Size => Memory_Size_8b,
                              Data          => Data,
                              Status        => Status);
         if Status /= Ok then
            raise Program_Error;
         end if;
         PID := Data (Data'First);

         if PID /= OV2640_PID then
            raise Program_Error;
         end if;

         Initialize (Camera, Cam_Addr);
         Set_Pixel_Format (Camera, Pix_RGB565);
         Set_Frame_Size (Camera, QQVGA2);
      end Initialize_Camera;

      -------------------
      -- Initialize_IO --
      -------------------

      procedure Initialize_IO is
         GPIO_Conf : GPIO_Port_Configuration;
         DCMI_AF_Points : constant GPIO_Points :=
           GPIO_Points'(DCMI_D0, DCMI_D1, DCMI_D2, DCMI_D3, DCMI_D4,
                        DCMI_D5, DCMI_D6, DCMI_D7, DCMI_VSYNC, DCMI_HSYNC,
                        DCMI_PCLK);
         DCMI_Out_Points : GPIO_Points :=
           GPIO_Points'(DCMI_PWDN, DCMI_RST, FS_IN);
      begin
         Enable_Clock (GPIO_Points'(Sensor_I2C_SCL, Sensor_I2C_SDA));

         GPIO_Conf.Speed       := Speed_25MHz;
         GPIO_Conf.Mode        := Mode_AF;
         GPIO_Conf.Output_Type := Open_Drain;
         GPIO_Conf.Resistors   := Floating;
         Configure_IO
           (GPIO_Points'(Sensor_I2C_SCL, Sensor_I2C_SDA), GPIO_Conf);

         Configure_Alternate_Function (Sensor_I2C_SCL, Sensor_I2C_SCL_AF);
         Configure_Alternate_Function (Sensor_I2C_SDA, Sensor_I2C_SDA_AF);

         Enable_Clock (Sensor_I2C);
         Reset (Sensor_I2C);
         Enable_Clock (Sensor_I2C);

         Configure
           (Sensor_I2C,
            (Mode                     => I2C_Mode,
             Duty_Cycle               => DutyCycle_2,
             Own_Address              => 16#00#,
             Addressing_Mode          => Addressing_Mode_7bit,
             General_Call_Enabled     => False,
             Clock_Stretching_Enabled => True,
             Clock_Speed              => 10_000));

         Enable_Clock (DCMI_Out_Points);
         --  Sensor PowerDown, Reset and FSIN
         GPIO_Conf.Mode := Mode_Out;
         GPIO_Conf.Output_Type := Push_Pull;
         GPIO_Conf.Resistors := Pull_Down;
         Configure_IO (DCMI_Out_Points, GPIO_Conf);

         Clear (DCMI_Out_Points);

         GPIO_Conf.Mode := Mode_AF;
         GPIO_Conf.Output_Type := Push_Pull;
         GPIO_Conf.Resistors := Pull_Down;
         Configure_IO (DCMI_AF_Points, GPIO_Conf);
         Configure_Alternate_Function (DCMI_AF_Points, GPIO_AF_DCMI);
      end Initialize_IO;

      ---------------------
      -- Initialize_DCMI --
      ---------------------

      procedure Initialize_DCMI is
      begin
         Enable_DCMI_Clock;
         DCMI.Configure (Data_Mode            => DCMI.DCMI_8bit,
                         Capture_Rate         => DCMI.Capture_All,
                         --  Sensor specific (OV2640)
                         Vertical_Polarity    => DCMI.Active_Low,
                         Horizontal_Polarity  => DCMI.Active_Low,
                         Pixel_Clock_Polarity => DCMI.Active_High,

                         Hardware_Sync        => True,
                         JPEG                 => False);
         DCMI.Disable_Crop;
         DCMI.Enable_DCMI;
      end Initialize_DCMI;

      --------------------
      -- Initialize_DMA --
      --------------------

      procedure Initialize_DMA is
         Config : DMA_Stream_Configuration;
      begin
         Enable_Clock (Sensor_DMA);
         Config.Channel := Sensor_DMA_Chan;
         Config.Direction := Peripheral_To_Memory;
         Config.Increment_Peripheral_Address := False;
         Config.Increment_Memory_Address := True;
         Config.Peripheral_Data_Format := Words;
         Config.Memory_Data_Format := Words;
         Config.Operation_Mode := Normal_Mode;
         Config.Priority := Priority_High;
         Config.FIFO_Enabled := True;
         Config.FIFO_Threshold := FIFO_Threshold_Full_Configuration;
         Config.Memory_Burst_Size := Memory_Burst_Inc4;
         Config.Peripheral_Burst_Size := Peripheral_Burst_Single;
         Configure (Sensor_DMA, Sensor_DMA_Stream, Config);
      end Initialize_DMA;
   begin
      Initialize_IO;
      Initialize_Camera;
      Initialize_DCMI;
      Initialize_DMA;
      Is_Initialized := True;
   end Initialize;

   --------------
   -- Snapshot --
   --------------

   procedure Snapshot (BM : HAL.Bitmap.Bitmap_Buffer'Class) is
      Status : DMA_Error_Code;
   begin
      if BM.Width /= Image_Width or else BM.Height /= Image_Height then
         raise Program_Error;
      end if;

      if not Compatible_Alignments (Sensor_DMA,
                                    Sensor_DMA_Stream,
                                    DCMI.Data_Register_Address,
                                    BM.Addr)
      then
         raise Program_Error;
      end if;

      Start_Transfer (Unit        => Sensor_DMA,
                      Stream      => Sensor_DMA_Stream,
                      Source      => DCMI.Data_Register_Address,
                      Destination => BM.Addr,
                      Data_Count  =>
                        Interfaces.Unsigned_16 ((BM.Width * BM.Height) / 2));

      DCMI.Start_Capture (DCMI.Snapshot);

      Poll_For_Completion (Sensor_DMA,
                           Sensor_DMA_Stream,
                           Full_Transfer,
                           Milliseconds (1000),
                           Status);
      if Status /= DMA_No_Error then
         Abort_Transfer (Sensor_DMA, Sensor_DMA_Stream, Status);
         pragma Unreferenced (Status);
         raise Program_Error;
      end if;
   end Snapshot;

end OpenMV.Sensor;
