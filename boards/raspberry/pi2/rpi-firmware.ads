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

--  Mailbox messages for the Raspberri Pi firmware are documented at:
--  https://github.com/raspberrypi/firmware/wiki/Mailboxes
--
--  This API can be used in two different ways. The more complete way is used
--  to send multiple requests to the firmware at once. To do so, the following
--  sequence is to be used:
--    Lock;
--    Offset1 := Add_Message (Tag1, Data1);
--    Offset2 := Add_Message (Tag2, Data2);
--    Do_Transaction;
--    Data1 := Get_Result (Offset1);
--    Data2 := Get_Result (Offset2);
--    Unlock;
--
--  If you just need to send a single command, then you can use the simpler
--  API:
--    Fw_Request (Tag, Data);
--  or
--    Fw_Request_RO (Tag, value);
--
package RPi.Firmware is

   type ARM_To_VC_Tag is
     (Tag_Get_Firmware_Revision,
      Tag_Get_Board_Model,
      Tag_Get_Board_Revision,
      Tag_Get_Board_MAC_Address,
      Tag_Get_Board_Serial,
      Tag_Get_ARM_Memory,
      Tag_Get_VC_Memory,
      Tag_Get_Clocks,
      Tag_Get_Command_Line,
      Tag_Get_DMA_Channels,
      Tag_Get_Power_State,
      Tag_Get_Timing,
      Tag_Set_Power_State,
      Tag_Get_Clock_State,
      Tag_Set_Clock_State,
      Tag_Get_Clock_Rate,
      Tag_Set_Clock_Rate,
      Tag_Get_Max_Clock_Rate,
      Tag_Get_Min_Clock_Rate,
      Tag_Get_Turbo,
      Tag_Set_Turbo,
      Tag_Get_Voltage,
      Tag_Set_Voltage,
      Tag_Get_Max_Voltage,
      Tag_Get_Min_Voltage,
      Tag_Get_Temperature,
      Tag_Get_Max_Temperature,
      Tag_Get_STC,
      Tag_Allocate_Memory,
      Tag_Lock_Memory,
      Tag_Unlock_Memory,
      Tag_Release_Memory,
      Tag_Execute_Code,
      Tag_Execute_QPU,
      Tag_Set_Enable_QPU,
      Tag_Get_Dispmanx_Resource_Mem_Handle,
      Tag_Get_EDID_Block,
      Tag_Get_Customer_OTP,
      Tag_Set_Customer_OTP,
      Tag_Get_Domain_State,
      Tag_Set_Domain_State,
      Tag_Set_SDHost_Clock,
      Tag_Allocate_Buffer,
      Tag_Release_Buffer,
      Tag_Blank_Screen,
      Tag_Get_Physical_Display_Size,
      Tag_Test_Physical_Display_Size,
      Tag_Set_Physical_Display_Size,
      Tag_Get_Virtual_Buffer_Size,
      Tag_Test_Virtual_Buffer_Size,
      Tag_Set_Virtual_Buffer_Size,
      Tag_Get_Depth,
      Tag_Test_Depth,
      Tag_Set_Depth,
      Tag_Get_Pixel_Order,
      Tag_Test_Pixel_Order,
      Tag_Set_Pixel_Order,
      Tag_Get_Alpha_Mode,
      Tag_Test_Alpha_Mode,
      Tag_Set_Alpha_Mode,
      Tag_Get_Pitch,
      Tag_Get_Virtual_Offset,
      Tag_Test_Virtual_Offset,
      Tag_Set_Virtual_Offset,
      Tag_Get_Overscan,
      Tag_Test_Overscan,
      Tag_Set_Overscan,
      Tag_Get_Palette,
      Tag_Test_Palette,
      Tag_Set_Palette,
      Tag_Set_VSync,
      Tag_Set_Backlight,
      Tag_VCHIQ_Init,
      Tag_Get_Touch_Buf,
      Tag_Set_Touch_Buf,
      Tag_Get_GPIO_Virtual_Buffer,
      Tag_Set_GPIO_Virtual_Buffer,
      Tag_Set_Cursor_Info,
      Tag_Set_Cursor_State);

   type Device_Id is
     (SDCard_Device,
      UART0_Device,
      UART1_Device,
      USB_HCD_Device,
      I2C0_Device,
      I2C1_Device,
      I2C2_Device,
      SPI_Device,
      CCP2TX_Device)
     with Size => 32;

   type Clock_Id is
     (Reserved,
      EMMC_Clock,
      UART_Clock,
      ARM_Clock,
      Core_Clock,
      V3D_Clock,
      H264_Clock,
      ISP_Clock,
      SDRAM_Clock,
      Pixel_Clock,
      PWM_Clock)
     with Size => 32;

   type Voltage_Id is
     (Reserved,
      Core_Voltage,
      SDRAM_C_Voltage,
      SDRAM_P_Voltage,
      SDRAM_I_Voltage)
     with Size => 32;

--     type Device_State is
--       (Off,
--        On,
--        Not_Present)
--       with Size => 32;

   procedure Initialize;

   --------------------
   -- SIMPLE REQUEST --
   --------------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag);

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt8_Array);

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt32_Array);

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt32);

   generic
      type T is private;
   procedure Gen_Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out T);

   --  Below are cases where the result is not important

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt8_Array);

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32_Array);

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32);

   generic
      type T is private;
   procedure Gen_Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : T);

   -----------------------
   -- MULTIPLE REQUESTS --
   -----------------------

   procedure Lock;
   --  Locks the interface and reserve the firmware message pool
   --  A call to unlock needs to be performed afterwards

   procedure Unlock;
   --  Unlocks the interface and discards the internal data

   procedure Add_Message (Tag   : ARM_To_VC_Tag);
   --  Add a message that does not have data

   function Add_Message (Tag   : ARM_To_VC_Tag;
                         Input : UInt8_Array) return Natural;
   --  Returns an index on the message data, used to retrieve the results
   --  afterwards

   function Add_Message (Tag   : ARM_To_VC_Tag;
                         Input : UInt32_Array) return Natural;
   --  Same as above, using words instead of bytes.

   function Add_Message (Tag   : ARM_To_VC_Tag;
                         Input : UInt32) return Natural;
   --  Same as above, using a single word

   generic
      type T is private;
   function Gen_Add_Message (Tag   : ARM_To_VC_Tag; Input : T) return Natural;
   --  Same as above, with a generic parameter

   function Do_Transaction return Boolean;
   --  Calls the firmware with the messages added using Add_Message above.
   --  Returns False upon error.

   procedure Do_Transaction;
   --  Calls the firmware with the messages added using Add_Message above.
   --  Ignores the response status

   function Get_Result (Offset : Natural; Size : Natural) return UInt8_Array;
   --  Returns the value of the data at Offset.

   function Get_Result (Offset : Natural; Size : Natural) return UInt32_Array;
   --  Same as above, using words instead of bytes.

   function Get_Result (Offset : Natural) return UInt32;
   --  Same as above, using a single word.

   generic
      type T is private;
   function Gen_Get_Result (Offset : Natural) return T;
   --  Same as above, using a generic type.

private

   ARM_To_VC_Tag_Code : constant array (ARM_To_VC_Tag) of UInt32 :=
     (Tag_Get_Firmware_Revision            => 16#0_0001#,
      Tag_Get_Board_Model                  => 16#1_0001#,
      Tag_Get_Board_Revision               => 16#1_0002#,
      Tag_Get_Board_MAC_Address            => 16#1_0003#,
      Tag_Get_Board_Serial                 => 16#1_0004#,
      Tag_Get_ARM_Memory                   => 16#1_0005#,
      Tag_Get_VC_Memory                    => 16#1_0006#,
      Tag_Get_Clocks                       => 16#1_0007#,
      Tag_Get_Command_Line                 => 16#5_0001#,
      Tag_Get_DMA_Channels                 => 16#6_0001#,
      Tag_Get_Power_State                  => 16#2_0001#,
      Tag_Set_Power_State                  => 16#2_8001#,
      Tag_Get_Timing                       => 16#2_0002#,
      Tag_Get_Clock_State                  => 16#3_0001#,
      Tag_Set_Clock_State                  => 16#3_8001#,
      Tag_Get_Clock_Rate                   => 16#3_0002#,
      Tag_Set_Clock_Rate                   => 16#3_8002#,
      Tag_Get_Voltage                      => 16#3_0003#,
      Tag_Set_Voltage                      => 16#3_8003#,
      Tag_Get_Max_Clock_Rate               => 16#3_0004#,
      Tag_Get_Max_Voltage                  => 16#3_0005#,
      Tag_Get_Temperature                  => 16#3_0006#,
      Tag_Get_Min_Clock_Rate               => 16#3_0007#,
      Tag_Get_Min_Voltage                  => 16#3_0008#,
      Tag_Get_Turbo                        => 16#3_0009#,
      Tag_Set_Turbo                        => 16#3_8009#,
      Tag_Get_Max_Temperature              => 16#3_000A#,
      Tag_Get_STC                          => 16#3_000B#,
      Tag_Allocate_Memory                  => 16#3_000C#,
      Tag_Lock_Memory                      => 16#3_000D#,
      Tag_Unlock_Memory                    => 16#3_000E#,
      Tag_Release_Memory                   => 16#3_000F#,
      Tag_Execute_Code                     => 16#3_0010#,
      Tag_Execute_QPU                      => 16#3_0011#,
      Tag_Set_Enable_QPU                   => 16#3_0012#,
      Tag_Get_Dispmanx_Resource_Mem_Handle => 16#3_0014#,
      Tag_Get_EDID_Block                   => 16#3_0020#,
      Tag_Get_Customer_OTP                 => 16#3_0021#,
      Tag_Set_Customer_OTP                 => 16#3_8021#,
      Tag_Get_Domain_State                 => 16#3_0030#,
      Tag_Set_Domain_State                 => 16#3_8030#,
      Tag_Set_SDHost_Clock                 => 16#3_8042#,
      --  Dispmanx Tags
      Tag_Allocate_Buffer                  => 16#4_0001#,
      Tag_Release_Buffer                   => 16#4_8001#,
      Tag_Blank_Screen                     => 16#4_0002#,
      Tag_Get_Physical_Display_Size        => 16#4_0003#,
      Tag_Test_Physical_Display_Size       => 16#4_4003#,
      Tag_Set_Physical_Display_Size        => 16#4_8003#,
      Tag_Get_Virtual_Buffer_Size          => 16#4_0004#,
      Tag_Test_Virtual_Buffer_Size         => 16#4_4004#,
      Tag_Set_Virtual_Buffer_Size          => 16#4_8004#,
      Tag_Get_Depth                        => 16#4_0005#,
      Tag_Test_Depth                       => 16#4_4005#,
      Tag_Set_Depth                        => 16#4_8005#,
      Tag_Get_Pixel_Order                  => 16#4_0006#,
      Tag_Test_Pixel_Order                 => 16#4_4006#,
      Tag_Set_Pixel_Order                  => 16#4_8006#,
      Tag_Get_Alpha_Mode                   => 16#4_0007#,
      Tag_Test_Alpha_Mode                  => 16#4_4007#,
      Tag_Set_Alpha_Mode                   => 16#4_8007#,
      Tag_Get_Pitch                        => 16#4_0008#,
      Tag_Get_Virtual_Offset               => 16#4_0009#,
      Tag_Test_Virtual_Offset              => 16#4_4009#,
      Tag_Set_Virtual_Offset               => 16#4_8009#,
      Tag_Get_Overscan                     => 16#4_000A#,
      Tag_Test_Overscan                    => 16#4_400A#,
      Tag_Set_Overscan                     => 16#4_800A#,
      Tag_Get_Palette                      => 16#4_000B#,
      Tag_Test_Palette                     => 16#4_400B#,
      Tag_Set_Palette                      => 16#4_800B#,
      Tag_Set_VSync                        => 16#4_800E#,
      Tag_Set_Backlight                    => 16#4_800F#,
      Tag_VCHIQ_Init                       => 16#4_8010#,
      Tag_Get_Touch_Buf                    => 16#4_000F#,
      Tag_Set_Touch_Buf                    => 16#4_801F#,
      Tag_Get_GPIO_Virtual_Buffer          => 16#4_0010#,
      Tag_Set_GPIO_Virtual_Buffer          => 16#4_8020#,
      Tag_Set_Cursor_Info                  => 16#0_8010#,
      Tag_Set_Cursor_State                 => 16#0_8011#);

end RPi.Firmware;
