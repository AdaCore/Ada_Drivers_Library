------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

--  Driver for HM-11/cc2541 chip

with HAL.UART;       use HAL.UART;
with System;

package HM11 is

   pragma Extensions_Allowed (On);

   type Receive_Handler is not null access
     procedure (Port      : HAL.UART.Any_UART_Port;
                Received  : System.Address;
                Length    : Natural;
                Status    : out UART_Status;
                Timeout   : Natural := 1000;
                As_Stream : Boolean := False);
   --  Received points to memory where incoming data will be stored.
   --  Length is the length of the memory buffer.
   --  Timeout in milliseconds.
   --  As_Stream starts reading without blocking the caller and read data
   --  into Received in a cycle

   type Last_Read_Position_Handler is access
     procedure (Closed : out Boolean;
                Zero   : out Positive);
   --  The last read byte is Zero-1. Closed = True if stream is closed
   --  due to error.

   procedure Default_Receive_Handler
     (Port      : HAL.UART.Any_UART_Port;
      Received  : System.Address;
      Length    : Natural;
      Status    : out UART_Status;
      Timeout   : Natural := 1000;
      As_Stream : Boolean := False);
   --  The default receive procedure is based on HAL.Receive that can be
   --  used with synchronous USART only. Use a custom one based on the DMA
   --  for asynchronous UART, like shown in
   --  examples/STM32F429_Discovery/hm11_f429disco.gpr

   -----------
   -- Types --
   -----------

   type Role is (Peripheral, Central);

   type Hex_Character   is new Character range '0' .. 'F';
   type Digit_Character is new Character range '0' .. '9';

   type Advertising_Interval is
     (ms_100, ms_211, ms_252, ms_318, ms_417, ms_546, ms_760, ms_852, ms_1022,
      ms_1285, ms_2000, ms_3000, ms_4000, ms_5000, ms_6000, ms_7000);

   type Advertising_Type is
     (Advertising_ScanResponse_Connectable,
      Last_Device_Connect,
      Advertising_ScanResponse,
      Advertising);

   type Advertising_FLAG is array (1 .. 2) of Hex_Character;
   --  0x00~0xFF

   type Advertising_Mode is (Normal_Advertising, Reliable_Advertising);

   type Advertising_Temperature_Type is array (1 .. 2) of Hex_Character;

   type MAC_Address_Type is
     (Normal_Address, Static_MAC, Static_Random_MAC, Random_MAC);

   type MAC_Address is array (1 .. 12) of Hex_Character;
   --    0xB4: 0x99: 0x4C: 0xXX: 0xXX: 0xXX is BLE MAC Address

   type UUID is array (1 .. 4) of Hex_Character;
   --  4 Bytes Length, HEX format string, only accept 16-bit UUID now

   type Write_Method is (Write, Write_Without_Response);

   type MAC_White_List_Index is range 1 .. 3;

   type Persent is range 0 .. 100;

   type UART_Baud_Rate is
     (BR_9600, BR_19200, BR_38400, BR_57600, BR_115200,
      BR_4800, BR_2400, BR_1200, BR_230400);

   type Link_Layer_Connection_Interval is
     (ms_7, ms_10, ms_15, ms_20, ms_25, ms_30, ms_35, ms_40, ms_45, ms_4000);

   type Layer_Connection_Latency is range 0 .. 4;

   type Connection_Supervision_Timeout is
     (ms_100, ms_1000, ms_2000, ms_3000, ms_4000, ms_5000, ms_6000);

   type Connect_Result is
     (Connecting, Connected, Connect_Error, Connect_Fail,
      No_Address, Other_Error);

   type Discovered_Index is range 0 .. 5;
   --  Index in the list of the discovered devices

   Max_Name_Length    : constant := 248;
   Max_RSII_Length    : constant := 4;
   Discovered_Lenght  : constant := 8 + --  OK+DISCS Discovery hello
     8 + MAC_Address'Length +           --  OK+DISC: + MAC address
     8 + Max_Name_Length + 2 +          --  OK+NAME: + Name + CRLF
     8 + Max_RSII_Length + 2;           --  OK+RSSI: + RSII + CRLF

   type Discovered_Callback is access procedure
     (Id   : Character;
      MAC  : MAC_Address;
      Name : String;
      RSSI : String);
   --  Called when Scan method discovered a device. Name and RSSI may be empty
   --  depends on Set_Show_Device_Information_When_Discovery setting.

   Discovered_iBeacon_Lenght : constant := 8 + -- OK+DISCS Discovery hello
     8 + -- Factory ID
     32 + -- iBeacon UUID
     10 + -- P2
     12 + -- MAC
     4; -- RSSI

   type Discovered_iBeacon_Callback is access
     procedure (Device : String);
   --  Device has the following format: P0:P1:P2:P3:P4
   --  Factory ID: P0 length is 8 Bytes;
   --  iBeacon UUID: P1 length is 32 Bytes;
   --  P2 length is 10 Bytes, include:
   --      Major Value (length 4 Bytes);
   --      Minor Value (length 4 Bytes);
   --      Measured Power (length 2 Bytes)
   --  MAC: P3 length is 12 Bytes,
   --  RSSI: P4 length is 4 Bytes,
   --  Those values all is ASCII mode.
   --  If the device not enable iBeacon function, P0, P1, P2 will
   --  use '0' to fill in.

   type iBeacon_Deploy_Mode is (Broadcast_Scanning, Only_Broadcast);

   type Service_UUID_Callback is access procedure (Service : String);
   --  Service format is [P1]:[P2]:[P3] where:
   --    P1: 4 Bytes, Services start handle.
   --    P2: 4 Bytes, Services end handle
   --    P3: 4 Bytes, Services UUID

   type Characteristic_UUID_Callback is
     access procedure (Characteristic : String);
   --  Characteristic format is [P1]:[P2]:[P3] where:
   --    P1: 4 Bytes, Characteristic handle.
   --    P2: 14 Bytes, "RD|WR|WN|NO|IN",
   --    P3: 4 Bytes, Characteristic UUID
   --  Characteristic properties length 14 Bytes, full string is
   --  "RD|WR|WN|NO|IN"; WR - Write; WN - Write_Without_Response;
   --  IN - Indicate; NO - Notify; RD - Read;
   --  Usually, a characteristic maybe only has one or two properties.
   --  Unsupported property part replaced by "--"

   type Handle_Type is array (1 .. 4) of Hex_Character;

   type Notify_Responce is (Send_Ok, Send_Er, Data_Er);
   --  Send_Ok: Send command successful.
   --  Send_Er: Send command error
   --  Data_Er: This characteristic doesn't have notify property.

   type Send_Data_Method is (Write, Write_Without_Response, Indicate, Notify);

   type Send_Data_Characteristic is (Write, Write_Without_Response);

   type Characteristic_UUID_Count is (Query, One, One_And_Next, One_And_Prev);
   --  One: Only use one Characteristic, please look at
   --       Set_Characteristic command
   --  One_And_Next: Use two Characteristics
   --       Second Char value = first Char value + 1
   --       First char value = Set_Characteristic value
   --  One_And_Prev: Use two Characteristic
   --       Second Char value = first Char value - 1
   --       First char value = Set_Characteristic value

   type RX_Gain is (No, Open);

   type Humi_Information is array (1 .. 2) of Hex_Character;
   --  0x00~0xFF

   type Work_Type is (Start_Immediately, Respond_AT_Command);

   type iBeacon_UUID is array (1 .. 32) of Hex_Character;

   type Version_Type is array (1 .. 4) of Hex_Character;

   type Measured_Power is array (1 .. 2) of Hex_Character;

   type Work_Mode is
     (Transmission_Mode, PIO_Collection_Mode, Remote_Control_Mode);

   type Notify_Mode is (Without_Address, With_Address);

   type Output_Power is (Normal_Power, Max_Power);

   type Parity_Bit is (None, Odd, Even);

   subtype PIO_Number is Positive range 1 .. 3;
   --  Note: HM-11 only have PIO0, PIO1, PIO2,PIO3.
   --  PIO0 has system KEY function, PIO1 - System LED

   type PIO_Output is (Low, High);

   type PIO_Numbers is array (1 .. 3) of Hex_Character;
   --  000 ~ 3FF; Hex format string

   type PIN_Type is array (1 .. 6) of Digit_Character;

   type Module_Power is (Dbm_Minus_23, Dbm_Minus_6, Dbm_0, Dbm_6);

   type Work_Interval is array (1 .. 2) of Digit_Character;

   type Stop_Bit is (One_Stop_Bit, Two_Stop_Bit);

   subtype Discovery_Time is Positive range 1 .. 9;

   type Sensor_Type is (None, DHT11, DS18B20);

   type Show_Device_Information is
     (Dont_Show, Show_Name, Show_RSSI, Show_RSSI_And_Name);

   subtype Temperature_Type is Natural range 0 .. 120;
   subtype Humidity_Type    is Natural range 0 .. 100;

   subtype Internal_Temperature_Type is String (1 .. 7);

   type Connect_Timeout is array (1 .. 6) of Digit_Character;

   type Bond_Mode is
     (Not_Need_PIN, Auth_Not_Need_PIN, Auth_With_PIN, Auth_And_Bonded);

   type UART_Sleep_Type is (Can_Wake_Up_Through_UART, Shutdown_UART);

   type Advertisement_Data is array (1 .. 12) of Hex_Character;

   type Characteristic_Type is array (1 .. 4) of Hex_Character;

   type PIO_Collection_Rate is range 0 .. 99;
   --  Unit: seconds

   When_Connected_Message : constant String := "OK+CONN";
   --  Message when device is connected. This message is sent via UART in
   --  Peripheral Role when some other device connected to this one.

   When_Disconnected_Message : constant String := "OK+LOST";
   --  This message is sent via UART in
   --  Peripheral Role when some other device connected to this one.

   Max_Message_Length : constant := Discovered_Lenght;

   -----------------
   -- HM11_Driver --
   -----------------

   type HM11_Driver
     (Port            : HAL.UART.Any_UART_Port;
      Receive         : Receive_Handler;
      Readed_Position : Last_Read_Position_Handler)
   is limited private;
   --  V7xx defaults: Name: HMSoft; Baud: 115200, N, 8, 1; Pin code: 000000;
   --  Peripheral Role; Remote-Control mode.

   procedure Test
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Returns Status = Ok if device responsed

   function Software_Version
     (This : in out HM11_Driver)
      return String;

   procedure Set_Role
     (This   : in out HM11_Driver;
      Value  : Role;
      Status : out UART_Status);
   --  Default: Peripheral.
   --  Warning: See Set_Work_Type to avoid losing the possibility of
   --   sending AT commands.

   procedure Get_Role
     (This   : in out HM11_Driver;
      Result : out Role;
      Status : out UART_Status);

   procedure Get_MAC_Address
     (This   : in out HM11_Driver;
      Result : out MAC_Address;
      Status : out UART_Status);

   --  In iOS system you can't get module MAC address directly. So, we put MAC
   --  address information into advert packet.
   procedure Set_Advertising_Interval
     (This   : in out HM11_Driver;
      Value  : Advertising_Interval;
      Status : out UART_Status);
   --  Added since V517 version.
   --  V522 version added max value F.
   --  The maximum 1285ms recommendations from the IOS system. That mean
   --  1285ms is apple allowed maximum value.

   procedure Get_Advertising_Interval
     (This   : in out HM11_Driver;
      Result : out Advertising_Interval;
      Status : out UART_Status);

   procedure Set_Advertising_Type
     (This   : in out HM11_Driver;
      Value  : Advertising_Type;
      Status : out UART_Status);
   --  Added since V519

   procedure Get_Advertising_Type
     (This   : in out HM11_Driver;
      Result : out Advertising_Type;
      Status : out UART_Status);

   procedure Set_White_List_Switch
     (This   : in out HM11_Driver;
      Value  : Boolean;
      Status : out UART_Status);
   --  This command added in V523.
   --  White List allows three MAC address link to module. Please use
   --  Set_White_List_MAC_Addresses command set white list MAC address.

   procedure Get_White_List_Switch
     (This   : in out HM11_Driver;
      Result : out Boolean;
      Status : out UART_Status);

   procedure Set_White_List_MAC_Addresses
     (This   : in out HM11_Driver;
      Index  : MAC_White_List_Index;
      Value  : MAC_Address;
      Status : out UART_Status);
   --  This command added in V523.
   --  White List allows three MAC address link to module.

   procedure Get_White_List_MAC_Address
     (This   : in out HM11_Driver;
      Index  : MAC_White_List_Index;
      Result : out MAC_Address;
      Status : out UART_Status);

   procedure Set_Battery_Monitor_Switch
     (This   : in out HM11_Driver;
      Value  : Boolean;
      Status : out UART_Status);
     --  This command added in V520
     --  When is set, module will add battery information into scan
     --  response data package. Default: False.

   procedure Get_Battery_Monitor_Switch
     (This   : in out HM11_Driver;
      Result : out Boolean;
      Status : out UART_Status);

   procedure Set_Battery_Information
     (This   : in out HM11_Driver;
      Value  : Persent;
      Status : out UART_Status);
   --  Should not be connected!
   --  Required: Set_Battery_Monitor_Switch (False)
   --  This is used to set battery information byte in advertising data when
   --  you closed module power monitor, you can use battery information byte
   --  in advertising package for other purposes.

   procedure Query_Battery_Information
     (This   : in out HM11_Driver;
      Result : out Persent;
      Status : out UART_Status);
   --  There has three ways to get battery information:
   --  a. Before establishing a connection.
   --  b. After established a connection, In Mode 1 or 2, remote side
   --  can send command.
   --  Battery information has included in scan response data package,
   --  one-hour updated once. When module has been discovered, you can get
   --  battery information from scan response package.
   --  Data format is:
   --   0x02, 0x16, 0x00, 0xB0, [FLAG], [temperature], [ humidity], [battery].

   procedure Set_UART_Baud_Rate
     (This   : in out HM11_Driver;
      Value  : UART_Baud_Rate;
      Status : out UART_Status);
   --  Set baud rate. Default: 9600. Note: If setup to Value BR_1200, After
   --  next power on, module will not support any AT Commands.

   procedure Get_UART_Baud_Rate
     (This   : in out HM11_Driver;
      Result : out UART_Baud_Rate;
      Status : out UART_Status);

   procedure Set_Minimum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Value  : Link_Layer_Connection_Interval;
      Status : out UART_Status);
   --  This command is added since V538
   --  Default: 20ms

   procedure Get_Minimum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Result : out Link_Layer_Connection_Interval;
      Status : out UART_Status);

   procedure Set_Maximum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Value  : Link_Layer_Connection_Interval;
      Status : out UART_Status);
   --  This command is added since V538
   --  Default: 40ms

   procedure Get_Maximum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Result : out Link_Layer_Connection_Interval;
      Status : out UART_Status);

   procedure Set_Link_Layer_Connection_Slave_Latency
     (This   : in out HM11_Driver;
      Value  : Layer_Connection_Latency;
      Status : out UART_Status);
   --  This command is added since V538
   --  Default: 0

   procedure Get_Link_Layer_Connection_Slave_Latency
     (This   : in out HM11_Driver;
      Result : out Layer_Connection_Latency;
      Status : out UART_Status);

   procedure Set_Connection_Supervision_Timeout
     (This   : in out HM11_Driver;
      Value  : Connection_Supervision_Timeout;
      Status : out UART_Status);
   --  This command is added since V538
   --  Default: 6000ms

   procedure Get_Connection_Supervision_Timeout
     (This   : in out HM11_Driver;
      Result : out Connection_Supervision_Timeout;
      Status : out UART_Status);

   procedure Set_Update_Connection
     (This   : in out HM11_Driver;
      Value  : Boolean;
      Status : out UART_Status);
   --  This command is added since V538
   --  Only Peripheral role is used.
   --  This command is only use when module is in slave role. In central role
   --  you can use Set_Minimum_Link_Layer_Connection_Interval and
   --  Set_Maximum_Link_Layer_Connection_Interval command change default
   --  connect parameters.
   --  Default: True

   procedure Get_Update_Connection
     (This   : in out HM11_Driver;
      Result : out Boolean;
      Status : out UART_Status);
   --  Only Peripheral role is used.

   procedure Clear_Last_Connected_Address
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Notice: Only Central role is used.

   procedure Sleep
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Only support Peripheral role.
   --  Enter sleep mode

   procedure Wake_Up
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Wake up from the sleep mode

   procedure Connect_Last_Device
     (This   : in out HM11_Driver;
      Result : out Connect_Result;
      Status : out UART_Status);
   --  Required: Set_Work_Type (Respond_AT_Command) and Set_Role (Central)
   --  and Set_Save_Connected_MAC (True).
   --  If remote device is not in connectable Connect_Fail result will
   --  received after about 10 seconds.

   procedure Connect
     (This     : in out HM11_Driver;
      MAC_Type : MAC_Address_Type;
      Address  : MAC_Address;
      Result   : out Connect_Result;
      Status   : out UART_Status);
   --  Required: Set_Work_Type (Respond_AT_Command) and Set_Role (Central)

   procedure Connect
     (This   : in out HM11_Driver;
      Index  : Discovered_Index;
      Result : out Connect_Result;
      Status : out UART_Status);
   --  This command is use after execute Scan
   --  This command will clear all discovered devices list.

   --  The first discovered device array index is 0, second device array
   --  index is 1, Scan command could return more than 6 devices, buy only
   --  first 6 devices could use array index, other devices must use AT+CO or
   --  AT+LN command.

   procedure Scan
     (This     : in out HM11_Driver;
      Callback : Discovered_Callback;
      Timeout  : Natural;
      Status   : out UART_Status);
   --  Required: Set_Work_Type (Respond_AT_Command), Central role

   procedure Scan_iBeacon
     (This     : in out HM11_Driver;
      Callback : Discovered_iBeacon_Callback;
      Timeout  : Natural;
      Status   : out UART_Status);
   --  Added since V539
   --  Please set Set_Work_Type (Respond_AT_Command) and Central role first.

   procedure Set_iBeacon_Deploy_Mode
     (This   : in out HM11_Driver;
      Value  : iBeacon_Deploy_Mode;
      Status : out UART_Status);
   --  After received OK, module will reset after 500ms.
   --  This command will let module into non-connectable status until
   --  next power on.

   procedure Remove_Bond_Information
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Added in V524 version

   procedure Find_All_Services_UUID
     (This     : in out HM11_Driver;
      Callback : Service_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used to
   --  find all services UUID on the slave device.

   procedure Find_All_Characteristic_UUID
     (This     : in out HM11_Driver;
      Callback : Characteristic_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used
   --  to find all Characteristic UUID on the slave device.

   procedure Find_Characteristic_UUID
     (This     : in out HM11_Driver;
      From     : Handle_Type;
      To       : Handle_Type;
      Callback : Characteristic_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used
   --  to find all Characteristic UUID on the slave device.
   --  From and To value could get from Find_All_Services_UUID Command.

   procedure Enable_Characteristic_Notify
     (This     : in out HM11_Driver;
      Handle   : Handle_Type;
      Status   : out UART_Status;
      Responce : out Notify_Responce);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used
   --  to enable notify on a characteristic who owned notify property.

   procedure Disable_Characteristic_Notify
     (This     : in out HM11_Driver;
      Handle   : Handle_Type;
      Status   : out UART_Status;
      Responce : out Notify_Responce);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used
   --  to disable notify on a characteristic who owned notify property.

   procedure Read_Characteristic_Notify
     (This     : in out HM11_Driver;
      Handle   : Handle_Type;
      Status   : out UART_Status;
      Responce : out Notify_Responce);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used to
   --  read characteristic value who owned read property.

   procedure Set_Method_And_Characteristic_Handle
     (This   : in out HM11_Driver;
      Handle : Handle_Type;
      Method : Send_Data_Method;
      Status : out UART_Status);
   --  This command added since V701
   --  Required state: after connect; Required role: Central.
   --  Note: after execute this command, now you can start to send and receive
   --  data without any AT commands.
   --  Note: This command is different between Send_Data_To_Characteristic,
   --  this command only need be executed once more.

   procedure Send_Data_To_Characteristic
     (This   : in out HM11_Driver;
      Handle : Handle_Type;
      Method : Send_Data_Characteristic;
      Data   : UART_Data_8b;
      Status : out UART_Status);
   --  This command added since V700
   --  Required state: after connect; Required role: Central.
   --  Only central role device can use that command. This command is used to
   --  send data to a characteristic who owned Write or Write-Without-Response
   --  property.
   --  P1: two bytes length, always is "WR" or "WN"
   --  P2: characteristic handle, get by Set_Characteristic or
   --   Find_All_Characteristic_UUID command
   --  P3: the data what you want to send.
   --  Note: Since V701, We added Set_Method_And_Characteristic_Handle command,
   --  you can forget this command.

   procedure Set_Use_Characteristic_UUID_Count
     (This   : in out HM11_Driver;
      Count  : Characteristic_UUID_Count;
      Status : out UART_Status);
   --  This command added since V550.
   --  Only Peripheral role is used.

   procedure Set_Advertising_FLAG
     (This   : in out HM11_Driver;
      Flag   : Advertising_FLAG;
      Status : out UART_Status);
   --  This command added in V530.
   --  This command is used to set flag information byte in advertising
   --  package for other purposes.

   procedure Set_UART_Flow_Control_Switch
     (This   : in out HM11_Driver;
      Switch : Boolean;
      Status : out UART_Status);
   --  Default: False

   procedure Get_UART_Flow_Control_Switch
     (This   : in out HM11_Driver;
      Switch : out Boolean;
      Status : out UART_Status);

   procedure Set_Module_RX_Gain
     (This   : in out HM11_Driver;
      Gain   : RX_Gain;
      Status : out UART_Status);
   --  This command is added since V535
   --  Default: No RX gain

   procedure Get_Module_RX_Gain
     (This   : in out HM11_Driver;
      Gain   : out RX_Gain;
      Status : out UART_Status);

   procedure Set_Humi_Information_Byte_In_Advertising
     (This   : in out HM11_Driver;
      Info   : Humi_Information;
      Status : out UART_Status);
   --  Added in V544
   --  This command is used to set humi-information byte in advertising data.

   procedure Set_Work_Type
     (This   : in out HM11_Driver;
      Work   : Work_Type;
      Status : out UART_Status);
   --  In Peripheral role:
   --  If Start_Immediately is setup, module will auto in advertising status.
   --  If Respond_AT_Command is setup, module will do nothing, you can config
   --  module now, until Start_Working command received, module will in
   --  advertising status.
   --  In Central role:
   --  If Start_Immediately is setup, module will discovery and try made
   --  connection automatic.
   --  If Respond_AT_Command is setup, module will do nothing, you can use
   --  Scan or Scan_iBeacon commands or other AT commands what you
   --  want to execute. Or you can execute Start_Working let module in auto
   --  work mode.

   procedure Get_Work_Type
     (This   : in out HM11_Driver;
      Work   : out Work_Type;
      Status : out UART_Status);
   --  Default: Start_Immediately

   procedure Set_iBeacon_Switch
     (This   : in out HM11_Driver;
      Switch : Boolean;
      Status : out UART_Status);
   --  This command is added since V517 version.
   --  Default: False

   procedure Get_iBeacon_Switch
     (This   : in out HM11_Driver;
      Switch : out Boolean;
      Status : out UART_Status);

   --  Default iBeacon UUID is: 74278BDA-B644-4520-8F0C-720EAF059935

   procedure Set_iBeacon_UUID
     (This   : in out HM11_Driver;
      UUID   : iBeacon_UUID;
      Status : out UART_Status);
   --  This command is added since V520 version.
   --  Default: False

   procedure Get_iBeacon_UUID
     (This   : in out HM11_Driver;
      UUID   : out iBeacon_UUID;
      Status : out UART_Status);

   procedure Set_iBeacon_Marjor_Version
     (This    : in out HM11_Driver;
      Version : Version_Type;
      Status  : out UART_Status);
   --  This command is added since V517 version.
   --  Default: 0xFFE0

   procedure Get_iBeacon_Marjor_Version
     (This    : in out HM11_Driver;
      Version : out Version_Type;
      Status  : out UART_Status);

   procedure Set_iBeacon_Minor_Version
     (This    : in out HM11_Driver;
      Version : Version_Type;
      Status  : out UART_Status);
   --  This command is added since V517 version.
   --  Default: 0xFFE1

   procedure Get_iBeacon_Minor_Version
     (This    : in out HM11_Driver;
      Version : out Version_Type;
      Status  : out UART_Status);

   procedure Set_iBeacon_Measured_Power
     (This   : in out HM11_Driver;
      Power  : Measured_Power;
      Status : out UART_Status);
   --  This command is added since V519 version.
   --  Default: 0xC5

   procedure Get_iBeacon_Measured_Power
     (This   : in out HM11_Driver;
      Power  : out Measured_Power;
      Status : out UART_Status);

   procedure Set_Work_Mode
     (This   : in out HM11_Driver;
      Mode   : Work_Mode;
      Status : out UART_Status);
   --  Default: Transmission_Mode
   --  Mode Transmission_Mode:
   --    Before establishing a connection, you can use the AT command
   --    configuration module through UART.
   --    After established a connection, you can send data to remote side
   --    from each other.
   --  Mode PIO_Collection_Mode:
   --    Before establishing a connection, you can use the AT command
   --    configuration module through UART.
   --    After established a connection, you can send data to remote side.
   --    Remote side can do fellows:
   --      Send AT command configuration module.
   --      Collect PIO04 to the PIO11 pins input state of HM-10.
   --      Collect PIO03 pins input state of HM-11.
   --      Remote control PIO2, PIO3 pins output state of HM-10.
   --      Remote control PIO2 pin output state of HM-11.
   --      Send data to module UART port (not include any AT command and per
   --      package must less than 20 bytes).
   --  Mode Remote_Control_Mode:
   --    Before establishing a connection, you can use the AT command
   --    configuration module through UART.
   --    After established a connection, you can send data to remote side.
   --    Remote side can do fellows:
   --      Send AT command configuration module.
   --      Remote control PIO2 to PIO11 pins output state of HM-10.
   --      Remote control PIO2, PIO3 pins output state of HM-11.
   --      Send data to module UART port (not include any AT command and per
   --      package must less than 20 bytes).

   procedure Get_Work_Mode
     (This   : in out HM11_Driver;
      Mode   : out Work_Mode;
      Status : out UART_Status);

   procedure Set_Notify_Information
     (This   : in out HM11_Driver;
      Notify : Boolean;
      Status : out UART_Status);
   --  Default: True
   --  If this value is set to True, when link ESTABLISHED or LOSTED module
   --  will send OK+CONN or OK+LOST string through UART.

   procedure Get_Notify_Information
     (This   : in out HM11_Driver;
      Notify : out Boolean;
      Status : out UART_Status);

   procedure Set_Notify_Mode
     (This   : in out HM11_Driver;
      Mode   : Notify_Mode;
      Status : out UART_Status);
   --  Added since V534
   --  Default: Without_Address
   --  This command must work with Set_Notify_Information (True), if this
   --  switch is open, when the module connect or disconnect, the prompt
   --  string will include the remote address.
   --  OK+CONN:001122334455 String "001122334455" is the MAC address string

   procedure Get_Notify_Mode
     (This   : in out HM11_Driver;
      Mode   : out Notify_Mode;
      Status : out UART_Status);

   procedure Set_Module_Name
     (This   : in out HM11_Driver;
      Name   : String;
      Status : out UART_Status)
     with Pre => (Name'Length in 1 .. 12);
   --  Default: HMSoft

   function Get_Module_Name
     (This : in out HM11_Driver)
      return String;

   procedure Set_Output_Power
     (This   : in out HM11_Driver;
      Power  : Output_Power;
      Status : out UART_Status);
   --  Added in V527
   --  Default: Max_Power

   procedure Get_Output_Power
     (This   : in out HM11_Driver;
      Power  : out Output_Power;
      Status : out UART_Status);

   procedure Set_Parity_Bit
     (This   : in out HM11_Driver;
      Parity : Parity_Bit;
      Status : out UART_Status);
   --  Default: None

   procedure Get_Parity_Bit
     (This   : in out HM11_Driver;
      Parity : out Parity_Bit;
      Status : out UART_Status);

   procedure Set_PIO_Output_Status
     (This   : in out HM11_Driver;
      PIO    : PIO_Number;
      Output : PIO_Output;
      Status : out UART_Status);
   --  Required Work_Mode: Transmission_Mode or Remote_Control_Mode.

   procedure Get_PIO_Output_Status
     (This   : in out HM11_Driver;
      PIO    : PIO_Number;
      Output : out PIO_Output;
      Status : out UART_Status);

   procedure Get_PIOs_Output_Status
     (This   : in out HM11_Driver;
      PIOs   : out PIO_Numbers;
      Status : out UART_Status);

   procedure Set_PIOs_Output_Status
     (This   : in out HM11_Driver;
      PIOs   : PIO_Numbers;
      Status : out UART_Status);
   --  This command added since V551
   --  This command is used to control multiple PIO pins output HIGH or LOW.
   --  PIOs is Hex format, max value 3FF changed to binary format is
   --  001111111111, Total length is 12 bit, left side to right bit mapped
   --  to module PIO0 to PIOB,
   --  Note: HM-11 only have PIO0, PIO1, PIO2, PIO3
   --  1: is output HIGH;
   --  0: is output LOW.

   procedure Set_PIN_Code
     (This   : in out HM11_Driver;
      Pin    : PIN_Type;
      Status : out UART_Status);
   --  Default: 000000

   procedure Get_PIN_Code
     (This   : in out HM11_Driver;
      Pin    : out PIN_Type;
      Status : out UART_Status);

   procedure Set_Module_Power
     (This   : in out HM11_Driver;
      Power  : Module_Power;
      Status : out UART_Status);
   --  Default: Dbm_0

   procedure Get_Module_Power
     (This   : in out HM11_Driver;
      Power  : out Module_Power;
      Status : out UART_Status);

   procedure Set_Module_Auto_Sleep
     (This   : in out HM11_Driver;
      Sleep  : Boolean;
      Status : out UART_Status);
   --  Only Peripheral role is used.
   --  Default: False

   procedure Get_Module_Auto_Sleep
     (This   : in out HM11_Driver;
      Sleep  : out Boolean;
      Status : out UART_Status);
   --  Only Peripheral role is used.

   procedure Set_Reliable_Advertising_Mode
     (This   : in out HM11_Driver;
      Mode   : Advertising_Mode;
      Status : out UART_Status);
   --  This command is added since V530
   --  Default: Normal_Advertising
   --  This command is used to make sure module always send advertising
   --  package when module is in long time standby mode.

   procedure Get_Reliable_Advertising_Mode
     (This   : in out HM11_Driver;
      Mode   : out Advertising_Mode;
      Status : out UART_Status);

   procedure Reset
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Restore all setup value to factory setup

   procedure Restart
     (This   : in out HM11_Driver;
      Status : out UART_Status);

   procedure Get_Last_Connected_Device_Address
     (This   : in out HM11_Driver;
      MAC    : out MAC_Address;
      Status : out UART_Status);

   procedure Set_Sensor_Work_Interval
     (This     : in out HM11_Driver;
      Interval : Work_Interval;
      Status   : out UART_Status);
   --  Default: 0, Unit: minute
   --  Note: This command is only used for HMSensor

   procedure Get_Sensor_Work_Interval
     (This     : in out HM11_Driver;
      Interval : out Work_Interval;
      Status   : out UART_Status);

   procedure Set_Stop_Bit
     (This   : in out HM11_Driver;
      Value  : Stop_Bit;
      Status : out UART_Status);
   --  Default: One_Stop_Bit

   procedure Get_Stop_Bit
     (This   : in out HM11_Driver;
      Result : out Stop_Bit;
      Status : out UART_Status);

   procedure Start_Working
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  This command is only used when Work_Type = Respond_AT_Command.

   procedure Set_Save_Connected_MAC
     (This   : in out HM11_Driver;
      Save   : Boolean;
      Status : out UART_Status);
   --  Only Central role is used.
   --  Default: False
   --  Note: In central role, when power on, module will check if there is
   --  a device address in internal flash, if have, module will try to connect
   --  it. If not, module will start a scan device procedure.

   procedure Get_Save_Connected_MAC
     (This   : in out HM11_Driver;
      Save   : out Boolean;
      Status : out UART_Status);
   --  Only Central role is used.

   procedure Set_Discovery_Time
     (This   : in out HM11_Driver;
      Time   : Discovery_Time;
      Status : out UART_Status);
   --  Add in V543
   --  Only Central role is used.
   --  Default: 3 seconds

   procedure Get_Discovery_Time
     (This   : in out HM11_Driver;
      Time   : out Discovery_Time;
      Status : out UART_Status);
   --  Only Central role is used.

   procedure Set_Sensor_Type
     (This   : in out HM11_Driver;
      Value  : Sensor_Type;
      Status : out UART_Status);
   --  This command is only use for HMSensor
   --  Sensor type on module PIO11 (PIO3 on HM-11)
   --  Default: None

   procedure Get_Sensor_Type
     (This   : in out HM11_Driver;
      Result : out Sensor_Type;
      Status : out UART_Status);

   procedure Set_Show_Device_Information_When_Discovery
     (This   : in out HM11_Driver;
      Show   : Show_Device_Information;
      Status : out UART_Status);
   --  Default: Dont_Show
   --  If Show_Name is set, Scan will add the device name information
   --  into scan result package.
   --  If Show_RSSI is set, Scan will add device RSSI information into
   --  scan result package.
   --  If Show_RSSI_And_Name is set, Scan will add device name and RSSI
   --  information into scan result package.

   procedure Get_Show_Device_Information_When_Discovery
     (This   : in out HM11_Driver;
      Show   : out Show_Device_Information;
      Status : out UART_Status);

   procedure Get_Sensor_Temperature_And_Humidity
     (This        : in out HM11_Driver;
      Temperature : out Temperature_Type;
      Humidity    : out Humidity_Type;
      Status      : out UART_Status);
   --  Note: This command is only use for HMSensor version and has a sensor
   --  This value is added into scan response data package.
   --  Data format is 0x02, 0x16, 0x00, 0xB0, [reserved], [temperature],
   --  [humidity], [battery].
   --  Android:
   --    Included in OnLeScan function result array, you can see it directly.
   --  iOS:
   --    Included in LeScan function result NSDictionary struct, service id
   --    is 0xB000.

   procedure Get_Module_Temperature
     (This        : in out HM11_Driver;
      Temperature : out Internal_Temperature_Type;
      Status      : out UART_Status);
   --  HMSoft version could get internal temperature.
   --  Added in V523 version, Modified at V544.
   --  Note: HMSensor version, if not setup Set_Sensor_Type value, will get
   --  IC temperature.

   procedure Set_Temperature_Information_Byte_In_Advertising
     (This        : in out HM11_Driver;
      Temperature : Advertising_Temperature_Type;
      Status      : out UART_Status);
   --  Added in V544
   --  This command is used to set temperature information byte in
   --  advertising data.

   procedure Set_Connect_Remote_Device_Timeout
     (This    : in out HM11_Driver;
      Timeout : Connect_Timeout;
      Status  : out UART_Status);
   --  Only Central role is used.
   --  Default: 000000 ms.
   --  This command is only used in central role.
   --  In central role, when module power on, if module have a saved device
   --  MAC address, then module will not into scan procedure, module will try
   --  to connect this device. This command caused module into a scan
   --  procedure if setup value is not 000000.

   procedure Get_Connect_Remote_Device_Timeout
     (This    : in out HM11_Driver;
      Timeout : out Connect_Timeout;
      Status  : out UART_Status);
   --  Only Central role is used.

   procedure Set_Bond_Mode
     (This   : in out HM11_Driver;
      Mode   : Bond_Mode;
      Status : out UART_Status);
   --  Default: Auth_And_Bonded
   --  Important: If your module version is less than V515, please don't use
   --  this command.
   --  Android system Auth_Not_Need_PIN is same to Auth_With_PIN.
   --  Note: Value 3 is added in V524.

   procedure Get_Bond_Mode
     (This   : in out HM11_Driver;
      Mode   : out Bond_Mode;
      Status : out UART_Status);

   procedure Set_Service_UUID
     (This   : in out HM11_Driver;
      Value  : UUID;
      Status : out UART_Status);

   procedure Get_Service_UUID
     (This   : in out HM11_Driver;
      Result : out UUID;
      Status : out UART_Status);

   procedure Set_UART_Sleep_Type
     (This   : in out HM11_Driver;
      Value  : UART_Sleep_Type;
      Status : out UART_Status);
   --  Note: This command is only use for HMSensor version.

   procedure Get_UART_Sleep_Type
     (This   : in out HM11_Driver;
      Result : out UART_Sleep_Type;
      Status : out UART_Status);

   procedure Set_Module_Advertisement_Data
     (This   : in out HM11_Driver;
      Data   : Advertisement_Data;
      Status : out UART_Status);
   --  Added since V607/V702

   procedure Disconnect
     (This   : in out HM11_Driver;
      Status : out UART_Status);
   --  Should be connected.

   procedure Set_Characteristic
     (This   : in out HM11_Driver;
      Value  : Characteristic_Type;
      Status : out UART_Status);
   --  Default: FFE1

   procedure Get_Characteristic
     (This   : in out HM11_Driver;
      Result : out Characteristic_Type;
      Status : out UART_Status);

   procedure Set_PIO_Collection_Rate
     (This   : in out HM11_Driver;
      Value  : PIO_Collection_Rate;
      Status : out UART_Status);
   --  This command is added since V515 version
   --  Default: 10 seconds
   --  Required Work Mode = PIO_Collection_Mode,
   --  when PIO state is change, module will send OK+Col:[xx]
   --  to UART or remote side. This command is set send interval.

   procedure Get_PIO_Collection_Rate
     (This   : in out HM11_Driver;
      Result : out PIO_Collection_Rate;
      Status : out UART_Status);

   procedure Set_Power_Pin_Output
     (This   : in out HM11_Driver;
      Value  : PIO_Numbers;
      Status : out UART_Status);
   --  This command added in V527.
   --  Default: 000
   --  Set pins state (high/low) after power supplied
   --  Hex format 0x3FF change to binary format is 001111111111, total length
   --  is 12 bit, left to right side is mapped to module PIO0~PIOB port, PIO0
   --  and PIo1 is used by system, So must be 0. Only Pio2~PIOB pins is
   --  available.

   procedure Get_Power_Pin_Output
     (This   : in out HM11_Driver;
      Result : out PIO_Numbers;
      Status : out UART_Status);

   procedure Set_Connect_Pin_Output
     (This   : in out HM11_Driver;
      Value  : PIO_Numbers;
      Status : out UART_Status);
   --  This command added in V527.
   --  Default: 000
   --  Set pins state (high/low) after power supplied
   --  Hex format 0x3FF change to binary format is 001111111111, total length
   --  is 12 bit, Left to right side is mapped to module PIO0~PIOB port,
   --  PIO0 and PIo1 is used by system. So, must be 0, Only Pio2~PIOB pins
   --  is available.

   procedure Get_Connect_Pin_Output
     (This   : in out HM11_Driver;
      Result : out PIO_Numbers;
      Status : out UART_Status);

   --  Query PIO04~PIO11 input(output) state "AT+COL??"

private

   type Variable_String (Length : Positive) is record
      Value : String (1 .. Length);
      Last  : Natural := 0;
   end record;

   type Discovered_Info_Type is record
      Id   : Character;
      MAC  : MAC_Address;
      Name : Variable_String (Max_Name_Length);
      RSSI : Variable_String (Max_RSII_Length);
   end record;

   Null_Discovered_Info : constant Discovered_Info_Type :=
     (' ', [others => '0'], others => <>);

   type Scan_Stage_Kind is (Selection, MAC, Name, RSSI);
   --  Used when scanning to know what we parsing now.

   -----------------
   -- HM11_Driver --
   -----------------

   type HM11_Driver
     (Port            : HAL.UART.Any_UART_Port;
      Receive         : Receive_Handler;
      Readed_Position : Last_Read_Position_Handler)
   is limited record
      Responce : UART_Data_8b (1 .. Max_Message_Length * 5);

      Discovered_Info : Discovered_Info_Type := Null_Discovered_Info;
   end record;

   procedure Transmit
     (This    : in out HM11_Driver;
      Command : String;
      Status  : out UART_Status);

   procedure Transmit
     (This    : in out HM11_Driver;
      Command : String;
      Data    : UART_Data_8b;
      Status  : out UART_Status);

   procedure Check_Responce
     (Received : System.Address;
      Length   : Natural;
      Expect   : String;
      Status   : in out UART_Status);
   --  Checks that Received (1 .. Expect'Length) = Expect

   procedure Transmit_And_Check
     (This       : in out HM11_Driver;
      Command    : String;
      Expect     : String;
      Received   : System.Address;
      Length     : Natural;
      Status     : out UART_Status);
   --  Sends command and checks responce

   procedure Transmit_And_Check
     (This    : in out HM11_Driver;
      Command : String;
      Expect  : String;
      Status  : out UART_Status);
   --  The same as above but don't return the actual responce

   procedure Read_Characteristic_UUID
     (This     : in out HM11_Driver;
      Callback : Characteristic_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status);

   procedure Read_Characteristic_Notify_Responce
     (This     : in out HM11_Driver;
      Status   : in out UART_Status;
      Responce : out Notify_Responce);

   function Start_With
     (This  : HM11_Driver;
      Value : String;
      From  : Positive;
      To    : Positive)
      return Boolean;

   function Find_Zero
     (This : HM11_Driver;
      From : Positive)
      return Natural;

   function Find
     (This   : HM11_Driver;
      Value  : String;
      From   : Positive;
      To     : Positive)
      return Natural;

   function Calc_Lenght
     (This : HM11_Driver;
      From : Positive;
      Zero : Positive)
      return Natural;

   procedure Move
     (This   : HM11_Driver;
      Result : in out Positive;
      Add    : Positive;
      Zero   : Positive;
      Length : out Natural);

   procedure Append
     (This : HM11_Driver;
      Str  : in out Variable_String;
      From : Positive;
      To   : Positive);

   procedure Copy
     (This : HM11_Driver;
      To   : out UART_Data_8b;
      From : Positive);

   function To_Connect_Result (C : Character) return Connect_Result;

   function To_String (Handle : Handle_Type) return String;

   From_Advertising_Interval : constant array
     (Advertising_Interval) of Character :=
     [ms_100  => '0',
      ms_211  => '1',
      ms_252  => '2',
      ms_318  => '3',
      ms_417  => '4',
      ms_546  => '5',
      ms_760  => '6',
      ms_852  => '7',
      ms_1022 => '8',
      ms_1285 => '9',
      ms_2000 => 'A',
      ms_3000 => 'B',
      ms_4000 => 'C',
      ms_5000 => 'D',
      ms_6000 => 'E',
      ms_7000 => 'F'];

end HM11;
