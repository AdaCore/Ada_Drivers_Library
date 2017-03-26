----------------------------------------------------------------------------
--                                                                        --
--  BlueNRG-MS Specification                                              --
--                                                                        --
--  Copyright (C) 2017, John Leimon                                       --
--                                                                        --
-- Permission to use, copy, modify, and/or distribute                     --
-- this software for any purpose with or without fee                      --
-- is hereby granted, provided that the above copyright                   --
-- notice and this permission notice appear in all copies.                --
--                                                                        --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR                        --
-- DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE                  --
-- INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY                    --
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE                    --
-- FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL                    --
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS                  --
-- OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF                       --
-- CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING                 --
-- OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF                 --
-- THIS SOFTWARE.                                                         --
--                                                                        --
----------------------------------------------------------------------------
with Bluetooth;     use Bluetooth;
with HAL;           use HAL;
with HAL.GPIO;      use HAL.GPIO;
with HAL.SPI;       use HAL.SPI;
with Interfaces;    use Interfaces;

package BlueNRG_MS is

   type BlueNRG_MS_Device is tagged limited record
      SPI_Port        : not null Any_SPI_Port;
      Chip_Select_Pin : not null Any_GPIO_Point;
      Reset_Pin       : not null Any_GPIO_Point;
   end record;

   subtype Power_Level is Integer range 0 .. 7;

   Config_Data_Public_Address_Offset     : constant UInt8         := 16#00#;
   -- Bluetooth public address --

   Config_Data_Div_Offset                : constant UInt8         := 16#06#;
   -- DIV used to derive CSRK --

   Config_Data_ER_Offset                 : constant UInt8         := 16#08#;
   -- Encryption root key used to derive LTK and CSRK --

   Config_Data_IR_Offset                 : constant UInt8         := 16#18#;
   -- Identity root key used to derive LTK and CSRK --

   Config_Data_LL_Without_Host           : constant UInt8         := 16#2C#;
   -- Switch on/off Link Layer only mode. Set to 1 to disable Host. --

   No_Properties                         : constant UInt8         := 16#00#;
   Broadcast_Property                    : constant UInt8         := 16#01#;
   Read_Property                         : constant UInt8         := 16#02#;
   Write_Without_Response_Property       : constant UInt8         := 16#04#;
   Write_Property                        : constant UInt8         := 16#08#;
   Notify_Property                       : constant UInt8         := 16#10#;
   Indicate_Property                     : constant UInt8         := 16#20#;
   Authenticated_Signed_Writes_Property  : constant UInt8         := 16#40#;

   Dont_Notify_Events_Mask               : constant UInt8         := 16#00#;
   Server_Write_Attribute_Mask           : constant UInt8         := 16#01#;
   Intimate_And_Write_For_Appl_Auth_Mask : constant UInt8         := 16#02#;
   Intimate_Appl_When_Read_And_Wait_Mask : constant UInt8         := 16#04#;

   type Device_Role is
      (Peripheral,
       Broadcaster,
       Central,
       Observer);

   for Device_Role use
      (Peripheral  => 16#01#,
       Broadcaster => 16#02#,
       Central     => 16#04#,
       Observer    => 16#08#);

   ---------------------------------------------------------------------
   -- The SPI frame header of the master on the MOSI line is composed --
   -- of 1 control byte and 4 dummy bytes (0x00)                      --
   ---------------------------------------------------------------------
   SPI_Frame_Header_Size : constant := 5;

   HCI_Disconnect                                 : constant OpCode := (16#06#, 16#04#);
   HCI_Read_Remote_Version_Information            : constant OpCode := (16#1D#, 16#04#);
   HCI_Set_Event_Mask                             : constant OpCode := (16#01#, 16#0C#);
   HCI_Reset                                      : constant OpCode := (16#03#, 16#0C#);
   HCI_Read_Transmit_Power_Level                  : constant OpCode := (16#2D#, 16#0C#);
   HCI_Read_Local_Version_Information             : constant OpCode := (16#01#, 16#10#);
   HCI_Read_Local_Supported_Commands              : constant OpCode := (16#02#, 16#10#);
   HCI_Read_Local_Supported_Features              : constant OpCode := (16#03#, 16#10#);
   HCI_Read_BD_ADDR                               : constant OpCode := (16#09#, 16#10#);
   HCI_Read_RSSI                                  : constant OpCode := (16#05#, 16#14#);
   HCI_LE_Set_Event_Mask                          : constant OpCode := (16#01#, 16#20#);
   HCI_LE_Read_Buffer_Size                        : constant OpCode := (16#02#, 16#20#);
   HCI_LE_Read_Local_Supported_Feature            : constant OpCode := (16#03#, 16#20#);
   HCI_LE_Set_Random_Address                      : constant OpCode := (16#05#, 16#20#);
   HCI_LE_Set_Advertizing_Parameters              : constant OpCode := (16#06#, 16#20#);
   HCI_LE_Read_Advertizing_Channel_Tx_Power       : constant OpCode := (16#07#, 16#20#);
   HCI_LE_Set_Advertizing_Data                    : constant OpCode := (16#08#, 16#20#);
   HCI_LE_Set_Scan_Resp_Data                      : constant OpCode := (16#09#, 16#20#);
   HCI_LE_Set_Advertize_Enable                    : constant OpCode := (16#0A#, 16#20#);
   HCI_LE_Set_Scan_Parameters                     : constant OpCode := (16#0B#, 16#20#);
   HCI_LE_Set_Scan_Enable                         : constant OpCode := (16#0C#, 16#20#);
   HCI_LE_Create_Connection                       : constant OpCode := (16#0D#, 16#20#);
   HCI_LE_Create_Connection_Cancel                : constant OpCode := (16#0E#, 16#20#);
   HCI_LE_Read_White_List_Size                    : constant OpCode := (16#0F#, 16#20#);
   HCI_LE_Clear_While_List                        : constant OpCode := (16#10#, 16#20#);
   HCI_LE_Add_Device_To_While_List                : constant OpCode := (16#11#, 16#20#);
   HCI_LE_Remove_Device_From_While_List           : constant OpCode := (16#12#, 16#20#);
   HCI_LE_Connection_Update                       : constant OpCode := (16#13#, 16#20#);
   HCI_LE_Set_Host_Channel_Classification         : constant OpCode := (16#14#, 16#20#);
   HCI_LE_Read_Channel_Map                        : constant OpCode := (16#15#, 16#20#);
   HCI_LE_Read_Remote_Used_Features               : constant OpCode := (16#16#, 16#20#);
   HCI_LE_Encrypt                                 : constant OpCode := (16#17#, 16#20#);
   HCI_LE_Rand                                    : constant OpCode := (16#18#, 16#20#);
   HCI_LE_Start_Encryption                        : constant OpCode := (16#19#, 16#20#);
   HCI_LE_Long_Term_Key_Request_Reply             : constant OpCode := (16#1A#, 16#20#);
   HCI_LE_Long_Term_Key_Requested_Negative_Reply  : constant OpCode := (16#1B#, 16#20#);
   HCI_LE_Read_Supported_States                   : constant OpCode := (16#1C#, 16#20#);
   HCI_LE_Receiver_Test                           : constant OpCode := (16#1D#, 16#20#);
   HCI_LE_Transmitter_Test                        : constant OpCode := (16#1E#, 16#20#);
   HCI_LE_Test_End                                : constant OpCode := (16#1F#, 16#20#);
   ACI_HAL_Get_Fw_Build_Number                    : constant OpCode := (16#00#, 16#FC#);
   ACI_HAL_Write_Config_Data                      : constant OpCode := (16#0C#, 16#FC#);
   ACI_HAL_Read_Config_Data                       : constant OpCode := (16#0D#, 16#FC#);
   ACI_HAL_Set_Tx_Power_Level                     : constant OpCode := (16#0F#, 16#FC#);
   ACI_HAL_Device_Standby                         : constant OpCode := (16#13#, 16#FC#);
   ACI_HAL_LE_Tx_Test_Packet_Number               : constant OpCode := (16#14#, 16#FC#);
   ACI_HAL_Tone_Start                             : constant OpCode := (16#15#, 16#FC#);
   ACI_HAL_Tone_Stop                              : constant OpCode := (16#16#, 16#FC#);
   ACI_HAL_Get_Link_Status                        : constant OpCode := (16#17#, 16#FC#);
   ACI_Updater_Start                              : constant OpCode := (16#20#, 16#FC#);
   ACI_Updater_Reboot                             : constant OpCode := (16#21#, 16#FC#);
   ACI_Get_Updater_Version                        : constant OpCode := (16#22#, 16#FC#);
   ACI_Get_Updater_Buffer_Size                    : constant OpCode := (16#23#, 16#FC#);
   ACI_Erase_Blue_Flag                            : constant OpCode := (16#24#, 16#FC#);
   ACI_Reset_Blue_Flag                            : constant OpCode := (16#25#, 16#FC#);
   ACI_Updater_Erase_Sector                       : constant OpCode := (16#26#, 16#FC#);
   ACI_Updater_Program_Data_Block                 : constant OpCode := (16#27#, 16#FC#);
   ACI_Updater_Read_Data_Block                    : constant OpCode := (16#28#, 16#FC#);
   ACI_Updater_Calc_CRC                           : constant OpCode := (16#29#, 16#FC#);
   ACI_Updater_HW_Version                         : constant OpCode := (16#2A#, 16#FC#);
   ACI_GAP_Set_Non_Discoverable                   : constant OpCode := (16#81#, 16#FC#);
   ACI_GAP_Set_Limited_Discoverable               : constant OpCode := (16#82#, 16#FC#);
   ACI_GAP_Set_Discoverable                       : constant OpCode := (16#83#, 16#FC#);
   ACI_GAP_Set_Direct_Connectable                 : constant OpCode := (16#84#, 16#FC#);
   ACI_GAP_Set_IO_Capability                      : constant OpCode := (16#85#, 16#FC#);
   ACI_GAP_Set_Auth_Requirement                   : constant OpCode := (16#86#, 16#FC#);
   ACI_GAP_Set_Author_Requirement                 : constant OpCode := (16#87#, 16#FC#);
   ACI_GAP_Pass_Key_Response                      : constant OpCode := (16#88#, 16#FC#);
   ACI_GAP_Authorization_Response                 : constant OpCode := (16#89#, 16#FC#);
   ACI_GAP_Init                                   : constant OpCode := (16#8A#, 16#FC#);
   ACI_GAP_Set_Non_Connectable                    : constant OpCode := (16#8B#, 16#FC#);
   ACI_GAP_Set_Undirected_Connectable             : constant OpCode := (16#8C#, 16#FC#);
   ACI_GAP_Slave_Security_request                 : constant OpCode := (16#8D#, 16#FC#);
   ACI_GAP_Update_Adv_Data                        : constant OpCode := (16#8E#, 16#FC#);
   ACI_GAP_Delete_AD_Type                         : constant OpCode := (16#8F#, 16#FC#);
   ACI_GAP_Get_Security_Level                     : constant OpCode := (16#90#, 16#FC#);
   ACI_GAP_Set_Event_Mask                         : constant OpCode := (16#91#, 16#FC#);
   ACI_GAP_Configure_WhiteList                    : constant OpCode := (16#92#, 16#FC#);
   ACI_GAP_Terminate                              : constant OpCode := (16#93#, 16#FC#);
   ACI_GAP_Clear_Security_Database                : constant OpCode := (16#94#, 16#FC#);
   ACI_GAP_Allow_Rebond                           : constant OpCode := (16#95#, 16#FC#);
   ACI_GAP_Start_Limited_Discovery_Proc           : constant OpCode := (16#96#, 16#FC#);
   ACI_GAP_Start_General_Discovery_Proc           : constant OpCode := (16#97#, 16#FC#);
   ACI_GAP_Start_Name_Discovery_Proc              : constant OpCode := (16#98#, 16#FC#);
   ACI_GAP_Start_Auto_Conn_Establishment          : constant OpCode := (16#99#, 16#FC#);
   ACI_GAP_Start_General_Conn_Establishment       : constant OpCode := (16#9A#, 16#FC#);
   ACI_GAP_Start_Selective_Conn_Establishment     : constant OpCode := (16#9B#, 16#FC#);
   ACI_GAP_Create_Connection                      : constant OpCode := (16#9C#, 16#FC#);
   ACI_GAP_Terminate_Gap_Procedure                : constant OpCode := (16#9D#, 16#FC#);
   ACI_GAP_Start_Connection_Update                : constant OpCode := (16#9E#, 16#FC#);
   ACI_GAP_Send_Pairing_Request                   : constant OpCode := (16#9F#, 16#FC#);
   ACI_GAP_Resolve_Private_Address                : constant OpCode := (16#A0#, 16#FC#);
   ACI_GAP_Get_Bonded_Devices                     : constant OpCode := (16#A3#, 16#FC#);
   ACI_HAL_Get_Anchor_Period                      : constant OpCode := (16#F8#, 16#FC#);
   ACI_GATT_Init                                  : constant OpCode := (16#01#, 16#FD#);
   ACI_GATT_Add_Serv                              : constant OpCode := (16#02#, 16#FD#);
   ACI_GATT_Include_Service                       : constant OpCode := (16#03#, 16#FD#);
   ACI_GATT_Add_Char                              : constant OpCode := (16#04#, 16#FD#);
   ACI_GATT_Add_Char_Desc                         : constant OpCode := (16#05#, 16#FD#);
   ACI_GATT_Update_Char_Value                     : constant OpCode := (16#06#, 16#FD#);
   ACI_GATT_Del_Char                              : constant OpCode := (16#07#, 16#FD#);
   ACI_GATT_Del_Service                           : constant OpCode := (16#08#, 16#FD#);
   ACI_GATT_Del_Include_Service                   : constant OpCode := (16#09#, 16#FD#);
   ACI_GATT_Set_Event_Mask                        : constant OpCode := (16#0A#, 16#FD#);
   ACI_GATT_Exchange_configuration                : constant OpCode := (16#0B#, 16#FD#);
   ACI_ATT_Find_Information_Req                   : constant OpCode := (16#0C#, 16#FD#);
   ACI_ATT_Find_By_Type_Value_Req                 : constant OpCode := (16#0D#, 16#FD#);
   ACI_ATT_Read_By_Type_Req                       : constant OpCode := (16#0E#, 16#FD#);
   ACI_ATT_Read_By_Group_Type_Req                 : constant OpCode := (16#0F#, 16#FD#);
   ACI_ATT_Prepare_Write_Req                      : constant OpCode := (16#10#, 16#FD#);
   ACI_ATT_Execute_Write_Req                      : constant OpCode := (16#11#, 16#FD#);
   ACI_GATT_Disc_All_Prim_Services                : constant OpCode := (16#12#, 16#FD#);
   ACI_GATT_Disc_Prim_Service_By_UUID             : constant OpCode := (16#13#, 16#FD#);
   ACI_GATT_Find_Included_Services                : constant OpCode := (16#14#, 16#FD#);
   ACI_GATT_Disc_All_Charac_Of_Serv               : constant OpCode := (16#15#, 16#FD#);
   ACI_GATT_Disc_Charac_By_UUID                   : constant OpCode := (16#16#, 16#FD#);
   ACI_GATT_Disc_All_Charac_Descriptors           : constant OpCode := (16#17#, 16#FD#);
   ACI_GATT_Read_Charac_Val                       : constant OpCode := (16#18#, 16#FD#);
   ACI_GATT_Read_Charac_Using_UUID                : constant OpCode := (16#19#, 16#FD#);
   ACI_GATT_Read_Long_Charac_Val                  : constant OpCode := (16#1A#, 16#FD#);
   ACI_GATT_Read_Multiple_Charac_Val              : constant OpCode := (16#1B#, 16#FD#);
   ACI_GATT_Write_Charac_Value                    : constant OpCode := (16#1C#, 16#FD#);
   ACI_GATT_Write_Long_Charac_Val                 : constant OpCode := (16#1D#, 16#FD#);
   ACI_GATT_Write_Charac_Reliable                 : constant OpCode := (16#1E#, 16#FD#);
   ACI_GATT_Write_Long_Charac_Desc                : constant OpCode := (16#1F#, 16#FD#);
   ACI_GATT_Read_Long_Charac_Desc                 : constant OpCode := (16#20#, 16#FD#);
   ACI_GATT_Write_Charac_Descriptor               : constant OpCode := (16#21#, 16#FD#);
   ACI_GATT_Read_Charac_Desc                      : constant OpCode := (16#22#, 16#FD#);
   ACI_GATT_Write_Without_Response                : constant OpCode := (16#23#, 16#FD#);
   ACI_GATT_Signed_Write_Without_Resp             : constant OpCode := (16#24#, 16#FD#);
   ACI_GATT_Confirm_Indication                    : constant OpCode := (16#25#, 16#FD#);
   ACI_GATT_Write_Response                        : constant OpCode := (16#26#, 16#FD#);
   ACI_GATT_Allow_Read                            : constant OpCode := (16#27#, 16#FD#);
   ACI_GATT_Set_Security_Permission               : constant OpCode := (16#28#, 16#FD#);
   ACI_GATT_Set_Desc_Value                        : constant OpCode := (16#29#, 16#FD#);
   ACI_GATT_Read_Handle_Value                     : constant OpCode := (16#2A#, 16#FD#);
   ACI_GATT_Read_Handle_Value_Offset              : constant OpCode := (16#2B#, 16#FD#);
   ACI_GATT_Update_Char_Value_Ext                 : constant OpCode := (16#2C#, 16#FD#);
   ACI_L2CAP_Connection_Parameter_Update_Request  : constant OpCode := (16#81#, 16#FD#);
   ACI_L2CAP_Connection_Parameter_Update_Response : constant OpCode := (16#82#, 16#FD#);

   type LE_Event is
     (Event_LE_Conn_Complete,
      Event_LE_Advertising_Report,
      Event_LE_Conn_Update_Complete,
      Event_LE_Read_Remote_Used_Features_Complete,
      Event_LE_LTK_Request);

   for LE_Event use
     (Event_LE_Conn_Complete                      => 16#01#,
      Event_LE_Advertising_Report                 => 16#02#,
      Event_LE_Conn_Update_Complete               => 16#03#,
      Event_LE_Read_Remote_Used_Features_Complete => 16#04#,
      Event_LE_LTK_Request                        => 16#05#);

   type Vendor_Specific_Event is
     (Event_Blue_Initialized,
      Event_Blue_Lost_Events,
      Event_Fault_Data_Event,
      Event_Blue_Gap_Limited_Discoverable,
      Event_Blue_Gap_Pairing_Cmplt,
      Event_Blue_Gap_Pass_Key_Request,
      Event_Blue_Gap_Authorization_Request,
      Event_Blue_Gap_Slave_Security_Initiated,
      Event_Blue_Gap_Bond_Lost,
      Event_Blue_Gap_Procedure_Complete,
      Event_Blue_Gap_Addr_Not_Resolved,
      Event_Blue_L2CAP_Conn_Upd_Resp,
      Event_Blue_L2CAP_Procedure_Timeout,
      Event_Blue_L2CAP_Conn_Upd_Req,
      Event_Blue_Gatt_Attribute_modified,
      Event_Blue_Gatt_Procedure_Timeout,
      Event_Blue_Att_Exchange_MTU_Resp,
      Event_Blue_Att_Find_Information_Resp,
      Event_Blue_Att_Find_By_Type_Value_Resp,
      Event_Blue_Att_Read_By_Type_Resp,
      Event_Blue_Att_Read_Resp,
      Event_Blue_Att_Read_Blob_Resp,
      Event_Blue_Att_Read_Multiple_Resp,
      Event_Blue_Att_Read_By_Group_Type_Resp,
      Event_Blue_Att_Prepare_Write_Resp,
      Event_Blue_Att_Exec_Write_Resp,
      Event_Blue_Gatt_Indication,
      Event_Blue_Gatt_notification,
      Event_Blue_Gatt_Procedure_Complete,
      Event_Blue_Gatt_Error_Resp,
      Event_Blue_Gatt_Disc_Read_Charac_By_UUID_Resp,
      Event_Blue_Gatt_Write_Permit_req,
      Event_Blue_Gatt_Read_Permit_Req,
      Event_Blue_Gatt_Read_Multi_Permit_Req,
      Event_Blue_Gatt_Tx_Pool_Available,
      Event_Blue_Gatt_Server_Confirmation);

   for Vendor_Specific_Event use
     (Event_Blue_Initialized                        => 16#0001#,
      Event_Blue_Lost_Events                        => 16#0002#,
      Event_Fault_Data_Event                        => 16#0003#,
      Event_Blue_Gap_Limited_Discoverable           => 16#0400#,
      Event_Blue_Gap_Pairing_Cmplt                  => 16#0401#,
      Event_Blue_Gap_Pass_Key_Request               => 16#0402#,
      Event_Blue_Gap_Authorization_Request          => 16#0403#,
      Event_Blue_Gap_Slave_Security_Initiated       => 16#0404#,
      Event_Blue_Gap_Bond_Lost                      => 16#0405#,
      Event_Blue_Gap_Procedure_Complete             => 16#0407#,
      Event_Blue_Gap_Addr_Not_Resolved              => 16#0408#,
      Event_Blue_L2CAP_Conn_Upd_Resp                => 16#0800#,
      Event_Blue_L2CAP_Procedure_Timeout            => 16#0801#,
      Event_Blue_L2CAP_Conn_Upd_Req                 => 16#0802#,
      Event_Blue_Gatt_Attribute_modified            => 16#0C01#,
      Event_Blue_Gatt_Procedure_Timeout             => 16#0C02#,
      Event_Blue_Att_Exchange_MTU_Resp              => 16#0C03#,
      Event_Blue_Att_Find_Information_Resp          => 16#0C04#,
      Event_Blue_Att_Find_By_Type_Value_Resp        => 16#0C05#,
      Event_Blue_Att_Read_By_Type_Resp              => 16#0C06#,
      Event_Blue_Att_Read_Resp                      => 16#0C07#,
      Event_Blue_Att_Read_Blob_Resp                 => 16#0C08#,
      Event_Blue_Att_Read_Multiple_Resp             => 16#0C09#,
      Event_Blue_Att_Read_By_Group_Type_Resp        => 16#0C0A#,
      Event_Blue_Att_Prepare_Write_Resp             => 16#0C0C#,
      Event_Blue_Att_Exec_Write_Resp                => 16#0C0D#,
      Event_Blue_Gatt_Indication                    => 16#0C0E#,
      Event_Blue_Gatt_notification                  => 16#0C0F#,
      Event_Blue_Gatt_Procedure_Complete            => 16#0C10#,
      Event_Blue_Gatt_Error_Resp                    => 16#0C11#,
      Event_Blue_Gatt_Disc_Read_Charac_By_UUID_Resp => 16#0C12#,
      Event_Blue_Gatt_Write_Permit_req              => 16#0C13#,
      Event_Blue_Gatt_Read_Permit_Req               => 16#0C14#,
      Event_Blue_Gatt_Read_Multi_Permit_Req         => 16#0C15#,
      Event_Blue_Gatt_Tx_Pool_Available             => 16#0C16#,
      Event_Blue_Gatt_Server_Confirmation           => 16#0C17#);
   -- Vendor Specific Event Codes (ECODE) --

   type HCI_Sub_Event is
     (Event_LE_Conn_Complete,
      Event_LE_Advertising_Report,
      Event_LE_Conn_Update_Complete,
      Event_LE_Read_Remote_Used_Features_Complete,
      Event_LE_LTK_Request);

   for HCI_Sub_Event use
     (Event_LE_Conn_Complete                       => 16#01#,
      Event_LE_Advertising_Report                  => 16#02#,
      Event_LE_Conn_Update_Complete                => 16#03#,
      Event_LE_Read_Remote_Used_Features_Complete  => 16#04#,
      Event_LE_LTK_Request                         => 16#05#);

   type Advertising_Event_Type is
     (Connectable_Unidirected,
      Scannable_Unidirected,
      Non_Connectable_Unidirected);

   for Advertising_Event_Type use
     (Connectable_Unidirected     => 16#00#,
      Scannable_Unidirected       => 16#02#,
      Non_Connectable_Unidirected => 16#03#);

   type Filter_Policy_Type is
     (Scan_From_Any_Connect_From_Any,
      Scan_From_Whitelist_Connect_From_Any,
      Scan_From_Any_Connect_From_Whitelist,
      Scan_From_Whitelist_Connect_From_Whitelist);

   for Filter_Policy_Type use
     (Scan_From_Any_Connect_From_Any             => 16#00#,
      Scan_From_Whitelist_Connect_From_Any       => 16#01#,
      Scan_From_Any_Connect_From_Whitelist       => 16#02#,
      Scan_From_Whitelist_Connect_From_Whitelist => 16#03#);

   type Address_Type is
     (Public,
      Random);

   for Address_Type use
     (Public => 16#00#,
      Random => 16#01#);

   type Name_Type is
     (Shortened_Local_Name,
      Complete_Local_Name);

   for Name_Type use
     (Shortened_Local_Name => 16#08#,
      Complete_Local_Name  => 16#09#);

   type GATT_Add_Service_Status is
     (Success,
      Out_Of_Memory,
      Error,
      Invalid_Parameter,
      Out_Of_Handle,
      Insufficient_Resources);

   for GATT_Add_Service_Status use
     (Success                => 16#00#,
      Out_Of_Memory          => 16#1F#,
      Error                  => 16#47#,
      Invalid_Parameter      => 16#61#,
      Out_Of_Handle          => 16#62#,
      Insufficient_Resources => 16#64#);

   function GAP_Init
     (Device                  : in out BlueNRG_MS_Device;
      Role                    :        Device_Role;
      Enable_Privacy          :        Boolean;
      Device_Name_Length      :        UInt8;
      Status                  :    out UInt8;
      Service_Handle          :    out Handle;
      Device_Name_Char_Handle :    out Handle;
      Appearance_Char_Handle  :    out Handle)
      return Boolean;
   --  Register the GAP service with the GATT. The device name characteristic and appearance
   --  characteristic are added by default and the handles of these characteristics are returned in
   --  the event data. The role parameter can be a bitwise OR of any of the values mentioned
   --  below.
   --  Returns: True when command is sent successfully

   function GAP_Set_Auth_Requirement
     (Device                  : in out BlueNRG_MS_Device;
      MITM_Required           :        Boolean;
      OOB_Enabled             :        Boolean;
      OOB_Data                :        UInt8_Array;
      Min_Encryption_Key_Size :        UInt8;
      Max_Encryption_Key_Size :        UInt8;
      Use_Fixed_Pin           :        Boolean;
      Pin                     :        Unsigned_32;
      Bonding_Required        :        Boolean;
      Status                  :    out UInt8)
      return Boolean
      with Pre => OOB_Data'Length = 16 and
                  Pin <= 999999 and
                  Pin >= 0;
   --  Set the authentication requirements for the device. If the OOB_Enable is set to 0, the
   --  following 16 octets of OOB_Data will be ignored on reception. This command has to be
   --  given only when the device is not in a connected state.
   --  Returns: True when command is sent successfully

   function GAP_Set_Discoverable
     (Device                   : in out BlueNRG_MS_Device;
      Advertising_Event        :        Advertising_Event_Type;
      Advertising_Interval_Min :        UInt16;
      Advertising_Interval_Max :        UInt16;
      BT_Address_Type          :        Address_Type;
      Filter_Policy            :        Filter_Policy_Type;
      Local_Name_Type          :        Name_Type;
      Local_Name               :        String;
      Service_UUID_List        :        UInt8_Array;
      Slave_Conn_Interval_Min  :        UInt16;
      Slave_Conn_Interval_Max  :        UInt16;
      Status                   :    out UInt8)
      return Boolean
      with Pre => Service_UUID_List'Length < 256 and
                  Local_Name'Length < 256;
   --  Set the device in general discoverable mode (as defined in GAP specification volume 3,
   --  section 9.2.4). The device will be discoverable until the host issues the
   --  Aci_Gap_Set_Non_Discoverable command. The Adv_Interval_Min and Adv_Interval_Max
   --  parameters are optional. If both are set to 0, the GAP uses the default values for adv
   --  intervals for general discoverable mode.
   --  Returns: True when command is sent successfully

   function GAP_Slave_Security_Request
     (Device           : in out BlueNRG_MS_Device;
      Conn_Handle      :        UInt8_Array;
      Bonding_Required :        Boolean;
      MITM_Required    :        Boolean)
      return Boolean
      with Pre => Conn_Handle'Length = 2;
   --  This command has to be issued to notify the master of the security requirements of the
   --  slave.
   --  Returns: True when command is sent successfully

   function GATT_Add_Characteristic
     (Device                : in out BlueNRG_MS_Device;
      Service               :        Handle;
      UUID                  :        UUID_16;
      Max_Value_Length      :        UInt8;
      Properties            :        UInt8;
      Security              :        UInt8;
      Event_Mask            :        UInt8;
      Encryption_Key_Size   :        UInt8;
      Value_Is_Fixed_Length :        Boolean;
      Status                :    out UInt8;
      Char_Handle           :    out Handle)
      return Boolean;
   --  Add a characteristic to a service
   --  Returns: True when command is sent successfully

   function GATT_Add_Characteristic
     (Device                : in out BlueNRG_MS_Device;
      Service               :        Handle;
      UUID                  :        UUID_128;
      Max_Value_Length      :        UInt8;
      Properties            :        UInt8;
      Security              :        UInt8;
      Event_Mask            :        UInt8;
      Encryption_Key_Size   :        UInt8;
      Value_Is_Fixed_Length :        Boolean;
      Status                :    out UInt8;
      Char_Handle           :    out Handle)
      return Boolean;
   --  Add a characteristic to a service
   --  Returns: True when command is sent successfully

   function GATT_Add_Service
      (Device                : in out BlueNRG_MS_Device;
       UUID                  :        UUID_16;
       Service               :        Service_Type;
       Max_Attribute_Records :        UInt8;
       Status                :    out UInt8;
       Service_Handle        :    out Handle)
       return Boolean;
   --  Add a service to GATT Server. When a service is created in the server, the host needs to
   --  reserve the handle ranges for this service using Max_Attribute_Records parameter. This
   --  parameter specifies the maximum number of attribute records that can be added to this
   --  service (including the service attribute, include attribute, characteristic attribute,
   --  characteristic value attribute and characteristic descriptor attribute). Handle of the created
   --  service is returned in command complete event.
   --  Returns: True when command is sent successfully

   function GATT_Add_Service
      (Device                : in out BlueNRG_MS_Device;
       UUID                  :        UUID_128;
       Service               :        Service_Type;
       Max_Attribute_Records :        UInt8;
       Status                :    out UInt8;
       Service_Handle        :    out Handle)
       return Boolean;
   --  Add a service to GATT Server. When a service is created in the server, the host needs to
   --  reserve the handle ranges for this service using Max_Attribute_Records parameter. This
   --  parameter specifies the maximum number of attribute records that can be added to this
   --  service (including the service attribute, include attribute, characteristic attribute,
   --  characteristic value attribute and characteristic descriptor attribute). Handle of the created
   --  service is returned in command complete event.
   --  Returns: True when command is sent successfully

   function GATT_Init
     (Device : in out BlueNRG_MS_Device;
      Status :    out UInt8)
      return Boolean;
   --  Initialize the GATT server on the slave device. Initialize all the pools and active nodes. Also
   --  it adds GATT service with service changed characteristic. Until this command is issued the
   --  GATT channel will not process any commands even if the connection is opened. This
   --  command has to be given before using any of the GAP features.
   --  Returns: True when command is sent successfully

   function GATT_Update_Characteristic_Value
     (Device            : in out BlueNRG_MS_Device;
      Service_Handle    :        Handle;
      Char_Handle       :        Handle;
      Offset            :        UInt8;
      Char_Value        :        UInt8_Array;
      Status            :    out UInt8)
      return Boolean
      with Pre => Char_Value'Length < 256;
   --  Update a characteristic value in a service
   --  Returns: True when command is sent successfully

   function Get_BlueNRG_Version
     (Device             : in out BlueNRG_MS_Device;
      HCI_Version        :    out UInt8;
      HCI_Revision       :    out UInt16;
      LMP_PAL_Version    :    out UInt8;
      Mfr_Name           :    out UInt16;
      LMP_PAL_Subversion :    out UInt16;
      Status             :    out UInt8)
      return Boolean;
   --  Returns true when the command is sent successfully

   function HAL_Set_TX_Power_Level
     (Device            : in out BlueNRG_MS_Device;
      Enable_High_Power :        Boolean;
      Level             :        Power_Level;
      Status            :    out UInt8)
      return Boolean;
   --  This command sets the TX power level of the BlueNRG-MS. By controlling the
   --  EN_HIGH_POWER and the PA_LEVEL, the combination of the 2 determines the output
   --  power level (dBm). See the table below.
   --  When the system starts up or reboots, the default TX power level will be used, which is the
   --  maximum value of 8 dBm. Once this command is given, the output power will be changed
   --  instantly, regardless if there is Bluetooth communication going on or not. For example, for
   --  debugging purpose, the BlueNRG-MS can be set to advertise all the time. And use this
   --  command to observe the signal strength changing.
   --  Returns: True when command is sent successfully

   --  EN_HIGH_POWER | PA_LEVEL TX | Power Level (dBm)
   --         0      |    0        |   -18
   --         0      |    1        |   -14.7
   --         0      |    2        |   -11.4
   --         0      |    3        |   -8.1
   --         0      |    4        |   -4.9
   --         0      |    5        |   -1.6
   --         0      |    6        |    1.7
   --         0      |    7        |    5.0
   --         1      |    0        |   -15
   --         1      |    1        |   -11.7
   --         1      |    2        |   -8.4
   --         1      |    3        |   -5.1
   --         1      |    4        |   -2.1
   --         1      |    5        |    1.4
   --         1      |    6        |    4.7
   --         1      |    7        |    8.0 (default)

   function HAL_Write_Config_Data
     (Device : in out BlueNRG_MS_Device;
      Offset :        UInt8;
      Value  :        UInt8_Array;
      Status :    out UInt8)
      return Boolean;
   --  This command writes a value to a low level configure data structure.
   --  It is useful to setup directly some low level parameters for the
   --  system in the runtime.
   --  Returns true when the command is sent successfully

   procedure Reset
     (Device : in out BlueNRG_MS_Device);
   --  Activates reset line of device to perform a hard reset

   function Set_Bluetooth_Public_Address
     (Device            : in out BlueNRG_MS_Device;
      Bluetooth_Address :        Address;
      Status            :    out UInt8)
      return Boolean;
   --  Writes to the Bluetooth Public Address segment in the
   --  device config data.
   --  Returns true when command is sent successfully

private

   function Read
     (Device : in out BlueNRG_MS_Device)
      return UInt8_Array;
   --  Reads all bytes that are available in the read buffer

   function Read
      (Device : in out BlueNRG_MS_Device;
       Count  : in out Natural)
       return UInt8_Array;
   --  Reads a maximum of Count bytes from the read buffer

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode;
      Parameters :        UInt8_Array)
      return Boolean
      with Pre => Parameters'Length < 256;
   --  Returns true when command is sent successfully

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode)
      return Boolean;
   --  Returns true when command is sent successfully

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode;
      Parameters :        UInt8_Array)
      return UInt8_Array
      with Pre => Parameters'Length < 256;
   --  Returns the HID event response from the BlueNRG
   --  or an empty array if a timeout condition occured.

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode)
      return UInt8_Array;
   --  Returns the HID event response from the BlueNRG
   --  or an empty array if a timeout condition occured.

   function To_UInt8_Array
     (Input : UInt16)
      return UInt8_Array;
   --  Convert a UInt16 to Bluetooth byte order

   function To_UInt8_Array
     (Input : String)
      return UInt8_Array;

   function Write
     (Device : in out BlueNRG_MS_Device;
      Data   :        UInt8_Array)
      return Boolean;
   --  Returns true if data was successfully written

end BlueNRG_MS;
