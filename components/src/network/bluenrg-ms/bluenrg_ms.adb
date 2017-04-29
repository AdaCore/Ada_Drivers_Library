----------------------------------------------------------------------------
--                                                                        --
--  BlueNRG-MS                                                            --
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
with Bluetooth.HCI;  use Bluetooth.HCI;

package body BlueNRG_MS is

   Reset_Duration           : constant := 10; -- Milliseconds
   Wait_For_Wakeup_Duration : constant := 50; -- Microseconds
   Response_Delay_Duration  : constant := 10; -- Microseconds
   Maximum_Response_Retries : constant := 10;
   Maximum_Command_Retries  : constant := 10;

   --------------
   -- GAP_Init --
   --------------

   function GAP_Init
     (Device                  : in out BlueNRG_MS_Device;
      Role                    :        Device_Role;
      Enable_Privacy          :        Boolean;
      Device_Name_Length      :        UInt8;
      Status                  :    out UInt8;
      Service_Handle          :    out Handle;
      Device_Name_Char_Handle :    out Handle;
      Appearance_Char_Handle  :    out Handle)
      return Boolean
   is
      Privacy : UInt8;
      Command : constant OpCode := ACI_GAP_Init;
   begin
      if Enable_Privacy then
         Privacy := 1;
      else
         Privacy := 0;
      end if;
      declare
         Parameters : constant
                      UInt8_Array := UInt8_Array'(Role'Enum_Rep &
                                                Privacy &
                                                Device_Name_Length);
         Response   : constant
                      UInt8_Array := Send_Command
                                      (Device,
                                       Command,
                                       Parameters);
      begin
         if Response'Length = 13 then
            Status                  := Response (7);
            Service_Handle          := Response (8 .. 9);
            Device_Name_Char_Handle := Response (10 .. 11);
            Appearance_Char_Handle  := Response (12 .. 13);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GAP_Init;

   ------------------------------
   -- GAP_Set_Auth_Requirement --
   ------------------------------

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
   is
      Command       : constant OpCode := ACI_GAP_Set_Auth_Requirement;
      MITM_Mode     : UInt8 := 0;
      OOB_Enable    : UInt8 := 0;
      Bonding_Mode  : UInt8 := 0;
      Fixed_Pin     : UInt8 := 0;
      Pin_Parameter : UInt8_Array (1 .. 4);
   begin

      if MITM_Required then
         MITM_Mode := 1;
      end if;

      if OOB_Enabled then
         OOB_Enable := 1;
      end if;

      if Use_Fixed_Pin = False then
         Fixed_Pin := 1;
      end if;

      if Bonding_Required then
         Bonding_Mode := 1;
      end if;

      Pin_Parameter (1) := UInt8 (Pin and 16#FF#);
      Pin_Parameter (2) := UInt8 (Shift_Right (Pin,  8) and 16#FF#);
      Pin_Parameter (3) := UInt8 (Shift_Right (Pin, 16) and 16#FF#);
      Pin_Parameter (4) := UInt8 (Shift_Right (Pin, 24) and 16#FF#);

      declare
         Parameters : constant
                      UInt8_Array := UInt8_Array'(MITM_Mode &
                                                OOB_Enable &
                                                OOB_Data &
                                                Min_Encryption_Key_Size &
                                                Max_Encryption_Key_Size &
                                                Fixed_Pin &
                                                Pin_Parameter &
                                                Bonding_Mode);
         Response   : constant
                      UInt8_Array := Send_Command
                                      (Device,
                                       Command,
                                       Parameters);
      begin
         if Response'Length = 7 then
            Status := Response (7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GAP_Set_Auth_Requirement;

   --------------------------
   -- GAP_Set_Discoverable --
   --------------------------

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
   is
      Advertising_Event_UInt8 : constant UInt8 := Advertising_Event'Enum_Rep;
      Name_Length            : UInt8;
      Command                : constant OpCode := ACI_GAP_Set_Discoverable;
   begin

      if Local_Name'Length = 0 then
         Name_Length := 0;
      else
         -- Add one byte for the AD type --
         Name_Length := Local_Name'Length + 1;
      end if;

      declare
         Parameters : constant
                      UInt8_Array := UInt8_Array'(Advertising_Event_UInt8 &
                                                To_UInt8_Array (Advertising_Interval_Min) &
                                                To_UInt8_Array (Advertising_Interval_Max) &
                                                BT_Address_Type'Enum_Rep &
                                                Filter_Policy'Enum_Rep &
                                                Name_Length &
                                                Local_Name_Type'Enum_Rep &
                                                To_UInt8_Array (Local_Name) &
                                                UInt8 (Service_UUID_List'Length) &
                                                Service_UUID_List &
                                                To_UInt8_Array (Slave_Conn_Interval_Min) &
                                                To_UInt8_Array (Slave_Conn_Interval_Max));
         Response   : constant
                      UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 7 then
            Status := Response (7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GAP_Set_Discoverable;

   --------------------------------
   -- GAP_Slave_Security_Request --
   --------------------------------

   function GAP_Slave_Security_Request
     (Device           : in out BlueNRG_MS_Device;
      Conn_Handle      :        UInt8_Array;
      Bonding_Required :        Boolean;
      MITM_Required    :        Boolean)
      return Boolean
   is
      Bonding : UInt8            := 0;
      MITM    : UInt8            := 0;
      Command : constant OpCode := ACI_GAP_Slave_Security_request;
   begin

      if Bonding_Required then
         Bonding := 1;
      end if;

      if MITM_Required then
         MITM := 1;
      end if;

      declare
         Parameters : constant
                      UInt8_Array := UInt8_Array'(Conn_Handle &
                                                Bonding &
                                                MITM);
         Response   : constant
                      UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 4 then
            if Response (4) = 0 then
               return True;
            end if;
            return False;
         else
            return False;
         end if;
      end;
   end GAP_Slave_Security_Request;

   -----------------------------
   -- GATT_Add_Characteristic --
   -----------------------------

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
      return Boolean
   is
      Is_Variable : UInt8;
      Command     : constant OpCode := ACI_GATT_Add_Char;
   begin

      if Value_Is_Fixed_Length then
         Is_Variable := 0;
      else
         Is_Variable := 1;
      end if;

      declare
         Parameters : constant UInt8_Array :=
                                  UInt8_Array'(Service &
                                              16#01# &
                                              UInt8_Array (UUID) &
                                              Max_Value_Length &
                                              Properties &
                                              Security &
                                              Event_Mask &
                                              Encryption_Key_Size &
                                              Is_Variable);
         Response   : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 9 then
            Status      := Response (7);
            Char_Handle := Response (8 .. 9);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GATT_Add_Characteristic;

   -----------------------------
   -- GATT_Add_Characteristic --
   -----------------------------

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
      return Boolean
   is
      Is_Variable : UInt8;
      Command     : constant OpCode := ACI_GATT_Add_Char;
   begin

      if Value_Is_Fixed_Length then
         Is_Variable := 0;
      else
         Is_Variable := 1;
      end if;

      declare
         Parameters : constant UInt8_Array :=
                                 UInt8_Array'(Service &
                                             16#02# &
                                             UInt8_Array (UUID) &
                                             Max_Value_Length &
                                             Properties &
                                             Security &
                                             Event_Mask &
                                             Encryption_Key_Size &
                                             Is_Variable);
         Response   : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 9 then
            Status      := Response (7);
            Char_Handle := Response (8 .. 9);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GATT_Add_Characteristic;

   ----------------------
   -- GATT_Add_Service --
   ----------------------

   function GATT_Add_Service
      (Device                : in out BlueNRG_MS_Device;
       UUID                  :        UUID_16;
       Service               :        Service_Type;
       Max_Attribute_Records :        UInt8;
       Status                :    out UInt8;
       Service_Handle        :    out Handle)
       return Boolean
   is
      Command : constant OpCode := ACI_GATT_Add_Serv;
   begin
      declare
         Parameters : constant UInt8_Array :=
                                 UInt8_Array'(16#01# &
                                             UInt8_Array (UUID) &
                                             Service'Enum_Rep &
                                             Max_Attribute_Records);
         Response   : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 9 then
            Status         := Response (7);
            Service_Handle := Response (8 .. 9);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GATT_Add_Service;

   ----------------------
   -- GATT_Add_Service --
   ----------------------

   function GATT_Add_Service
      (Device                : in out BlueNRG_MS_Device;
       UUID                  :        UUID_128;
       Service               :        Service_Type;
       Max_Attribute_Records :        UInt8;
       Status                :    out UInt8;
       Service_Handle        :    out Handle)
       return Boolean
   is
      Command : constant OpCode := ACI_GATT_Add_Serv;
   begin
      declare
         Parameters : constant UInt8_Array :=
                                 UInt8_Array'(16#02# &
                                             UInt8_Array (UUID) &
                                             Service'Enum_Rep &
                                             Max_Attribute_Records);
         Response   : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 9 then
            Status         := Response (7);
            Service_Handle := Response (8 .. 9);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GATT_Add_Service;

   ---------------
   -- GATT_Init --
   ---------------

   function GATT_Init
     (Device : in out BlueNRG_MS_Device;
      Status :    out UInt8)
      return Boolean
   is
      Command : constant OpCode := ACI_GATT_Init;
   begin
      declare
         Response : constant UInt8_Array := Send_Command (Device, Command);
      begin
         if Response'Length = 7 then
            Status := Response (7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   exception
      when Constraint_Error =>
         return False;
   end GATT_Init;

   -------------------------------------
   -- GATT_Update_Characteristic_Value --
   -------------------------------------

   function GATT_Update_Characteristic_Value
     (Device            : in out BlueNRG_MS_Device;
      Service_Handle    :        Handle;
      Char_Handle       :        Handle;
      Offset            :        UInt8;
      Char_Value        :        UInt8_Array;
      Status            :    out UInt8)
      return Boolean
   is
      Command : constant OpCode := ACI_GATT_Update_Char_Value;
   begin
      declare
         Parameters : constant UInt8_Array :=
                                 UInt8_Array'(Service_Handle &
                                             Char_Handle &
                                             Offset &
                                             UInt8 (Char_Value'Length) &
                                             Char_Value);
         Response   : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 7 then
            Status := Response (7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GATT_Update_Characteristic_Value;

   -------------------------
   -- Get_BlueNRG_Verison --
   -------------------------

   function Get_BlueNRG_Version
     (Device             : in out BlueNRG_MS_Device;
      HCI_Version        :    out UInt8;
      HCI_Revision       :    out UInt16;
      LMP_PAL_Version    :    out UInt8;
      Mfr_Name           :    out UInt16;
      LMP_PAL_Subversion :    out UInt16;
      Status             :    out UInt8)
      return Boolean
   is
      Command : constant OpCode := HCI_Read_Local_Version_Information;
   begin
      declare
         Response : constant UInt8_Array := Send_Command (Device, Command);
      begin
         if Response'Length = 15 then
            Status := Response (7);
            HCI_Version        := Response (8);
            HCI_Revision       := UInt16 (Response (9) +
                                               Response (10) * 16#FF#);
            LMP_PAL_Version    := Response (11);
            Mfr_Name           := UInt16 (Response (12) +
                                               Response (13) * 16#FF#);
            LMP_PAL_Subversion := UInt16 (Response (14) +
                                               Response (15) * 16#FF#);
            return True;
         else
            HCI_Version        := 0;
            HCI_Revision       := 0;
            LMP_PAL_Version    := 0;
            Mfr_Name           := 0;
            LMP_PAL_Subversion := 0;
            Status             := 0;
            return False;
         end if;
      end;
   end Get_BlueNRG_Version;

   ----------------------------
   -- HAL_Set_TX_Power_Level --
   ----------------------------

   function HAL_Set_TX_Power_Level
     (Device            : in out BlueNRG_MS_Device;
      Enable_High_Power :        Boolean;
      Level             :        Power_Level;
      Status            :    out UInt8)
      return Boolean
   is
      High_Power_UInt8  : UInt8;
      Power_Level_UInt8 : constant UInt8 := UInt8 (Level);
      Command          : constant OpCode := ACI_HAL_Set_Tx_Power_Level;
   begin
      if Enable_High_Power then
         High_Power_UInt8 := 1;
      else
         High_Power_UInt8 := 0;
      end if;

      declare
         Parameters : constant UInt8_Array := UInt8_Array'(High_Power_UInt8 &
                                                         Power_Level_UInt8);
         Response   : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 7 then
            Status := Response (7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end HAL_Set_TX_Power_Level;

   ---------------------------
   -- HAL_Write_Config_Data --
   ---------------------------

   function HAL_Write_Config_Data
     (Device : in out BlueNRG_MS_Device;
      Offset :        UInt8;
      Value  :        UInt8_Array;
      Status :    out UInt8)
      return Boolean
   is
      Parameters : UInt8_Array (1 .. Value'Length + 2);
      Command    : constant OpCode := ACI_HAL_Write_Config_Data;
   begin
      Parameters (1)                    := Offset;
      Parameters (2)                    := Value'Length;
      Parameters (3 .. Parameters'Last) := Value;

      declare
         Response : constant UInt8_Array := Send_Command (Device, Command, Parameters);
      begin
         if Response'Length = 7 then
            Status := Response (7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end HAL_Write_Config_Data;

   ----------
   -- Read --
   ----------

   function Read
     (Device : in out BlueNRG_MS_Device)
      return UInt8_Array
   is
      Operation        : constant SPI_Data_8b (1 .. 1) := (1 => Read_Operation);
      Empty            : constant SPI_Data_8b (1 .. 4) := (others => 16#00#);
      No_Data          : UInt8_Array (1 .. 0);
      Read_Buffer_Size : UInt8;
      Ready            : SPI_Data_8b (1 .. 1);
      SPI_Response     : SPI_Data_8b (1 .. 4);
      Transfer_Status  : SPI_Status;
   begin
      Device.Chip_Select_Pin.all.Clear;
      Device.SPI_Port.all.Transfer (Operation, Ready, Transfer_Status);

      if Ready (1) /= Device_Ready then
         Device.Chip_Select_Pin.all.Set;
         return No_Data;
      end if;

      Device.SPI_Port.all.Transfer (Empty, SPI_Response, Transfer_Status);
      Read_Buffer_Size := SPI_Response (3);

      if Read_Buffer_Size = 0 then
         Device.Chip_Select_Pin.all.Set;
         return No_Data;
      end if;

      declare
         Data  : UInt8_Array  (1 .. Integer (Read_Buffer_Size));
         Zeros : constant SPI_Data_8b (1 .. Integer (Read_Buffer_Size)) := (others => 16#00#);
      begin
         Device.SPI_Port.all.Transfer (Zeros, SPI_Data_8b (Data), Transfer_Status);
         Device.Chip_Select_Pin.all.Set;
         return Data;
      end;
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Device : in out BlueNRG_MS_Device;
      Count  : in out Natural)
      return UInt8_Array
   is
      Operation        : constant SPI_Data_8b (1 .. 1) := (1 => Read_Operation);
      Empty            : constant SPI_Data_8b (1 .. 4) := (others => 16#00#);
      No_Data          : UInt8_Array (1 .. 0);
      Read_Buffer_Size : UInt8;
      Ready            : SPI_Data_8b (1 .. 1);
      SPI_Response     : SPI_Data_8b (1 .. 4);
      Transfer_Status  : SPI_Status;
   begin
      Device.Chip_Select_Pin.all.Clear;
      Device.SPI_Port.all.Transfer (Operation, Ready, Transfer_Status);

      if Ready (1) /= Device_Ready then
         Device.Chip_Select_Pin.all.Set;
         return No_Data;
      end if;

      Device.SPI_Port.all.Transfer (Empty, SPI_Response, Transfer_Status);
      Read_Buffer_Size := SPI_Response (3);

      if Read_Buffer_Size = 0 then
         Device.Chip_Select_Pin.all.Set;
         return No_Data;
      end if;

      if Count > Natural (Read_Buffer_Size) then
         Count := Natural (Read_Buffer_Size);
      end if;

      declare
         Data  : UInt8_Array  (1 .. Integer (Count));
         Zeros : constant SPI_Data_8b (1 .. Integer (Count)) := (others => 16#00#);
      begin
         Device.SPI_Port.all.Transfer (Zeros, SPI_Data_8b (Data), Transfer_Status);
         Device.Chip_Select_Pin.all.Set;
         return Data;
      end;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Device : in out BlueNRG_MS_Device)
   is
   begin
      Device.Reset_Pin.all.Clear;
      Device.Time.Delay_Milliseconds (Reset_Duration);
      Device.Reset_Pin.all.Set;
      Device.Time.Delay_Milliseconds (Reset_Duration);
   end Reset;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode;
      Parameters :        UInt8_Array)
      return Boolean
   is
      Try_Count : Natural := 0;
   begin
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write (Command_Packet_Type &
                                 Command &
                                 Parameters'Length &
                                 Parameters);
         if Try_Count = Maximum_Command_Retries then
            return False;
         end if;
         Device.Time.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;
      return True;
   end Send_Command;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode)
      return Boolean
   is
      Try_Count : Natural := 0;
   begin
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write (Command_Packet_Type &
                                 Command &
                                 16#00#);
         if Try_Count = Maximum_Command_Retries then
            return False;
         end if;
         Device.Time.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;
      return True;
   end Send_Command;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode;
      Parameters :        UInt8_Array)
      return UInt8_Array
   is
      Try_Count  : Natural := 0;
      Null_Array : UInt8_Array (1 .. 0);
   begin
      -- Send Command --
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write (Command_Packet_Type &
                                 Command &
                                 Parameters'Length &
                                 Parameters);
         if Try_Count = Maximum_Command_Retries then
            return Null_Array;
         end if;
         Device.Time.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;

      Try_Count := 0;

      loop
         declare
            Response : constant UInt8_Array := Device.Read;
         begin
            if Response'Length >= 7 then
               if Response (1) = Event_Packet_Type and then
                  Response (5 .. 6) = Command
               then
                  return Response;
               end if;
            end if;
         end;
         exit when Try_Count = Maximum_Response_Retries;
         Device.Time.Delay_Microseconds (Response_Delay_Duration);
         Try_Count := Try_Count + 1;
      end loop;

      return Null_Array;
   end Send_Command;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode)
      return UInt8_Array
   is
      Try_Count  : Natural := 0;
      Null_Array : UInt8_Array (1 .. 0);
   begin
      -- Send Command --
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write (Command_Packet_Type &
                                 Command &
                                 16#00#);
         if Try_Count = Maximum_Command_Retries then
            return Null_Array;
         end if;
         Device.Time.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;

      Try_Count := 0;

      loop
         declare
            Response : constant UInt8_Array := Device.Read;
         begin
            if Response'Length >= 7 then
               if Response (1) = Event_Packet_Type and then
                  Response (5 .. 6) = Command
               then
                  return Response;
               end if;
            end if;
         end;
         exit when Try_Count = Maximum_Response_Retries;
         Device.Time.Delay_Microseconds (Response_Delay_Duration);
         Try_Count := Try_Count + 1;
      end loop;

      return Null_Array;
   end Send_Command;

   ----------------------------------
   -- Set_Bluetooth_Public_Address --
   ----------------------------------

   function Set_Bluetooth_Public_Address
     (Device            : in out BlueNRG_MS_Device;
      Bluetooth_Address :        Address;
      Status            :    out UInt8)
      return Boolean
   is
   begin
      return HAL_Write_Config_Data
               (Device => Device,
                Offset => Config_Data_Public_Address_Offset,
                Value  => Bluetooth_Address,
                Status => Status);
   end Set_Bluetooth_Public_Address;

   -------------------
   -- To_UInt8_Array --
   -------------------

   function To_UInt8_Array
     (Input : UInt16)
      return UInt8_Array
   is
      No_Output : UInt8_Array (1 .. 0);
      First     : UInt8;
      Second    : UInt8;
   begin
      First  := UInt8 (Input and 16#00FF#);
      Second := UInt8 (Shift_Right (Input, 8));
      return UInt8_Array'(Second, First);
   exception
      when Constraint_Error =>
         return No_Output;
   end To_UInt8_Array;

   -------------------
   -- To_UInt8_Array --
   -------------------

   function To_UInt8_Array
     (Input : String)
      return UInt8_Array
   is
      No_Output    : UInt8_Array (1 .. 0);
      Output_Index : Natural := 1;
   begin
      declare
         Output : UInt8_Array (1 .. Input'Length);
      begin
         for Input_Index in Integer range Input'Range loop
            Output (Output_Index) := UInt8 (Character'Pos (Input (Input_Index)));
            Output_Index := Output_Index + 1;
         end loop;
         return Output;
      end;
   exception
      when Constraint_Error =>
         return No_Output;
   end To_UInt8_Array;

   -----------
   -- Write --
   -----------

   function Write
     (Device : in out BlueNRG_MS_Device;
      Data   :        UInt8_Array)
      return Boolean
   is
      Operation         : constant SPI_Data_8b (1 .. 1) := (1 => Write_Operation);
      Empty             : constant SPI_Data_8b (1 .. 4) := (others => 16#00#);
      Write_Buffer_Size : UInt8;
      Ready             : SPI_Data_8b (1 .. 1);
      SPI_Response      : SPI_Data_8b (1 .. 4);
      Transfer_Status   : SPI_Status;
   begin
      Device.Chip_Select_Pin.all.Clear;
      Device.SPI_Port.all.Transfer (Operation, Ready, Transfer_Status);

      if Ready (1) /= Device_Ready then
         Device.Chip_Select_Pin.all.Set;
         return False;
      end if;

      Device.SPI_Port.all.Transfer (Empty, SPI_Response, Transfer_Status);
      Write_Buffer_Size := SPI_Response (1);

      if Write_Buffer_Size < Data'Length then
         Device.Chip_Select_Pin.all.Set;
         return False;
      end if;

      declare
         Not_Used : SPI_Data_8b (Data'Range);
      begin
         Device.SPI_Port.all.Transfer (SPI_Data_8b (Data), Not_Used, Transfer_Status);
      end;

      Device.Chip_Select_Pin.all.Set;

      return True;
   end Write;

end BlueNRG_MS;
