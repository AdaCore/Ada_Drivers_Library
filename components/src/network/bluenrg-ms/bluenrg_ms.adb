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
with HAL.Time;
with Ravenscar_Time;

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
      Device_Name_Length      :        Byte;
      Status                  :    out Byte;
      Service_Handle          :    out Handle;
      Device_Name_Char_Handle :    out Handle;
      Appearance_Char_Handle  :    out Handle)
      return Boolean
   is
      Privacy : Byte;
      Command : constant OpCode := ACI_GAP_Init;
   begin
      if Enable_Privacy then
         Privacy := 1;
      else
         Privacy := 0;
      end if;
      declare
         Parameters : constant
                      Byte_Array := Byte_Array'(Role'Enum_Rep &
                                                Privacy &
                                                Device_Name_Length);
         Response   : constant
                      Byte_Array := Send_Command
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
      OOB_Data                :        Byte_Array;
      Min_Encryption_Key_Size :        Byte;
      Max_Encryption_Key_Size :        Byte;
      Use_Fixed_Pin           :        Boolean;
      Pin                     :        Unsigned_32;
      Bonding_Required        :        Boolean;
      Status                  :    out Byte)
      return Boolean
   is
      Command       : constant OpCode := ACI_GAP_Set_Auth_Requirement;
      MITM_Mode     : Byte := 0;
      OOB_Enable    : Byte := 0;
      Bonding_Mode  : Byte := 0;
      Fixed_Pin     : Byte := 0;
      Pin_Parameter : Byte_Array (1 .. 4);
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

      Pin_Parameter (1) := Byte (Pin and 16#FF#);
      Pin_Parameter (2) := Byte (Shift_Right (Pin,  8) and 16#FF#);
      Pin_Parameter (3) := Byte (Shift_Right (Pin, 16) and 16#FF#);
      Pin_Parameter (4) := Byte (Shift_Right (Pin, 24) and 16#FF#);

      declare
         Parameters : constant
                      Byte_Array := Byte_Array'(MITM_Mode &
                                                OOB_Enable &
                                                OOB_Data &
                                                Min_Encryption_Key_Size &
                                                Max_Encryption_Key_Size &
                                                Fixed_Pin &
                                                Pin_Parameter &
                                                Bonding_Mode);
         Response   : constant
                      Byte_Array := Send_Command
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
      Advertising_Interval_Min :        Unsigned_16;
      Advertising_Interval_Max :        Unsigned_16;
      BT_Address_Type          :        Address_Type;
      Filter_Policy            :        Filter_Policy_Type;
      Local_Name_Type          :        Name_Type;
      Local_Name               :        String;
      Service_UUID_List        :        Byte_Array;
      Slave_Conn_Interval_Min  :        Unsigned_16;
      Slave_Conn_Interval_Max  :        Unsigned_16;
      Status                   :    out Byte)
      return Boolean
   is
      Advertising_Event_Byte : constant Byte := Advertising_Event'Enum_Rep;
      Name_Length            : Byte;
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
                      Byte_Array := Byte_Array'(Advertising_Event_Byte &
                                                To_Byte_Array (Advertising_Interval_Min) &
                                                To_Byte_Array (Advertising_Interval_Max) &
                                                BT_Address_Type'Enum_Rep &
                                                Filter_Policy'Enum_Rep &
                                                Name_Length &
                                                Local_Name_Type'Enum_Rep &
                                                To_Byte_Array (Local_Name) &
                                                Byte (Service_UUID_List'Length) &
                                                Service_UUID_List &
                                                To_Byte_Array (Slave_Conn_Interval_Min) &
                                                To_Byte_Array (Slave_Conn_Interval_Max));
         Response   : constant
                      Byte_Array := Send_Command (Device, Command, Parameters);
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
      Conn_Handle      :        Byte_Array;
      Bonding_Required :        Boolean;
      MITM_Required    :        Boolean)
      return Boolean
   is
      Bonding : Byte            := 0;
      MITM    : Byte            := 0;
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
                      Byte_Array := Byte_Array'(Conn_Handle &
                                                Bonding &
                                                MITM);
         Response   : constant
                      Byte_Array := Send_Command (Device, Command, Parameters);
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
      Max_Value_Length      :        Byte;
      Properties            :        Byte;
      Security              :        Byte;
      Event_Mask            :        Byte;
      Encryption_Key_Size   :        Byte;
      Value_Is_Fixed_Length :        Boolean;
      Status                :    out Byte;
      Char_Handle           :    out Handle)
      return Boolean
   is
      Is_Variable : Byte;
      Command     : constant OpCode := ACI_GATT_Add_Char;
   begin

      if Value_Is_Fixed_Length then
         Is_Variable := 0;
      else
         Is_Variable := 1;
      end if;

      declare
         Parameters : constant Byte_Array :=
                                  Byte_Array'(Service &
                                              16#01# &
                                              Byte_Array (UUID) &
                                              Max_Value_Length &
                                              Properties &
                                              Security &
                                              Event_Mask &
                                              Encryption_Key_Size &
                                              Is_Variable);
         Response   : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
      Max_Value_Length      :        Byte;
      Properties            :        Byte;
      Security              :        Byte;
      Event_Mask            :        Byte;
      Encryption_Key_Size   :        Byte;
      Value_Is_Fixed_Length :        Boolean;
      Status                :    out Byte;
      Char_Handle           :    out Handle)
      return Boolean
   is
      Is_Variable : Byte;
      Command     : constant OpCode := ACI_GATT_Add_Char;
   begin

      if Value_Is_Fixed_Length then
         Is_Variable := 0;
      else
         Is_Variable := 1;
      end if;

      declare
         Parameters : constant Byte_Array :=
                                 Byte_Array'(Service &
                                             16#02# &
                                             Byte_Array (UUID) &
                                             Max_Value_Length &
                                             Properties &
                                             Security &
                                             Event_Mask &
                                             Encryption_Key_Size &
                                             Is_Variable);
         Response   : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
       Max_Attribute_Records :        Byte;
       Status                :    out Byte;
       Service_Handle        :    out Handle)
       return Boolean
   is
      Command : constant OpCode := ACI_GATT_Add_Serv;
   begin
      declare
         Parameters : constant Byte_Array :=
                                 Byte_Array'(16#01# &
                                             Byte_Array (UUID) &
                                             Service'Enum_Rep &
                                             Max_Attribute_Records);
         Response   : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
       Max_Attribute_Records :        Byte;
       Status                :    out Byte;
       Service_Handle        :    out Handle)
       return Boolean
   is
      Command : constant OpCode := ACI_GATT_Add_Serv;
   begin
      declare
         Parameters : constant Byte_Array :=
                                 Byte_Array'(16#02# &
                                             Byte_Array (UUID) &
                                             Service'Enum_Rep &
                                             Max_Attribute_Records);
         Response   : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
      Status :    out Byte)
      return Boolean
   is
      Command : constant OpCode := ACI_GATT_Init;
   begin
      declare
         Response : constant Byte_Array := Send_Command (Device, Command);
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
      Offset            :        Byte;
      Char_Value        :        Byte_Array;
      Status            :    out Byte)
      return Boolean
   is
      Command : constant OpCode := ACI_GATT_Update_Char_Value;
   begin
      declare
         Parameters : constant Byte_Array :=
                                 Byte_Array'(Service_Handle &
                                             Char_Handle &
                                             Offset &
                                             Byte (Char_Value'Length) &
                                             Char_Value);
         Response   : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
      HCI_Version        :    out Unsigned_8;
      HCI_Revision       :    out Unsigned_16;
      LMP_PAL_Version    :    out Unsigned_8;
      Mfr_Name           :    out Unsigned_16;
      LMP_PAL_Subversion :    out Unsigned_16;
      Status             :    out Byte)
      return Boolean
   is
      Command : constant OpCode := HCI_Read_Local_Version_Information;
   begin
      declare
         Response : constant Byte_Array := Send_Command (Device, Command);
      begin
         if Response'Length = 15 then
            Status := Response (7);
            HCI_Version        := Response (8);
            HCI_Revision       := Unsigned_16 (Response (9) +
                                               Response (10) * 16#FF#);
            LMP_PAL_Version    := Response (11);
            Mfr_Name           := Unsigned_16 (Response (12) +
                                               Response (13) * 16#FF#);
            LMP_PAL_Subversion := Unsigned_16 (Response (14) +
                                               Response (15) * 16#FF#);
            return True;
         else
            HCI_Version        := 0;
            HCI_Revision       := 0;
            LMP_PAL_Version    := 0;
            Mfr_Name           := 0;
            LMP_PAL_Subversion := 0;
            Status := 0;
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
      Status            :    out Byte)
      return Boolean
   is
      High_Power_Byte  : Byte;
      Power_Level_Byte : constant Byte := Byte (Level);
      Command          : constant OpCode := ACI_HAL_Set_Tx_Power_Level;
   begin
      if Enable_High_Power then
         High_Power_Byte := 1;
      else
         High_Power_Byte := 0;
      end if;

      declare
         Parameters : constant Byte_Array := Byte_Array'(High_Power_Byte &
                                                         Power_Level_Byte);
         Response   : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
      Offset :        Byte;
      Value  :        Byte_Array;
      Status :    out Byte)
      return Boolean
   is
      Parameters : Byte_Array (1 .. Value'Length + 2);
      Command    : constant OpCode := ACI_HAL_Write_Config_Data;
   begin
      Parameters (1)                    := Offset;
      Parameters (2)                    := Value'Length;
      Parameters (3 .. Parameters'Last) := Value;

      declare
         Response : constant Byte_Array := Send_Command (Device, Command, Parameters);
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
      return Byte_Array
   is
      Operation        : constant SPI_Data_8b (1 .. 1) := (1 => Read_Operation);
      Empty            : constant SPI_Data_8b (1 .. 4) := (others => 16#00#);
      No_Data          : Byte_Array (1 .. 0);
      Read_Buffer_Size : Byte;
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
         Data  : Byte_Array  (1 .. Integer (Read_Buffer_Size));
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
      return Byte_Array
   is
      Operation        : constant SPI_Data_8b (1 .. 1) := (1 => Read_Operation);
      Empty            : constant SPI_Data_8b (1 .. 4) := (others => 16#00#);
      No_Data          : Byte_Array (1 .. 0);
      Read_Buffer_Size : Byte;
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
         Data  : Byte_Array  (1 .. Integer (Count));
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
      HAL_Time  : constant HAL.Time.Any_Delays := Ravenscar_Time.Delays;
   begin
      Device.Reset_Pin.all.Clear;
      HAL_Time.all.Delay_Milliseconds (Reset_Duration);
      Device.Reset_Pin.all.Set;
      HAL_Time.all.Delay_Milliseconds (Reset_Duration);
   end Reset;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode;
      Parameters :        Byte_Array)
      return Boolean
   is
      HAL_Time  : constant HAL.Time.Any_Delays := Ravenscar_Time.Delays;
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
         HAL_Time.all.Delay_Microseconds (Wait_For_Wakeup_Duration);
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
      HAL_Time  : constant HAL.Time.Any_Delays := Ravenscar_Time.Delays;
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
         HAL_Time.all.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;
      return True;
   end Send_Command;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out BlueNRG_MS_Device;
      Command    :        OpCode;
      Parameters :        Byte_Array)
      return Byte_Array
   is
      HAL_Time   : constant HAL.Time.Any_Delays := Ravenscar_Time.Delays;
      Try_Count  : Natural := 0;
      Null_Array : Byte_Array (1 .. 0);
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
         HAL_Time.all.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;

      Try_Count := 0;

      loop
         declare
            Response : constant Byte_Array := Device.Read;
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
         HAL_Time.all.Delay_Microseconds (Response_Delay_Duration);
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
      return Byte_Array
   is
      HAL_Time   : constant HAL.Time.Any_Delays := Ravenscar_Time.Delays;
      Try_Count  : Natural := 0;
      Null_Array : Byte_Array (1 .. 0);
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
         HAL_Time.all.Delay_Microseconds (Wait_For_Wakeup_Duration);
      end loop;

      Try_Count := 0;

      loop
         declare
            Response : constant Byte_Array := Device.Read;
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
         HAL_Time.all.Delay_Microseconds (Response_Delay_Duration);
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
      Status            :    out Byte)
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
   -- To_Byte_Array --
   -------------------

   function To_Byte_Array
     (Input : Unsigned_16)
      return Byte_Array
   is
      No_Output : Byte_Array (1 .. 0);
      First     : Byte;
      Second    : Byte;
   begin
      First  := Byte (Input and 16#00FF#);
      Second := Byte (Shift_Right (Input, 8));
      return Byte_Array'(Second, First);
   exception
      when Constraint_Error =>
         return No_Output;
   end To_Byte_Array;

   -------------------
   -- To_Byte_Array --
   -------------------

   function To_Byte_Array
     (Input : String)
      return Byte_Array
   is
      No_Output    : Byte_Array (1 .. 0);
      Output_Index : Natural := 1;
   begin
      declare
         Output : Byte_Array (1 .. Input'Length);
      begin
         for Input_Index in Integer range Input'Range loop
            Output (Output_Index) := Byte (Character'Pos (Input (Input_Index)));
            Output_Index := Output_Index + 1;
         end loop;
         return Output;
      end;
   exception
      when Constraint_Error =>
         return No_Output;
   end To_Byte_Array;

   -----------
   -- Write --
   -----------

   function Write
     (Device : in out BlueNRG_MS_Device;
      Data   :        Byte_Array)
      return Boolean
   is
      Operation         : constant SPI_Data_8b (1 .. 1) := (1 => Write_Operation);
      Empty             : constant SPI_Data_8b (1 .. 4) := (others => 16#00#);
      Write_Buffer_Size : Byte;
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

--      Device.SPI_Port.all.Transmit_Receive (16#00#, Write_Buffer_Size);
--      Device.SPI_Port.all.Transmit_Receive (16#00#, Dummy);
--      Device.SPI_Port.all.Transmit_Receive (16#00#, Dummy);
--      Device.SPI_Port.all.Transmit_Receive (16#00#, Dummy);

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

--      for Index in Integer range Data'First .. Data'Last loop
--         Device.SPI_Port.all.Transmit_Receive (Data (Index), Dummy);
--      end loop;

      Device.Chip_Select_Pin.all.Set;

      return True;
   end Write;

end BlueNRG_MS;
