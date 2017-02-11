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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;  use Ada.Real_Time;
with Bluetooth;      use Bluetooth;
with Bluetooth.HCI;  use Bluetooth.HCI;
with Conversions;    use Conversions;
with HAL;            use HAL;
with HAL.SPI;        
with STM32.Device;   use STM32.Device;
with STM32.SPI;

package body BlueNRG_MS is

   Debug_Mode               : constant Boolean   := True;

   Reset_Duration           : constant Time_Span := Milliseconds(10);
   Wait_For_Wakeup_Duration : constant Time_Span := Microseconds(50);
   Maximum_Command_Retries  : constant           := 10;
   Response_Timeout         : constant Time_Span := Milliseconds(5);

   -------------------
   -- To_Byte_Array --
   -------------------

   function To_Byte_Array
     (Input : Unsigned_16)
      return Byte_Array
   is
      First  : Byte := Byte(Input and 16#00FF#);
      Second : Byte := Byte(Shift_Right(Input, 8));
   begin
      return Byte_Array'(Second, First);
   end To_Byte_Array;

   -------------------
   -- To_Byte_Array --
   -------------------

   function To_Byte_Array
     (Input : String)
      return Byte_Array
   is
      No_Output    : Byte_Array(1 .. 0);
      Output       : Byte_Array(1 .. Input'Length);
      Output_Index : Natural := 1;
   begin
      for Input_Index in Integer range Input'Range loop
         Output(Output_Index) := Byte(Character'Pos(Input(Input_Index)));
         Output_Index := Output_Index + 1;
      end loop;

      return Output;
   end To_Byte_Array;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out Tranceiver;
      Command    : in     OpCode;
      Parameters : in     Byte_Array)
      return Boolean
   is
      Wake_Time : Time;
      Try_Count : Natural := 0;
   begin
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write(Command_Packet_Type &
                                Command &
                                Parameters'Length &
                                Parameters) = True;
         Wake_Time := Clock + Wait_For_Wakeup_Duration;
         if Try_Count = Maximum_Command_Retries then
            return False;
         end if;
         delay until Wake_Time;
      end loop;
      return True;
   end Send_Command;
   
   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out Tranceiver;
      Command    : in     OpCode)
      return Boolean
   is
      Wake_Time : Time;
      Try_Count : Natural := 0;
   begin
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write(Command_Packet_Type &
                                Command &
                                16#00#) = True;
         Wake_Time := Clock + Wait_For_Wakeup_Duration;
         if Try_Count = Maximum_Command_Retries then
            return False;
         end if;
         delay until Wake_Time;
      end loop;
      return True;
   end Send_Command;
   
   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out Tranceiver;
      Command    : in     OpCode;
      Parameters : in     Byte_Array)
      return Byte_Array
   is
      Timeout    : Time;
      Wake_Time  : Time;
      Try_Count  : Natural := 0;
      Null_Array : Byte_Array(1 .. 0);
   begin
      -- Send Command --
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write(Command_Packet_Type &
                                Command &
                                Parameters'Length &
                                Parameters) = True;
         Wake_Time := Clock + Wait_For_Wakeup_Duration;
         if Try_Count = Maximum_Command_Retries then
            return Null_Array;
         end if;
         delay until Wake_Time;
      end loop;
    
      -- Wait for response --
      Timeout := Clock + Response_Timeout;

      loop
         declare
            Response : Byte_Array := Device.Read;
         begin
            if Response'Length >= 7 then
               if Response(1) = Event_Packet_Type and
                  Response(5 .. 6) = Command
               then
                  return Response;
               end if;
            end if;
         end;
         exit when Clock > Timeout;
      end loop;

      return Null_Array;
   end Send_Command;
   
   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Device     : in out Tranceiver;
      Command    : in     OpCode)
      return Byte_Array
   is
      Timeout    : Time;
      Wake_Time  : Time;
      Try_Count  : Natural := 0;
      Null_Array : Byte_Array(1 .. 0);
   begin
      -- Send Command --
      loop
         Try_Count := Try_Count + 1;
         exit when Device.Write(Command_Packet_Type &
                                Command &
                                16#00#) = True;
         Wake_Time := Clock + Wait_For_Wakeup_Duration;
         if Try_Count = Maximum_Command_Retries then
            return Null_Array;
         end if;
         delay until Wake_Time;
      end loop;
    
      -- Wait for response --
      Timeout := Clock + Response_Timeout;

      loop
         declare
            Response : Byte_Array := Device.Read;
         begin
            if Response'Length >= 7 then
               if Response(1) = Event_Packet_Type and
                  Response(5 .. 6) = Command
               then
                  return Response;
               end if;
            end if;
         end;
         exit when Clock > Timeout;
      end loop;

      return Null_Array;
   end Send_Command;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Device : in out Tranceiver)
   is

      procedure Initialize_SPI
        (Device : in out Tranceiver);
      procedure Initialize_GPIO
        (Device : in out Tranceiver);
      
      SPI : SPI_Port renames Device.Port.all;

      --------------------
      -- Initialize_SPI --
      --------------------

      procedure Initialize_SPI
        (Device : in out Tranceiver)
      is
         Config : STM32.SPI.SPI_Configuration;
      begin
         Enable_Clock(SPI);

         Config.Mode                := Master;
         Config.Baud_Rate_Prescaler := BRP_32;
         Config.Clock_Polarity      := Low;
         Config.Clock_Phase         := P1Edge;
         Config.First_Bit           := MSB;
         Config.CRC_Poly            := 7;
         Config.Slave_Management    := Software_Managed;
         Config.Direction           := D2Lines_FullDuplex;
         Config.Data_Size           := HAL.SPI.Data_Size_8b;

         Disable(SPI);
         Configure(SPI, Config);
         Enable(SPI);
      end Initialize_SPI;

      ---------------------
      -- Initialize_GPIO --
      ---------------------

      procedure Initialize_GPIO
        (Device : in out Tranceiver)
      is
         Config     : GPIO_Port_Configuration;
         SPI_Points : constant GPIO_Points := Device.SCK_Pin &
                                              Device.MISO_Pin &
                                              Device.MOSI_Pin;
      begin
         Enable_Clock(SPI_Points);

         Config.Output_Type := Push_Pull;
         Config.Resistors   := Floating;
         Config.Speed       := Speed_25MHz;
         Config.Mode        := Mode_AF;

         Configure_IO(SPI_Points, Config);
         Configure_Alternate_Function(SPI_Points, Device.SPI_AF);

         Enable_Clock(Device.Chip_Select_Pin);

         Config.Output_Type := Push_Pull;
         Config.Resistors   := Pull_Up;
         Config.Speed       := Speed_25MHz;
         Config.Mode        := Mode_Out;

         Device.Chip_Select_Pin.Configure_IO(Config);
         Device.Chip_Select_Pin.Set;

         Enable_Clock(Device.Reset_Pin);

         Config.Output_Type := Push_Pull;
         Config.Resistors   := Pull_Up;
         Config.Speed       := Speed_25MHz;
         Config.Mode        := Mode_Out;

         Device.Reset_Pin.Configure_IO(Config);
         Device.Reset_Pin.Set;
      end Initialize_GPIO;

   begin
      Initialize_GPIO(Device);
      Initialize_SPI(Device);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Device : in out Tranceiver)
   is
      Stop_Time      : Time;
   begin
      Stop_Time := Clock + Reset_Duration;
      Device.Reset_Pin.Clear;
      delay until Stop_Time;
      Device.Reset_Pin.Set;
      Stop_Time := Clock + Reset_Duration;
      delay until Stop_Time;
   end Reset;

   ----------------------------------
   -- Set_Bluetooth_Public_Address --
   ----------------------------------

   function Set_Bluetooth_Public_Address
     (Device            : in out Tranceiver;
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

   ---------------
   -- GATT_Init --
   ---------------

   function GATT_Init
     (Device : in out Tranceiver;
      Status :    out Byte)
      return Boolean
   is
      Command : constant OpCode := ACI_GATT_Init;
   begin
      declare
         Response : Byte_Array := Send_Command(Device, Command);
      begin
         if Response'Length = 7 then
            Status := Response(7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end GATT_Init;

   ----------------------
   -- GATT_Add_Service --
   ----------------------
   
   function GATT_Add_Service
      (Device                : in out Tranceiver;
       UUID                  : in     UUID_16;
       Service               : in     Service_Type;
       Max_Attribute_Records : in     Byte;
       Status                :    out Byte;
       Service_Handle        :    out Handle)
       return Boolean
   is
      Command : constant OpCode := ACI_GATT_Add_Serv;
   begin
      declare
         Parameters : Byte_Array := Byte_Array'(16#01# &
                                                Byte_Array(UUID) &
                                                Service'Enum_Rep &
                                                Max_Attribute_Records);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 9 then
           Status         := Response(7);
           Service_Handle := Response(8 .. 9);
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
      (Device                : in out Tranceiver;
       UUID                  : in     UUID_128;
       Service               : in     Service_Type;
       Max_Attribute_Records : in     Byte;
       Status                :    out Byte;
       Service_Handle        :    out Handle)
       return Boolean
   is
      Command : constant OpCode := ACI_GATT_Add_Serv;
   begin
      declare
         Parameters : Byte_Array := Byte_Array'(16#02# &
                                                Byte_Array(UUID) &
                                                Service'Enum_Rep &
                                                Max_Attribute_Records);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 9 then
           Status         := Response(7);
           Service_Handle := Response(8 .. 9);
           return True;
        else
           Status := 0;
           return False;
        end if;
      end;
   end GATT_Add_Service;

   -----------------------------
   -- GATT_Add_Characteristic --
   -----------------------------

   function GATT_Add_Characteristic
     (Device                : in out Tranceiver;
      Service               : in     Handle;
      UUID                  : in     UUID_16;
      Max_Value_Length      : in     Byte;
      Properties            : in     Byte;
      Security              : in     Byte;
      Event_Mask            : in     Byte;
      Encryption_Key_Size   : in     Byte;
      Value_Is_Fixed_Length : in     Boolean;
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
         Parameters : Byte_Array := Byte_Array'(Service &
                                                16#01# &
                                                Byte_Array(UUID) &
                                                Max_Value_Length &
                                                Properties &
                                                Security &
                                                Event_Mask &
                                                Encryption_Key_Size &
                                                Is_Variable);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 9 then
           Status      := Response(7);
           Char_Handle := Response(8 .. 9);
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
     (Device                : in out Tranceiver;
      Service               : in     Handle;
      UUID                  : in     UUID_128;
      Max_Value_Length      : in     Byte;
      Properties            : in     Byte;
      Security              : in     Byte;
      Event_Mask            : in     Byte;
      Encryption_Key_Size   : in     Byte;
      Value_Is_Fixed_Length : in     Boolean;
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
         Parameters : Byte_Array := Byte_Array'(Service &
                                                16#02# &
                                                Byte_Array(UUID) &
                                                Max_Value_Length &
                                                Properties &
                                                Security &
                                                Event_Mask &
                                                Encryption_Key_Size &
                                                Is_Variable);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 9 then
           Status      := Response(7);
           Char_Handle := Response(8 .. 9);
           return True;
        else
           Status := 0;
           return False;
        end if;
      end;
   end GATT_Add_Characteristic;
   
   -------------------------------------
   -- GATT_Update_Characteristic_Value --
   -------------------------------------
   
   function GATT_Update_Characteristic_Value
     (Device            : in out Tranceiver;
      Service_Handle    : in     Handle;
      Char_Handle       : in     Handle;
      Offset            : in     Byte;
      Char_Value        : in     Byte_Array;
      Status            :    out Byte)
      return Boolean
    is
       Command : constant OpCode := ACI_GATT_Update_Char_Value;
    begin
      declare
         Parameters : Byte_Array := Byte_Array'(Service_Handle &
                                                Char_Handle &
                                                Offset &
                                                Byte(Char_Value'Length) &
                                                Char_Value);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 7 then
           Status := Response(7);
           return True;
        else
           Status := 0;
           return False;
        end if;
      end;
    end GATT_Update_Characteristic_Value;

   --------------
   -- GAP_Init --
   --------------

   function GAP_Init
     (Device                  : in out Tranceiver;
      Role                    : in     Device_Role;
      Enable_Privacy          : in     Boolean;
      Device_Name_Length      : in     Byte;
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
         Parameters : Byte_Array := Byte_Array'(Role'Enum_Rep &
                                                Privacy &
                                                Device_Name_Length);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 13 then
           Status                  := Response(7);
           Service_Handle          := Response(8 .. 9);
           Device_Name_Char_Handle := Response(10 .. 11);
           Appearance_Char_Handle  := Response(12 .. 13);
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
     (Device                  : in out Tranceiver;
      MITM_Required           : in     Boolean;
      OOB_Enabled             : in     Boolean;
      OOB_Data                : in     Byte_Array;
      Min_Encryption_Key_Size : in     Byte;
      Max_Encryption_Key_Size : in     Byte;
      Use_Fixed_Pin           : in     Boolean;
      Pin                     : in     Unsigned_32;
      Bonding_Required        : in     Boolean;
      Status                  :    out Byte)
      return Boolean
   is
      Command       : constant OpCode := ACI_GAP_Set_Auth_Requirement;
      MITM_Mode     : Byte := 0;
      OOB_Enable    : Byte := 0;
      Bonding_Mode  : Byte := 0;
      Fixed_Pin     : Byte := 0;
      Pin_Parameter : Byte_Array(1 .. 4);
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

      Pin_Parameter(1) := Byte(Pin and 16#FF#);
      Pin_Parameter(2) := Byte(Shift_Right(Pin,  8) and 16#FF#);
      Pin_Parameter(3) := Byte(Shift_Right(Pin, 16) and 16#FF#);
      Pin_Parameter(4) := Byte(Shift_Right(Pin, 24) and 16#FF#);
      
      declare
         Parameters : Byte_Array := Byte_Array'(MITM_Mode &
                                                OOB_Enable &
                                                OOB_Data &
                                                Min_Encryption_Key_Size &
                                                Max_Encryption_Key_Size &
                                                Fixed_Pin &
                                                Pin_Parameter &
                                                Bonding_Mode);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 7 then
           Status := Response(7);
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
     (Device                   : in out Tranceiver;
      Advertising_Event        : in     Advertising_Event_Type;
      Advertising_Interval_Min : in     Unsigned_16;
      Advertising_Interval_Max : in     Unsigned_16;
      Address                  : in     Address_Type;
      Filter_Policy            : in     Filter_Policy_Type;
      Local_Name_Type          : in     Name_Type;
      Local_Name               : in     String;
      Service_UUID_List        : in     Byte_Array;
      Slave_Conn_Interval_Min  : in     Unsigned_16;
      Slave_Conn_Interval_Max  : in     Unsigned_16;
      Status                   :    out Byte)
      return Boolean
   is
      Advertising_Event_Byte : Byte := Advertising_Event'Enum_Rep;
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
         Parameters : Byte_Array := Byte_Array'(Advertising_Event_Byte &
                                                To_Byte_Array(Advertising_Interval_Min) &
                                                To_Byte_Array(Advertising_Interval_Max) &
                                                Address'Enum_Rep &
                                                Filter_Policy'Enum_Rep &
                                                Name_Length &
                                                Local_Name_Type'Enum_Rep &
                                                To_Byte_Array(Local_Name) &
                                                Byte(Service_UUID_List'Length) &
                                                Service_UUID_List &
                                                To_Byte_Array(Slave_Conn_Interval_Min) &
                                                To_Byte_Array(Slave_Conn_Interval_Max));
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 7 then
           Status := Response(7);
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
     (Device           : in out Tranceiver;
      Conn_Handle      : in     Byte_Array;
      Bonding_Required : in     Boolean;
      MITM_Required    : in     Boolean)
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
         Parameters : Byte_Array := Byte_Array'(Conn_Handle &
                                                Bonding &
                                                MITM);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
         if Response'Length = 4 then
            if Response(4) = 0 then
               return True;
            end if;
            return False;
         else
            return False;
         end if;
      end;
   end GAP_Slave_Security_Request;

   ---------------------------
   -- HAL_Write_Config_Data --
   ---------------------------

   function HAL_Write_Config_Data
     (Device : in out Tranceiver;
      Offset : in     Byte;
      Value  : in     Byte_Array;
      Status :    out Byte)
      return Boolean
   is
      Parameters : Byte_Array (1 .. Value'Length + 2);
      Command    : constant OpCode := ACI_HAL_Write_Config_Data;
   begin
      Parameters(1)                    := Offset; 
      Parameters(2)                    := Value'Length;
      Parameters(3 .. Parameters'Last) := Value;

      declare
         Response : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
         if Response'Length = 7 then
            Status := Response(7);
            return True;
         else
            Status := 0;
            return False;
         end if;
      end;
   end HAL_Write_Config_Data;

   -------------------------
   -- Get_BlueNRG_Verison --
   -------------------------

   function Get_BlueNRG_Version
     (Device             : in out Tranceiver;
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
         Response : Byte_Array := Send_Command(Device, Command);
      begin
        if Response'Length = 15 then
           Status := Response(7);
           HCI_Version        := Response(8);
           HCI_Revision       := Unsigned_16(
                                 Response(9) + 
                                 Response(10) * 16#FF#);
           LMP_PAL_Version    := Response(11);
           Mfr_Name           := Unsigned_16(
                                 Response(12) + 
                                 Response(13) * 16#FF#);
           LMP_PAL_Subversion := Unsigned_16(
                                 Response(14) + 
                                 Response(15) * 16#FF#);
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
     (Device            : in out Tranceiver;
      Enable_High_Power : in     Boolean;
      Level             : in     Power_Level;
      Status            :    out Byte)
      return Boolean
   is
      High_Power_Byte  : Byte;
      Power_Level_Byte : Byte := Byte(Level);
      Command          : constant OpCode := ACI_HAL_Set_Tx_Power_Level;
   begin
      if Enable_High_Power then
         High_Power_Byte := 1;
      else
         High_Power_Byte := 0;
      end if;
      
      declare
         Parameters : Byte_Array := Byte_Array'(High_Power_Byte &
                                                Power_Level_Byte);
         Response   : Byte_Array := Send_Command(Device, Command, Parameters);
      begin
        if Response'Length = 7 then
           Status := Response(7);
           return True;
        else
           Status := 0;
           return False;
        end if;
      end;
   end HAL_Set_TX_Power_Level;

end BlueNRG_MS;
