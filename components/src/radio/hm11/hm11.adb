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

package body HM11 is

   Ok_Get : constant String := "OK+Get:";
   Ok_Set : constant String := "OK+Set:";

   function Image (Value : Natural) return String;
   function Image (Value : Send_Data_Method) return String;
   function Image (Value : Send_Data_Characteristic) return String;
   function Image (Value : Boolean) return String;
   --  Returns "1" if Value = True else "0"
   function Image (Value : MAC_Address_Type) return String;

   function Value (V : HAL.UInt8) return Natural;

   -----------------------------
   -- Default_Receive_Handler --
   -----------------------------

   procedure Default_Receive_Handler
     (Port      : HAL.UART.Any_UART_Port;
      Received  : System.Address;
      Length    : Natural;
      Status    : out UART_Status;
      Timeout   : Natural := 1000;
      As_Stream : Boolean := False)
   is
      pragma Unreferenced (Timeout);
      Data : UART_Data_8b (1 .. Length) with Import, Address => Received;
   begin
      pragma Assert (not As_Stream);

      Data := (others => 0);
      Receive (Port.all, Data, Status);
   end Default_Receive_Handler;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (This    : in out HM11_Driver;
      Command : String;
      Status  : out UART_Status)
   is
      Local    : constant String := Command;
      Cmd_Data : UART_Data_8b (Local'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      This.Port.Transmit (Cmd_Data, Status);
   end Transmit;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (This    : in out HM11_Driver;
      Command : String;
      Data    : UART_Data_8b;
      Status  : out UART_Status)
   is
      Local    : constant String := Command;
      Cmd_Data : UART_Data_8b (Local'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      This.Port.Transmit (Cmd_Data & Data, Status);
   end Transmit;

   --------------------
   -- Check_Responce --
   --------------------

   procedure Check_Responce
     (Received : System.Address;
      Length   : Natural;
      Expect   : String;
      Status   : in out UART_Status)
   is
      Data : UART_Data_8b (1 .. Length) with Import, Address => Received;
   begin
      if Status = Ok then
         declare
            Local : constant String := Expect;
            Exp   : UART_Data_8b (Local'Range) with Import,
              Address => Local'Address;
         begin
            if Length < Exp'Length
              or else Data (Data'First .. Data'First + Exp'Length - 1) /= Exp
            then
               Status := Err_Error;
            end if;
         end;
      end if;
   end Check_Responce;

   ------------------------
   -- Transmit_And_Check --
   ------------------------

   procedure Transmit_And_Check
     (This       : in out HM11_Driver;
      Command    : String;
      Expect     : String;
      Received   : System.Address;
      Length     : Natural;
      Status     : out UART_Status) is
   begin
      Transmit (This, Command, Status);

      if Status = Ok then
         This.Receive (This.Port, Received, Length, Status);
         if Status /= Ok then
            return;
         end if;

         Check_Responce (Received, Length, Expect, Status);
      end if;
   end Transmit_And_Check;

   ------------------------
   -- Transmit_And_Check --
   ------------------------

   procedure Transmit_And_Check
     (This    : in out HM11_Driver;
      Command : String;
      Expect  : String;
      Status  : out UART_Status) is
   begin
      Transmit_And_Check
        (This, Command, Expect, This.Responce'Address, Expect'Length, Status);
   end Transmit_And_Check;

   --------------
   -- Get_Role --
   --------------

   procedure Get_Role
     (This   : in out HM11_Driver;
      Result : out Role;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+ROLE?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Role'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Role;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role
     (This   : in out HM11_Driver;
      Value  : Role;
      Status : out UART_Status)
   is
      C : constant String := Image (Role'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+ROLE" & C, Ok_Set & C, Status);
   end Set_Role;

   ---------------------------------------
   -- Get_Last_Connected_Device_Address --
   ---------------------------------------

   procedure Get_Last_Connected_Device_Address
     (This   : in out HM11_Driver;
      MAC    : out MAC_Address;
      Status : out UART_Status)
   is
      Expect : constant String := "OK+RADD:";
   begin
      Transmit_And_Check
        (This, "AT+RADD?", Expect,
         This.Responce'Address, Expect'Length + MAC_Address'Length, Status);

      if Status = Ok then
         declare
            Value : MAC_Address with Import,
              Address => This.Responce (Expect'Length + 1)'Address;
         begin
            MAC := Value;
         end;
      end if;
   end Get_Last_Connected_Device_Address;

   ------------------------------
   -- Get_Sensor_Work_Interval --
   ------------------------------

   procedure Get_Sensor_Work_Interval
     (This     : in out HM11_Driver;
      Interval : out Work_Interval;
      Status   : out UART_Status)
   is
      use type HAL.UInt8;
   begin
      Interval := "00";
      Transmit_And_Check
        (This, "AT+RAT??", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 2, Status);

      if Status = Ok then
         if This.Responce (Ok_Get'Length + 2) = 0 then
            declare
               L : Digit_Character with Import,
                 Address => This.Responce (Ok_Get'Length + 1)'Address;
            begin
               Interval (Interval'Last) := L;
            end;
         else
            declare
               L : Work_Interval with Import,
                 Address => This.Responce (Ok_Get'Length + 1)'Address;
            begin
               Interval := L;
            end;
         end if;
      end if;
   end Get_Sensor_Work_Interval;

   ------------------------------
   -- Set_Sensor_Work_Interval --
   ------------------------------

   procedure Set_Sensor_Work_Interval
     (This     : in out HM11_Driver;
      Interval : Work_Interval;
      Status   : out UART_Status) is
   begin
      if Interval (Interval'First) = '0' then
         declare
            C : constant Character := Character (Interval (Interval'Last));
         begin
            Transmit_And_Check (This, "AT+RAT" & C, Ok_Set & C, Status);
         end;

      else
         declare
            Local : constant Work_Interval := Interval;
            S     : String (Work_Interval'Range) with Import,
              Address => Local (Local'First)'Address;
         begin
            Transmit_And_Check (This, "AT+RAT" & S, Ok_Set & S, Status);
         end;
      end if;
   end Set_Sensor_Work_Interval;

   ------------------
   -- Set_Stop_Bit --
   ------------------

   procedure Set_Stop_Bit
     (This   : in out HM11_Driver;
      Value  : Stop_Bit;
      Status : out UART_Status)
   is
      S : constant String := Image (Stop_Bit'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+STOP" & S, Ok_Set & S, Status);
   end Set_Stop_Bit;

   ------------------
   -- Get_Stop_Bit --
   ------------------

   procedure Get_Stop_Bit
     (This   : in out HM11_Driver;
      Result : out Stop_Bit;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+STOP?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Stop_Bit'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Stop_Bit;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+SLEEP", "OK+SLEEP", Status);
   end Sleep;

   -------------
   -- Wake_Up --
   -------------

   procedure Wake_Up
     (This   : in out HM11_Driver;
      Status : out UART_Status)
   is
      Cmd : constant String (1 .. 7) := (others => 'W');
   begin
      Transmit_And_Check (This, Cmd, "OK+WAKE", Status);
   end Wake_Up;

   ----------
   -- Test --
   ----------

   procedure Test
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT", "OK", Status);
   end Test;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT", When_Disconnected_Message, Status);
   end Disconnect;

   ------------------------------
   -- Get_Advertising_Interval --
   ------------------------------

   procedure Get_Advertising_Interval
     (This   : in out HM11_Driver;
      Result : out Advertising_Interval;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+ADVI?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         case Character'Val (This.Responce (Ok_Get'Length + 1)) is
            when '0' => Result := ms_100;
            when '1' => Result := ms_211;
            when '2' => Result := ms_252;
            when '3' => Result := ms_318;
            when '4' => Result := ms_417;
            when '5' => Result := ms_546;
            when '6' => Result := ms_760;
            when '7' => Result := ms_852;
            when '8' => Result := ms_1022;
            when '9' => Result := ms_1285;
            when 'A' => Result := ms_2000;
            when 'B' => Result := ms_3000;
            when 'C' => Result := ms_4000;
            when 'D' => Result := ms_5000;
            when 'E' => Result := ms_6000;
            when 'F' => Result := ms_7000;
            when others =>
               Status := Err_Error;
         end case;
      end if;
   end Get_Advertising_Interval;

   ------------------------------
   -- Set_Advertising_Interval --
   ------------------------------

   procedure Set_Advertising_Interval
     (This   : in out HM11_Driver;
      Value  : Advertising_Interval;
      Status : out UART_Status)
   is
      C : constant Character := From_Advertising_Interval (Value);
   begin
      Transmit_And_Check (This, "AT+ADVI" & C, Ok_Set & C, Status);
   end Set_Advertising_Interval;

   --------------------------
   -- Get_Advertising_Type --
   --------------------------

   procedure Get_Advertising_Type
     (This   : in out HM11_Driver;
      Result : out Advertising_Type;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+ADTY?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Advertising_Type'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Advertising_Type;

   --------------------------
   -- Set_Advertising_Type --
   --------------------------

   procedure Set_Advertising_Type
     (This   : in out HM11_Driver;
      Value  : Advertising_Type;
      Status : out UART_Status)
   is
      C : constant String := Image (Advertising_Type'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+ADTY" & C, Ok_Set & C, Status);
   end Set_Advertising_Type;

   ---------------------------
   -- Get_White_List_Switch --
   ---------------------------

   procedure Get_White_List_Switch
     (This   : in out HM11_Driver;
      Result : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+ALLO?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := (if Character'Val (This.Responce (Ok_Get'Length + 1)) = '0'
                    then False
                    else True);
      end if;
   end Get_White_List_Switch;

   ---------------------------
   -- Set_White_List_Switch --
   ---------------------------

   procedure Set_White_List_Switch
     (This   : in out HM11_Driver;
      Value  : Boolean;
      Status : out UART_Status)
   is
      C : constant Character := (if Value then '1' else '0');
   begin
      Transmit_And_Check (This, "AT+ALLO" & C, Ok_Set & C, Status);
   end Set_White_List_Switch;

   --------------------------------
   -- Set_Battery_Monitor_Switch --
   --------------------------------

   procedure Set_Battery_Monitor_Switch
     (This   : in out HM11_Driver;
      Value  : Boolean;
      Status : out UART_Status)
   is
      C : constant Character := (if Value then '1' else '0');
   begin
      Transmit_And_Check (This, "AT+BATC" & C, Ok_Set & C, Status);
   end Set_Battery_Monitor_Switch;

   --------------------------------
   -- Get_Battery_Monitor_Switch --
   --------------------------------

   procedure Get_Battery_Monitor_Switch
     (This   : in out HM11_Driver;
      Result : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+BATC?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := (if Character'Val (This.Responce (Ok_Get'Length + 1)) = '0'
                    then False
                    else True);
      end if;
   end Get_Battery_Monitor_Switch;

   -----------------------------
   -- Set_Battery_Information --
   -----------------------------

   procedure Set_Battery_Information
     (This   : in out HM11_Driver;
      Value  : Persent;
      Status : out UART_Status)
   is
      Expect : constant String := "OK+BATT";
   begin
      Transmit_And_Check
        (This, "AT+BATT" & Image (Natural (Value)), Expect,
         This.Responce'Address, Expect'Length + 3, Status);
   end Set_Battery_Information;

   -------------------------------
   -- Query_Battery_Information --
   -------------------------------

   procedure Query_Battery_Information
     (This   : in out HM11_Driver;
      Result : out Persent;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+BATT?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 3, Status);

      if Status = Ok then
         declare
            S : String (1 .. Find_Zero (This, 1) - 1) with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Result := Persent'Value (S);
         end;
      end if;
   end Query_Battery_Information;

   ------------------------
   -- Set_UART_Baud_Rate --
   ------------------------

   procedure Set_UART_Baud_Rate
     (This   : in out HM11_Driver;
      Value  : UART_Baud_Rate;
      Status : out UART_Status)
   is
      C : constant String := Image (UART_Baud_Rate'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+BAUD" & C, Ok_Set & C, Status);
   end Set_UART_Baud_Rate;

   ------------------------
   -- Get_UART_Baud_Rate --
   ------------------------

   procedure Get_UART_Baud_Rate
     (This   : in out HM11_Driver;
      Result : out UART_Baud_Rate;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+BAUD?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := UART_Baud_Rate'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_UART_Baud_Rate;

   ------------------------------------------------
   -- Set_Minimum_Link_Layer_Connection_Interval --
   ------------------------------------------------

   procedure Set_Minimum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Value  : Link_Layer_Connection_Interval;
      Status : out UART_Status)
   is
      C : constant String := Image
        (Link_Layer_Connection_Interval'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+COMI" & C, Ok_Set & C, Status);
   end Set_Minimum_Link_Layer_Connection_Interval;

   ------------------------------------------------
   -- Get_Minimum_Link_Layer_Connection_Interval --
   ------------------------------------------------

   procedure Get_Minimum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Result : out Link_Layer_Connection_Interval;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+COMI?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Link_Layer_Connection_Interval'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Minimum_Link_Layer_Connection_Interval;

   ------------------------------------------------
   -- Set_Maximum_Link_Layer_Connection_Interval --
   ------------------------------------------------

   procedure Set_Maximum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Value  : Link_Layer_Connection_Interval;
      Status : out UART_Status)
   is
      C : constant String := Image
        (Link_Layer_Connection_Interval'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+COMA" & C, Ok_Set & C, Status);
   end Set_Maximum_Link_Layer_Connection_Interval;

   ------------------------------------------------
   -- Get_Maximum_Link_Layer_Connection_Interval --
   ------------------------------------------------

   procedure Get_Maximum_Link_Layer_Connection_Interval
     (This   : in out HM11_Driver;
      Result : out Link_Layer_Connection_Interval;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+COMA?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Link_Layer_Connection_Interval'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Maximum_Link_Layer_Connection_Interval;

   ---------------------------------------------
   -- Set_Link_Layer_Connection_Slave_Latency --
   ---------------------------------------------

   procedure Set_Link_Layer_Connection_Slave_Latency
     (This   : in out HM11_Driver;
      Value  : Layer_Connection_Latency;
      Status : out UART_Status)
   is
      C : constant String := Image (Natural (Value));
   begin
      Transmit_And_Check (This, "AT+COLA" & C, Ok_Set & C, Status);
   end Set_Link_Layer_Connection_Slave_Latency;

   ---------------------------------------------
   -- Get_Link_Layer_Connection_Slave_Latency --
   ---------------------------------------------

   procedure Get_Link_Layer_Connection_Slave_Latency
     (This   : in out HM11_Driver;
      Result : out Layer_Connection_Latency;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+COLA?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Layer_Connection_Latency'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Link_Layer_Connection_Slave_Latency;

   ----------------------------------------
   -- Set_Connection_Supervision_Timeout --
   ----------------------------------------

   procedure Set_Connection_Supervision_Timeout
     (This   : in out HM11_Driver;
      Value  : Connection_Supervision_Timeout;
      Status : out UART_Status)
   is
      C : constant String := Image
        (Connection_Supervision_Timeout'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+COSU" & C, Ok_Set & C, Status);
   end Set_Connection_Supervision_Timeout;

   ----------------------------------------
   -- Get_Connection_Supervision_Timeout --
   ----------------------------------------

   procedure Get_Connection_Supervision_Timeout
     (This   : in out HM11_Driver;
      Result : out Connection_Supervision_Timeout;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+COSU?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Connection_Supervision_Timeout'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Connection_Supervision_Timeout;

   ---------------------------
   -- Set_Update_Connection --
   ---------------------------

   procedure Set_Update_Connection
     (This   : in out HM11_Driver;
      Value  : Boolean;
      Status : out UART_Status)
   is
      C : constant Character := (if Value then '1' else '0');
   begin
      Transmit_And_Check (This, "AT+COUP" & C, Ok_Set & C, Status);
   end Set_Update_Connection;

   ---------------------------
   -- Get_Update_Connection --
   ---------------------------

   procedure Get_Update_Connection
     (This   : in out HM11_Driver;
      Result : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+COUP?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Result := (if S = '1' then True else False);
         end;
      end if;
   end Get_Update_Connection;

   ----------------------------------
   -- Clear_Last_Connected_Address --
   ----------------------------------

   procedure Clear_Last_Connected_Address
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+CLEAR", "OK+CLEAR", Status);
   end Clear_Last_Connected_Address;

   -------------------------
   -- Connect_Last_Device --
   -------------------------

   procedure Connect_Last_Device
     (This   : in out HM11_Driver;
      Result : out Connect_Result;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+CONNL", When_Connected_Message,
         This.Responce'Address, When_Connected_Message'Length + 1, Status);

      if Status = Ok then
         declare
            C : Character with Import,
              Address => This.Responce
                (When_Connected_Message'Length + 1)'Address;
         begin
            Result := To_Connect_Result (C);
         end;
      end if;
   end Connect_Last_Device;

   ---------------------
   -- Get_MAC_Address --
   ---------------------

   procedure Get_MAC_Address
     (This   : in out HM11_Driver;
      Result : out MAC_Address;
      Status : out UART_Status)
   is
      Expect : constant String := "OK+ADDR:";
   begin
      Transmit_And_Check
        (This, "AT+ADDR?", Expect,
         This.Responce'Address, Expect'Length + MAC_Address'Length, Status);

      if Status = Ok then
         declare
            MAC : MAC_Address with Import,
              Address => This.Responce (Expect'Length + 1)'Address;
         begin
            Result := MAC;
         end;
      end if;
   end Get_MAC_Address;

   ----------------------------------
   -- Set_White_List_MAC_Addresses --
   ----------------------------------

   procedure Set_White_List_MAC_Addresses
     (This   : in out HM11_Driver;
      Index  : MAC_White_List_Index;
      Value  : MAC_Address;
      Status : out UART_Status)
   is
      Local : MAC_Address := Value;
      M     : String (Local'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+AD" & Image (Natural (Index)) & M, Ok_Set & M, Status);
   end Set_White_List_MAC_Addresses;

   --------------------------------
   -- Get_White_List_MAC_Address --
   --------------------------------

   procedure Get_White_List_MAC_Address
     (This   : in out HM11_Driver;
      Index  : MAC_White_List_Index;
      Result : out MAC_Address;
      Status : out UART_Status)
   is
      Num    : constant String := Image (Natural (Index));
      Expect : constant String := "OK+AD" & Num & "?:";
   begin
      Transmit_And_Check
        (This, "AT+AD" & Num & "??", Expect,
         This.Responce'Address, Expect'Length + MAC_Address'Length, Status);

      if Status = Ok then
         declare
            S : MAC_Address with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Result := S;
         end;
      end if;
   end Get_White_List_MAC_Address;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (This     : in out HM11_Driver;
      MAC_Type : MAC_Address_Type;
      Address  : MAC_Address;
      Result   : out Connect_Result;
      Status   : out UART_Status)
   is

      T      : constant String := Image (MAC_Type);
      Local  : MAC_Address := Address;
      A      : String (Local'Range) with Import,
        Address => Local (Local'First)'Address;
      Expect : constant String := "OK+CO" & T & T;

   begin
      Result := Other_Error;
      Transmit_And_Check
        (This, "AT+CO"  & T & A, Expect,
         This.Responce'Address, Expect'Length + 1, Status);

      if Status = Ok then
         declare
            C : Character with Import,
              Address => This.Responce (Expect'Length + 1)'Address;
         begin
            Result := To_Connect_Result (C);
         end;
      end if;
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (This   : in out HM11_Driver;
      Index  : Discovered_Index;
      Result : out Connect_Result;
      Status : out UART_Status) is
   begin
      Result := Other_Error;
      Transmit_And_Check
        (This     => This,
         Command  => "AT+CONN" & Image (Natural (Index)),
         Expect   => When_Connected_Message,
         Received => This.Responce'Address,
         Length   => When_Connected_Message'Length + 1,
         Status   => Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce
                (When_Connected_Message'Length + 1)'Address;
         begin
            Result := To_Connect_Result (S);
         end;
      end if;
   end Connect;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (This     : in out HM11_Driver;
      Callback : Discovered_Callback;
      Timeout  : Natural;
      Status   : out UART_Status)
   is
      OK_DISCS   : constant String := "OK+DISCS";
      OK_DISC    : constant String := "OK+DISC:";
      OK_NAME    : constant String := "OK+NAME:";
      OK_RSSI    : constant String := "OK+RSSI:";
      OK_DISCE   : constant String := "OK+DISCE";
      Min_Prefix : constant := 8;

      procedure Call;
      --  Call Callback with discovered info

      ----------
      -- Call --
      ----------

      procedure Call is
      begin
         if This.Discovered_Info /= Null_Discovered_Info then
            Callback
              (Id   => This.Discovered_Info.Id,
               MAC  => This.Discovered_Info.MAC,
               Name => This.Discovered_Info.Name.Value
                 (1 .. This.Discovered_Info.Name.Last),
               RSSI => This.Discovered_Info.RSSI.Value
                 (1 .. This.Discovered_Info.RSSI.Length));

            This.Discovered_Info := Null_Discovered_Info;
            This.Discovered_Info.Name.Last := 0;
            This.Discovered_Info.RSSI.Last := 0;
         end if;
      end Call;

      Pos           : Positive := 1;
      Zero          : Positive;
      Stream_Closed : Boolean := False;
      Length        : Natural;
      EON           : Natural;
      Kind          : Scan_Stage_Kind := Selection;

   begin
      This.Discovered_Info := Null_Discovered_Info;

      Transmit (This, "AT+DISC?", Status);
      if Status /= Ok then
         return;
      end if;

      This.Receive
        (This.Port, This.Responce'Address, This.Responce'Length,
         Status, Timeout, As_Stream => True);

      Main : loop
         This.Readed_Position (Stream_Closed, Zero);
         exit Main when Stream_Closed;
         Length := Calc_Lenght (This, Pos, Zero);

         loop
            exit when Length = 0;

            --  End message, exit
            exit Main when Start_With (This, OK_DISCE, Pos, Zero - 1);

            case Kind is
               when Selection =>
                  if Length < Min_Prefix then
                     --  Don't have enought characters
                     exit;
                  end if;

                  if Start_With (This, OK_DISCS, Pos, Zero - 1) then
                     Move (This, Pos, OK_DISCS'Length, Zero, Length);
                  end if;

                  if Start_With (This, OK_DISC, Pos, Zero - 1) then
                     Move (This, Pos, OK_DISC'Length, Zero, Length);
                     Kind := MAC;
                  end if;

                  if Start_With (This, OK_NAME, Pos, Zero - 1) then
                     Move (This, Pos, OK_NAME'Length, Zero, Length);
                     Kind := Name;
                  end if;

                  if Start_With (This, OK_RSSI, Pos, Zero - 1) then
                     Move (This, Pos, OK_RSSI'Length, Zero, Length);
                     Kind := RSSI;
                  end if;

               when MAC =>
                  Call; --  Call the callback if we have previous data

                  if Length < MAC_Address'Length then
                     exit;

                  else
                     declare
                        Value : MAC_Address with Import,
                          Address => This.Responce (Pos)'Address;
                     begin
                        This.Discovered_Info.MAC := Value;
                     end;
                     Move (This, Pos, MAC_Address'Length, Zero, Length);
                     Kind := Selection;
                  end if;

               when Name =>
                  EON := Find (This, ASCII.CR & ASCII.LF, Pos, Zero - 1);
                  if EON = 0 then
                     Append (This, This.Discovered_Info.Name, Pos, Zero - 1);
                     Pos := Zero;

                  else
                     Append (This, This.Discovered_Info.Name, Pos, EON - 1);
                     Pos  := EON + 1;
                     Kind := Selection;
                  end if;

               when RSSI =>
                  EON := Find (This, ASCII.CR & ASCII.LF, Pos, Zero - 1);
                  if EON = 0 then
                     Append (This, This.Discovered_Info.RSSI, Pos, Zero - 1);
                     Pos := Zero;

                  else
                     Append (This, This.Discovered_Info.RSSI, Pos, EON - 1);
                     Pos  := EON + 1;
                     Kind := Selection;
                  end if;
            end case;
         end loop;
      end loop Main;

      Call;

      if not Stream_Closed then
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
      end if;

   exception
      when others =>
         --  Stop stream
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
         Status := Err_Error;
   end Scan;

   ------------------
   -- Scan_iBeacon --
   ------------------

   procedure Scan_iBeacon
     (This     : in out HM11_Driver;
      Callback : Discovered_iBeacon_Callback;
      Timeout  : Natural;
      Status   : out UART_Status)
   is
      OK_DISCS : constant String := "OK+DISCS";
      OK_DISC  : constant String := "OK+DISC";
      OK_DISCE : constant String := "OK+DISCE";

      iBeacon_Data : UART_Data_8b (1 .. Discovered_iBeacon_Lenght);
      iBeacon      : String (iBeacon_Data'Range) with Import,
        Address => iBeacon_Data'Address;

      Pos           : Positive := 1;
      Zero          : Positive;
      Stream_Closed : Boolean := False;
      Length        : Natural;

   begin
      Transmit (This, "AT+DISI?", Status);

      if Status /= Ok then
         return;
      end if;

      This.Receive
        (This.Port, This.Responce'Address, This.Responce'Length,
         Status, Timeout, As_Stream => True);

      Main : loop
         This.Readed_Position (Stream_Closed, Zero);
         exit Main when Stream_Closed;
         Length := Calc_Lenght (This, Pos, Zero);

         loop
            exit when Length < OK_DISCS'Length;

            --  End message, exit
            exit Main when Start_With (This, OK_DISCE, Pos, Zero - 1);

            if Start_With (This, OK_DISCS, Pos, Zero - 1) then
               Move (This, Pos, OK_DISCS'Length, Zero, Length);
            end if;

            if Start_With (This, OK_DISC, Pos, Zero - 1)
              and then Length >= OK_DISC'Length + Discovered_iBeacon_Lenght
            then
               Copy (This, iBeacon_Data, Pos + OK_DISC'Length - 1);

               Move (This, Pos, OK_DISCS'Length + Discovered_iBeacon_Lenght,
                     Zero, Length);

               Callback (iBeacon);
            end if;
         end loop;
      end loop Main;

      if not Stream_Closed then
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
      end if;

   exception
      when others =>
         --  Stop stream
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
         Status := Err_Error;
   end Scan_iBeacon;

   -----------------------------
   -- Set_iBeacon_Deploy_Mode --
   -----------------------------

   procedure Set_iBeacon_Deploy_Mode
     (This   : in out HM11_Driver;
      Value  : iBeacon_Deploy_Mode;
      Status : out UART_Status)
   is
      V : constant String := Image
        (Natural (iBeacon_Deploy_Mode'Pos (Value)) + 1);
   begin
      Transmit_And_Check (This, "AT+DELO" & V, "OK+DELO" & V, Status);
   end Set_iBeacon_Deploy_Mode;

   -----------------------------
   -- Remove_Bond_Information --
   -----------------------------

   procedure Remove_Bond_Information
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+ERASE", "OK+ERASE", Status);
   end Remove_Bond_Information;

   ----------------------------
   -- Find_All_Services_UUID --
   ----------------------------

   procedure Find_All_Services_UUID
     (This     : in out HM11_Driver;
      Callback : Service_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status)
   is
      use type HAL.UInt8;

      Header  : constant String (1 .. 56) := (others => '*');
      Started : Boolean := False;

      UUID_Data : UART_Data_8b (1 .. 14);
      UUID      : String (UUID_Data'Range) with Import,
        Address => UUID_Data'Address;

      Pos           : Positive := 1;
      Zero          : Positive;
      Stream_Closed : Boolean := False;
      Length        : Natural;
   begin

      Transmit (This, "AT+FINDSERVICES?", Status);
      if Status /= Ok then
         return;
      end if;

      This.Receive
        (This.Port, This.Responce'Address, This.Responce'Length,
         Status, Timeout, As_Stream => True);

      Main : loop
         This.Readed_Position (Stream_Closed, Zero);
         exit Main when Stream_Closed;
         Length := Calc_Lenght (This, Pos, Zero);

         loop
            exit when Length < UUID_Data'Length;

            if This.Responce (Pos) = Character'Pos ('*') then
               if Start_With (This, Header, Pos, Zero - 1) then
                  if Started then
                     --  End message, exit
                     exit Main;
                  else

                     --  Start message
                     Started := True;
                     Move (This, Pos, Header'Length, Zero, Length);
                  end if;
               end if;

            elsif Start_With (This, ASCII.CR & ASCII.LF, Pos, Zero - 1) then
               Move (This, Pos, 2, Zero, Length);

            else
               Copy (This, UUID_Data, Pos);

               Move (This, Pos, UUID_Data'Length, Zero, Length);

               Callback (UUID);
            end if;
         end loop;
      end loop Main;

      if not Stream_Closed then
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
      end if;

   exception
      when others =>
         --  Stop stream
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
         Status := Err_Error;
   end Find_All_Services_UUID;

   ----------------------------------
   -- Find_All_Characteristic_UUID --
   ----------------------------------

   procedure Find_All_Characteristic_UUID
     (This     : in out HM11_Driver;
      Callback : Characteristic_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status) is
   begin
      Transmit (This, "AT+FINDALLCHARS?", Status);
      if Status = Ok then
         Read_Characteristic_UUID (This, Callback, Timeout, Status);
      end if;
   end Find_All_Characteristic_UUID;

   ------------------------------
   -- Find_Characteristic_UUID --
   ------------------------------

   procedure Find_Characteristic_UUID
     (This     : in out HM11_Driver;
      From     : Handle_Type;
      To       : Handle_Type;
      Callback : Characteristic_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status)
   is
      Local_From : Handle_Type := From;
      F          : String (Handle_Type'Range) with Import,
        Address => Local_From (Local_From'First)'Address;
      Local_To   : Handle_Type := To;
      T          : String (Handle_Type'Range) with Import,
        Address => Local_To (Local_To'First)'Address;

   begin
      Transmit (This, "AT+CHAR" & F & T & "?", Status);
      if Status = Ok then
         Read_Characteristic_UUID (This, Callback, Timeout, Status);
      end if;
   end Find_Characteristic_UUID;

   ------------------------------
   -- Read_Characteristic_UUID --
   ------------------------------

   procedure Read_Characteristic_UUID
     (This     : in out HM11_Driver;
      Callback : Characteristic_UUID_Callback;
      Timeout  : Natural;
      Status   : out UART_Status)
   is
      use type HAL.UInt8;

      Header  : constant String (1 .. 56) := (others => '*');
      Started : Boolean := False;

      UUID_Data : UART_Data_8b (1 .. 24);
      UUID      : String (UUID_Data'Range) with Import,
        Address => UUID_Data'Address;

      Pos           : Positive := 1;
      Zero          : Positive;
      Stream_Closed : Boolean := False;
      Length        : Natural;

   begin
      This.Receive
        (This.Port, This.Responce'Address, This.Responce'Length,
         Status, Timeout, As_Stream => True);

      Main : loop
         This.Readed_Position (Stream_Closed, Zero);
         exit Main when Stream_Closed;
         Length := Calc_Lenght (This, Pos, Zero);

         loop
            exit when Length < UUID_Data'Length;

            if This.Responce (Pos) = Character'Pos ('*') then
               if Start_With (This, Header, Pos, Zero - 1) then
                  if Started then
                     --  End message, exit
                     exit Main;
                  else
                     --  Start message
                     Started := True;
                     Move (This, Pos, Header'Length, Zero, Length);
                  end if;
               end if;

            elsif Start_With (This, ASCII.CR & ASCII.LF, Pos, Zero - 1) then
               Move (This, Pos, 2, Zero, Length);

            else
               Copy (This, UUID_Data, Pos);

               Move (This, Pos, UUID_Data'Length, Zero, Length);

               Callback (UUID);
            end if;
         end loop;
      end loop Main;

      if not Stream_Closed then
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
      end if;

   exception
      when others =>
         --  Stop stream
         This.Receive
           (This.Port, This.Responce'Address, This.Responce'Length, Status);
         Status := Err_Error;
   end Read_Characteristic_UUID;


   ----------------------------------
   -- Enable_Characteristic_Notify --
   ----------------------------------

   procedure Enable_Characteristic_Notify
     (This     : in out HM11_Driver;
      Handle   : Handle_Type;
      Status   : out UART_Status;
      Responce : out Notify_Responce) is
   begin
      Transmit (This, "AT+NOTIFY_ON" & To_String (Handle), Status);
      Read_Characteristic_Notify_Responce (This, Status, Responce);
   end Enable_Characteristic_Notify;

   -----------------------------------
   -- Disable_Characteristic_Notify --
   -----------------------------------

   procedure Disable_Characteristic_Notify
     (This     : in out HM11_Driver;
      Handle   : Handle_Type;
      Status   : out UART_Status;
      Responce : out Notify_Responce) is
   begin
      Transmit (This, "AT+NOTIFYOFF" & To_String (Handle), Status);
      Read_Characteristic_Notify_Responce (This, Status, Responce);
   end Disable_Characteristic_Notify;

   --------------------------------
   -- Read_Characteristic_Notify --
   --------------------------------

   procedure Read_Characteristic_Notify
     (This     : in out HM11_Driver;
      Handle   : Handle_Type;
      Status   : out UART_Status;
      Responce : out Notify_Responce) is
   begin
      Transmit (This, "AT+READDATA" & To_String (Handle), Status);
      Read_Characteristic_Notify_Responce (This, Status, Responce);
   end Read_Characteristic_Notify;

   -----------------------------------------
   -- Read_Characteristic_Notify_Responce --
   -----------------------------------------

   procedure Read_Characteristic_Notify_Responce
     (This     : in out HM11_Driver;
      Status   : in out UART_Status;
      Responce : out Notify_Responce)
   is
      Send_Ok : constant String := "OK+SEND-OK";
      Data_Er : constant String := "OK+DATA-ER";
      Data    : UART_Data_8b (1 .. Send_Ok'Length);

   begin
      if Status = Ok then
         This.Port.Receive (Data, Status);

         if Status = Ok then
            declare
               S : String (Data'Range) with Import,
                 Address => Data (Data'First)'Address;
            begin
               if S = Send_Ok then
                  Responce := HM11.Send_Ok;
               elsif S = Data_Er then
                  Responce := HM11.Data_Er;
               else
                  Responce := HM11.Send_Er;
               end if;
            end;
         end if;
      end if;
   end Read_Characteristic_Notify_Responce;

   ------------------------------------------
   -- Set_Method_And_Characteristic_Handle --
   ------------------------------------------

   procedure Set_Method_And_Characteristic_Handle
     (This   : in out HM11_Driver;
      Handle : Handle_Type;
      Method : Send_Data_Method;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+SET_WAY" & Image (Method) & To_String (Handle),
         "OK+SEND-OK", Status);
   end Set_Method_And_Characteristic_Handle;

   ---------------------------------
   -- Send_Data_To_Characteristic --
   ---------------------------------

   procedure Send_Data_To_Characteristic
     (This   : in out HM11_Driver;
      Handle : Handle_Type;
      Method : Send_Data_Characteristic;
      Data   : UART_Data_8b;
      Status : out UART_Status) is
   begin
      Transmit
        (This, "AT+SEND_DATA" & Image (Method) & To_String (Handle),
         Data, Status);
   end Send_Data_To_Characteristic;

   ---------------------------------------
   -- Set_Use_Characteristic_UUID_Count --
   ---------------------------------------

   procedure Set_Use_Characteristic_UUID_Count
     (This   : in out HM11_Driver;
      Count  : Characteristic_UUID_Count;
      Status : out UART_Status)
   is
      I : constant String :=
        (if Count = Query
         then "?"
         else Image (Characteristic_UUID_Count'Pos (Count)));
   begin
      Transmit_And_Check (This, "AT+FFE2" & I, Ok_Set & I, Status);
   end Set_Use_Characteristic_UUID_Count;

   --------------------------
   -- Set_Advertising_FLAG --
   --------------------------

   procedure Set_Advertising_FLAG
     (This   : in out HM11_Driver;
      Flag   : Advertising_FLAG;
      Status : out UART_Status)
   is
      Local_Flag : constant Advertising_FLAG := Flag;
      S          : constant String (Advertising_FLAG'Range) with Import,
        Address => Local_Flag (Local_Flag'First)'Address;
   begin
      Transmit_And_Check (This, "AT+FLAG" & S, Ok_Set & S, Status);
   end Set_Advertising_FLAG;

   ----------------------------------
   -- Set_UART_Flow_Control_Switch --
   ----------------------------------

   procedure Set_UART_Flow_Control_Switch
     (This   : in out HM11_Driver;
      Switch : Boolean;
      Status : out UART_Status)
   is
      S : constant String := Image (Switch);
   begin
      Transmit_And_Check (This, "AT+FIOW" & S, Ok_Set & S, Status);
   end Set_UART_Flow_Control_Switch;

   ----------------------------------
   -- Get_UART_Flow_Control_Switch --
   ----------------------------------

   procedure Get_UART_Flow_Control_Switch
     (This   : in out HM11_Driver;
      Switch : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+FIOW?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Switch := S = '1';
         end;
      end if;
   end Get_UART_Flow_Control_Switch;

   ------------------------
   -- Set_Module_RX_Gain --
   ------------------------

   procedure Set_Module_RX_Gain
     (This   : in out HM11_Driver;
      Gain   : RX_Gain;
      Status : out UART_Status)
   is
      S : constant String := Image (RX_Gain'Pos (Gain));
   begin
      Transmit_And_Check (This, "AT+GAIN" & S, Ok_Set & S, Status);
   end Set_Module_RX_Gain;

   ------------------------
   -- Get_Module_RX_Gain --
   ------------------------

   procedure Get_Module_RX_Gain
     (This   : in out HM11_Driver;
      Gain   : out RX_Gain;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+GAIN?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Gain := RX_Gain'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Module_RX_Gain;

   ----------------------------------------------
   -- Set_Humi_Information_Byte_In_Advertising --
   ----------------------------------------------

   procedure Set_Humi_Information_Byte_In_Advertising
     (This   : in out HM11_Driver;
      Info   : Humi_Information;
      Status : out UART_Status)
   is
      Local_Info : constant Humi_Information := Info;
      S          : constant String (Humi_Information'Range) with Import,
        Address => Local_Info (Local_Info'First)'Address;
   begin
      Transmit_And_Check (This, "AT+HUMI" & S, Ok_Set & S, Status);
   end Set_Humi_Information_Byte_In_Advertising;

   -------------------
   -- Set_Work_Type --
   -------------------

   procedure Set_Work_Type
     (This   : in out HM11_Driver;
      Work   : Work_Type;
      Status : out UART_Status)
   is
      S : constant String := Image (Work_Type'Pos (Work));
   begin
      Transmit_And_Check (This, "AT+IMME" & S, Ok_Set & S, Status);
   end Set_Work_Type;

   -------------------
   -- Get_Work_Type --
   -------------------

   procedure Get_Work_Type
     (This   : in out HM11_Driver;
      Work   : out Work_Type;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+IMME?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Work := Work_Type'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Work_Type;

   ------------------------
   -- Set_iBeacon_Switch --
   ------------------------

   procedure Set_iBeacon_Switch
     (This   : in out HM11_Driver;
      Switch : Boolean;
      Status : out UART_Status)
   is
      S : constant String := Image (Switch);
   begin
      Transmit_And_Check (This, "AT+IBEA" & S, Ok_Set & S, Status);
   end Set_iBeacon_Switch;

   ------------------------
   -- Get_iBeacon_Switch --
   ------------------------

   procedure Get_iBeacon_Switch
     (This   : in out HM11_Driver;
      Switch : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+IBEA?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Switch := S = '1';
         end;
      end if;
   end Get_iBeacon_Switch;

   ----------------------
   -- Set_iBeacon_UUID --
   ----------------------

   procedure Set_iBeacon_UUID
     (This   : in out HM11_Driver;
      UUID   : iBeacon_UUID;
      Status : out UART_Status)
   is
      Local_UUID : constant iBeacon_UUID := UUID;

      procedure Set (Pos : Natural);
      procedure Set (Pos : Natural)
      is
         I : constant String := Image (Pos);
         S : String (1 .. 8) with Import,
           Address => Local_UUID (Pos * 8 + 1)'Address;
      begin
         Transmit_And_Check
           (This, "AT+IBE" & I & "0x" & S, Ok_Set & I & "0x" & S, Status);
      end Set;

   begin
      for I in 0 .. 3 loop
         Set (I);
         exit when Status /= Ok;
      end loop;
   end Set_iBeacon_UUID;

   ----------------------
   -- Get_iBeacon_UUID --
   ----------------------

   procedure Get_iBeacon_UUID
     (This   : in out HM11_Driver;
      UUID   : out iBeacon_UUID;
      Status : out UART_Status)
   is
      Local_UUID : String (iBeacon_UUID'Range);

      procedure Get (Pos : Natural);
      procedure Get (Pos : Natural)
      is
         I : constant String := Image (Pos);
      begin
         Transmit_And_Check
           (This, "AT+IBE" & I & "?", Ok_Get,
            This.Responce'Address, Ok_Get'Length + 10, Status);

         if Status = Ok then
            declare
               S : String (1 .. 8) with Import,
                 Address => This.Responce (Ok_Get'Length + 3)'Address;
            begin
               Local_UUID (Pos * 8 + 1 .. Pos * 8 + 8) := S;
            end;
         end if;
      end Get;

   begin
      for I in 0 .. 3 loop
         Get (I);
         exit when Status /= Ok;
      end loop;

      declare
         L : iBeacon_UUID with Import,
           Address => Local_UUID (Local_UUID'First)'Address;
      begin
         UUID := L;
      end;
   end Get_iBeacon_UUID;

   --------------------------------
   -- Set_iBeacon_Marjor_Version --
   --------------------------------

   procedure Set_iBeacon_Marjor_Version
     (This    : in out HM11_Driver;
      Version : Version_Type;
      Status  : out UART_Status)
   is
      S : String (Version_Type'Range) with Import,
        Address => Version (Version'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+MARJ0x" & S, Ok_Set & "0x" & S, Status);
   end Set_iBeacon_Marjor_Version;

   --------------------------------
   -- Get_iBeacon_Marjor_Version --
   --------------------------------

   procedure Get_iBeacon_Marjor_Version
     (This    : in out HM11_Driver;
      Version : out Version_Type;
      Status  : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+MARJ?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + Version_Type'Length + 2, Status);

      if Status = Ok then
         declare
            S : Version_Type with Import,
              Address => This.Responce (Ok_Get'Length + 3)'Address;
         begin
            Version := S;
         end;
      end if;
   end Get_iBeacon_Marjor_Version;

   -------------------------------
   -- Set_iBeacon_Minor_Version --
   -------------------------------

   procedure Set_iBeacon_Minor_Version
     (This    : in out HM11_Driver;
      Version : Version_Type;
      Status  : out UART_Status)
   is
      S : String (Version_Type'Range) with Import,
        Address => Version (Version'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+MINO0x" & S, Ok_Set & "0x" & S, Status);
   end Set_iBeacon_Minor_Version;

   -------------------------------
   -- Get_iBeacon_Minor_Version --
   -------------------------------

   procedure Get_iBeacon_Minor_Version
     (This    : in out HM11_Driver;
      Version : out Version_Type;
      Status  : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+MINO?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + Version_Type'Length + 2, Status);

      if Status = Ok then
         declare
            S : Version_Type with Import,
              Address => This.Responce (Ok_Get'Length + 3)'Address;
         begin
            Version := S;
         end;
      end if;
   end Get_iBeacon_Minor_Version;

   --------------------------------
   -- Set_iBeacon_Measured_Power --
   --------------------------------

   procedure Set_iBeacon_Measured_Power
     (This   : in out HM11_Driver;
      Power  : Measured_Power;
      Status : out UART_Status)
   is
      S : String (Measured_Power'Range) with Import,
        Address => Power (Power'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+MEAS0x" & S, Ok_Set & "0x" & S, Status);
   end Set_iBeacon_Measured_Power;

   --------------------------------
   -- Get_iBeacon_Measured_Power --
   --------------------------------

   procedure Get_iBeacon_Measured_Power
     (This   : in out HM11_Driver;
      Power  : out Measured_Power;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+MEAS?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + Measured_Power'Length + 2, Status);

      if Status = Ok then
         declare
            S : Measured_Power with Import,
              Address => This.Responce (Ok_Get'Length + 3)'Address;
         begin
            Power := S;
         end;
      end if;
   end Get_iBeacon_Measured_Power;

   -------------------
   -- Set_Work_Mode --
   -------------------

   procedure Set_Work_Mode
     (This   : in out HM11_Driver;
      Mode   : Work_Mode;
      Status : out UART_Status)
   is
      S : constant String := Image (Work_Mode'Pos (Mode));
   begin
      Transmit_And_Check (This, "AT+MODE" & S, Ok_Set & S, Status);
   end Set_Work_Mode;

   -------------------
   -- Get_Work_Mode --
   -------------------

   procedure Get_Work_Mode
     (This   : in out HM11_Driver;
      Mode   : out Work_Mode;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+MODE?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Mode := Work_Mode'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Work_Mode;

   ----------------------------
   -- Set_Notify_Information --
   ----------------------------

   procedure Set_Notify_Information
     (This   : in out HM11_Driver;
      Notify : Boolean;
      Status : out UART_Status)
   is
      S : constant String := Image (Notify);
   begin
      Transmit_And_Check (This, "AT+NOTI" & S, Ok_Set & S, Status);
   end Set_Notify_Information;

   ----------------------------
   -- Get_Notify_Information --
   ----------------------------

   procedure Get_Notify_Information
     (This   : in out HM11_Driver;
      Notify : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+NOTI?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Notify := S = '1';
         end;
      end if;
   end Get_Notify_Information;

   ---------------------
   -- Set_Notify_Mode --
   ---------------------

   procedure Set_Notify_Mode
     (This   : in out HM11_Driver;
      Mode   : Notify_Mode;
      Status : out UART_Status)
   is
      S : constant String := Image (Notify_Mode'Pos (Mode));
   begin
      Transmit_And_Check (This, "AT+NOTP" & S, Ok_Set & S, Status);
   end Set_Notify_Mode;

   ---------------------
   -- Get_Notify_Mode --
   ---------------------

   procedure Get_Notify_Mode
     (This   : in out HM11_Driver;
      Mode   : out Notify_Mode;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+NOTP?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Mode := Notify_Mode'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Notify_Mode;

   ---------------------
   -- Set_Module_Name --
   ---------------------

   procedure Set_Module_Name
     (This   : in out HM11_Driver;
      Name   : String;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+NAME" & Name, Ok_Set & Name, Status);
   end Set_Module_Name;

   ---------------------
   -- Get_Module_Name --
   ---------------------

   function Get_Module_Name
     (This : in out HM11_Driver)
      return String
   is
      Status   : UART_Status;
      Expect   : constant String := "OK+NAME:";
      Str      : String (1 .. 12) with Import,
        Address => This.Responce (Expect'Length + 1)'Address;
   begin
      Transmit_And_Check
        (This, "AT+NAME?", Expect,
         This.Responce'Address, Expect'Length + 12, Status);

      if Status = Ok then
         return Str
           (1 .. Natural'Min
              (12, Find_Zero (This, Expect'Length + 1) - 1 - Expect'Length));
      else
         return "";
      end if;
   end Get_Module_Name;

   ----------------------
   -- Set_Output_Power --
   ----------------------

   procedure Set_Output_Power
     (This   : in out HM11_Driver;
      Power  : Output_Power;
      Status : out UART_Status)
   is
      S : constant String := Image (Output_Power'Pos (Power));
   begin
      Transmit_And_Check (This, "AT+PCTL" & S, Ok_Set & S, Status);
   end Set_Output_Power;

   ----------------------
   -- Get_Output_Power --
   ----------------------

   procedure Get_Output_Power
     (This   : in out HM11_Driver;
      Power  : out Output_Power;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+PCTL?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Power := Output_Power'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Output_Power;

   --------------------
   -- Set_Parity_Bit --
   --------------------

   procedure Set_Parity_Bit
     (This   : in out HM11_Driver;
      Parity : Parity_Bit;
      Status : out UART_Status)
   is
      S : constant String := Image (Parity_Bit'Pos (Parity));
   begin
      Transmit_And_Check (This, "AT+PARI" & S, Ok_Set & S, Status);
   end Set_Parity_Bit;

   --------------------
   -- Get_Parity_Bit --
   --------------------

   procedure Get_Parity_Bit
     (This   : in out HM11_Driver;
      Parity : out Parity_Bit;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+PARI?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Parity := Parity_Bit'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Parity_Bit;

   ---------------------------
   -- Set_PIO_Output_Status --
   ---------------------------

   procedure Set_PIO_Output_Status
     (This   : in out HM11_Driver;
      PIO    : PIO_Number;
      Output : PIO_Output;
      Status : out UART_Status)
   is
      I      : constant String := Image (PIO);
      S      : constant String := Image (PIO_Output'Pos (Output));
      Expect : constant String := "OK+PIO" & I & ":";
   begin
      if PIO = 1 then
         Transmit_And_Check (This, "AT+PIO1" & S, Ok_Set & S, Status);
      else
         Transmit_And_Check (This, "AT+PIO" & I & S, Expect & S, Status);
      end if;
   end Set_PIO_Output_Status;

   ---------------------------
   -- Get_PIO_Output_Status --
   ---------------------------

   procedure Get_PIO_Output_Status
     (This   : in out HM11_Driver;
      PIO    : PIO_Number;
      Output : out PIO_Output;
      Status : out UART_Status)
   is
      I      : constant String := Image (PIO);
      Expect : constant String := "OK+PIO" & I & ":";
   begin
      if PIO = 1 then
         Transmit_And_Check
           (This, "AT+PIO1?", Ok_Get,
            This.Responce'Address, Ok_Get'Length + 1, Status);

         if Status = Ok then
            Output := PIO_Output'Val
              (Value (This.Responce (Ok_Get'Length + 1)));
         end if;

      else
         Transmit_And_Check
           (This, "AT+PIO" & I & "?", Expect,
            This.Responce'Address, Expect'Length + 1, Status);

         if Status = Ok then
            Output := PIO_Output'Val
              (Value (This.Responce (Expect'Length + 1)));
         end if;
      end if;
   end Get_PIO_Output_Status;

   ----------------------------
   -- Get_PIOs_Output_Status --
   ----------------------------

   procedure Get_PIOs_Output_Status
     (This   : in out HM11_Driver;
      PIOs   : out PIO_Numbers;
      Status : out UART_Status)
   is
      Expect : constant String := "OK+PIO?";
      PIO    : PIO_Numbers with Import,
        Address => This.Responce (Expect'Length + 1)'Address;
   begin
      Transmit_And_Check
        (This, "AT+PIO??", Expect,
         This.Responce'Address, Expect'Length + 3, Status);

      if Status = Ok then
         PIOs := PIO;
      end if;
   end Get_PIOs_Output_Status;

   ----------------------------
   -- Set_PIOs_Output_Status --
   ----------------------------

   procedure Set_PIOs_Output_Status
     (This   : in out HM11_Driver;
      PIOs   : PIO_Numbers;
      Status : out UART_Status)
   is
      Local : constant PIO_Numbers := PIOs;
      S     : constant String (PIO_Numbers'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check (This, "AT+MPIO" & S, Ok_Set & S, Status);
   end Set_PIOs_Output_Status;

   ------------------
   -- Set_PIN_Code --
   ------------------

   procedure Set_PIN_Code
     (This   : in out HM11_Driver;
      Pin    : PIN_Type;
      Status : out UART_Status)
   is
      Local : constant PIN_Type := Pin;
      S     : constant String (PIN_Type'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check (This, "AT+PASS" & S, Ok_Set & S, Status);
   end Set_PIN_Code;

   ------------------
   -- Get_PIN_Code --
   ------------------

   procedure Get_PIN_Code
     (This   : in out HM11_Driver;
      Pin    : out PIN_Type;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+PASS?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + PIN_Type'Length, Status);

      if Status = Ok then
         declare
            L : PIN_Type with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Pin := L;
         end;
      end if;
   end Get_PIN_Code;

   ----------------------
   -- Set_Module_Power --
   ----------------------

   procedure Set_Module_Power
     (This   : in out HM11_Driver;
      Power  : Module_Power;
      Status : out UART_Status)
   is
      S : constant String := Image (Module_Power'Pos (Power));
   begin
      Transmit_And_Check (This, "AT+POWE" & S, Ok_Set & S, Status);
   end Set_Module_Power;

   ----------------------
   -- Get_Module_Power --
   ----------------------

   procedure Get_Module_Power
     (This   : in out HM11_Driver;
      Power  : out Module_Power;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+POWE?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Power := Module_Power'Val (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Module_Power;

   ---------------------------
   -- Set_Module_Auto_Sleep --
   ---------------------------

   procedure Set_Module_Auto_Sleep
     (This   : in out HM11_Driver;
      Sleep  : Boolean;
      Status : out UART_Status)
   is
      S : constant String := Image (not Sleep);
   begin
      Transmit_And_Check (This, "AT+PWRM" & S, Ok_Set & S, Status);
   end Set_Module_Auto_Sleep;

   ---------------------------
   -- Get_Module_Auto_Sleep --
   ---------------------------

   procedure Get_Module_Auto_Sleep
     (This   : in out HM11_Driver;
      Sleep  : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+PWRM?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Sleep := S = '0';
         end;
      end if;
   end Get_Module_Auto_Sleep;

   -----------------------------------
   -- Set_Reliable_Advertising_Mode --
   -----------------------------------

   procedure Set_Reliable_Advertising_Mode
     (This   : in out HM11_Driver;
      Mode   : Advertising_Mode;
      Status : out UART_Status)
   is
      S : constant String := Image (Advertising_Mode'Pos (Mode));
   begin
      Transmit_And_Check (This, "AT+RELI" & S, Ok_Set & S, Status);
   end Set_Reliable_Advertising_Mode;

   -----------------------------------
   -- Get_Reliable_Advertising_Mode --
   -----------------------------------

   procedure Get_Reliable_Advertising_Mode
     (This   : in out HM11_Driver;
      Mode   : out Advertising_Mode;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+RELI?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Mode := Advertising_Mode'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Reliable_Advertising_Mode;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+RENEW", "OK+RENEW", Status);
   end Reset;

   -------------
   -- Restart --
   -------------

   procedure Restart
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+RESET", "OK+RESET", Status);
   end Restart;

   -------------------
   -- Start_Working --
   -------------------

   procedure Start_Working
     (This   : in out HM11_Driver;
      Status : out UART_Status) is
   begin
      Transmit_And_Check (This, "AT+START", "OK+START", Status);
   end Start_Working;

   ----------------------------
   -- Set_Save_Connected_MAC --
   ----------------------------

   procedure Set_Save_Connected_MAC
     (This   : in out HM11_Driver;
      Save   : Boolean;
      Status : out UART_Status)
   is
      S : constant String := Image (not Save);
   begin
      Transmit_And_Check (This, "AT+SAVE" & S, Ok_Set & S, Status);
   end Set_Save_Connected_MAC;

   ----------------------------
   -- Get_Save_Connected_MAC --
   ----------------------------

   procedure Get_Save_Connected_MAC
     (This   : in out HM11_Driver;
      Save   : out Boolean;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+SAVE?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : Character with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Save := S = '0';
         end;
      end if;
   end Get_Save_Connected_MAC;

   ------------------------
   -- Set_Discovery_Time --
   ------------------------

   procedure Set_Discovery_Time
     (This   : in out HM11_Driver;
      Time   : Discovery_Time;
      Status : out UART_Status)
   is
      S : constant String := Image (Time);
   begin
      Transmit_And_Check (This, "AT+SCAN" & S, Ok_Set & S, Status);
   end Set_Discovery_Time;

   ------------------------
   -- Get_Discovery_Time --
   ------------------------

   procedure Get_Discovery_Time
     (This   : in out HM11_Driver;
      Time   : out Discovery_Time;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+SCAN?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         declare
            S : String (1 .. 1) with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Time := Discovery_Time'Value (S);
         end;
      end if;
   end Get_Discovery_Time;

   ---------------------
   -- Set_Sensor_Type --
   ---------------------

   procedure Set_Sensor_Type
     (This   : in out HM11_Driver;
      Value  : Sensor_Type;
      Status : out UART_Status)
   is
      S : constant String := Image (Sensor_Type'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+SENS" & S, Ok_Set & S, Status);
   end Set_Sensor_Type;

   ---------------------
   -- Get_Sensor_Type --
   ---------------------

   procedure Get_Sensor_Type
     (This   : in out HM11_Driver;
      Result : out Sensor_Type;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+SENS?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := Sensor_Type'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Sensor_Type;

   ------------------------------------------------
   -- Set_Show_Device_Information_When_Discovery --
   ------------------------------------------------

   procedure Set_Show_Device_Information_When_Discovery
     (This   : in out HM11_Driver;
      Show   : Show_Device_Information;
      Status : out UART_Status)
   is
      S : constant String := Image (Show_Device_Information'Pos (Show));
   begin
      Transmit_And_Check (This, "AT+SHOW" & S, Ok_Set & S, Status);
   end Set_Show_Device_Information_When_Discovery;

   ------------------------------------------------
   -- Get_Show_Device_Information_When_Discovery --
   ------------------------------------------------

   procedure Get_Show_Device_Information_When_Discovery
     (This   : in out HM11_Driver;
      Show   : out Show_Device_Information;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+SHOW?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Show := Show_Device_Information'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Show_Device_Information_When_Discovery;

   -----------------------------------------
   -- Get_Sensor_Temperature_And_Humidity --
   -----------------------------------------

   procedure Get_Sensor_Temperature_And_Humidity
     (This        : in out HM11_Driver;
      Temperature : out Temperature_Type;
      Humidity    : out Humidity_Type;
      Status      : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+TEHU?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 6, Status);

      if Status = Ok then
         declare
            T : String (1 .. 3) with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
            H : String (1 .. 3) with Import,
              Address => This.Responce (Ok_Get'Length + 4)'Address;
         begin
            Temperature := Temperature_Type'Value (T);
            Humidity    := Humidity_Type'Value (H);
         end;
      end if;
   end Get_Sensor_Temperature_And_Humidity;

   ----------------------------
   -- Get_Module_Temperature --
   ----------------------------

   procedure Get_Module_Temperature
     (This        : in out HM11_Driver;
      Temperature : out Internal_Temperature_Type;
      Status      : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+TEMP?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + Internal_Temperature_Type'Length, Status);

      if Status = Ok then
         declare
            T : Internal_Temperature_Type with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Temperature := T;
         end;
      end if;
   end Get_Module_Temperature;

   -----------------------------------------------------
   -- Set_Temperature_Information_Byte_In_Advertising --
   -----------------------------------------------------

   procedure Set_Temperature_Information_Byte_In_Advertising
     (This        : in out HM11_Driver;
      Temperature : Advertising_Temperature_Type;
      Status      : out UART_Status)
   is
      Local : constant Advertising_Temperature_Type := Temperature;
      S     : constant String (Advertising_Temperature_Type'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+TEMP0x" & S, Ok_Set & "0x" & S, Status);
   end Set_Temperature_Information_Byte_In_Advertising;

   ---------------------------------------
   -- Set_Connect_Remote_Device_Timeout --
   ---------------------------------------

   procedure Set_Connect_Remote_Device_Timeout
     (This    : in out HM11_Driver;
      Timeout : Connect_Timeout;
      Status  : out UART_Status)
   is
      Local : constant Connect_Timeout := Timeout;
      S     : constant String (Connect_Timeout'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check (This, "AT+TCON" & S, Ok_Set & S, Status);
   end Set_Connect_Remote_Device_Timeout;

   ---------------------------------------
   -- Get_Connect_Remote_Device_Timeout --
   ---------------------------------------

   procedure Get_Connect_Remote_Device_Timeout
     (This    : in out HM11_Driver;
      Timeout : out Connect_Timeout;
      Status  : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+TCON?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + Connect_Timeout'Length, Status);

      if Status = Ok then
         declare
            L : Connect_Timeout with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Timeout := L;
         end;
      end if;
   end Get_Connect_Remote_Device_Timeout;

   -------------------
   -- Set_Bond_Mode --
   -------------------

   procedure Set_Bond_Mode
     (This   : in out HM11_Driver;
      Mode   : Bond_Mode;
      Status : out UART_Status)
   is
      S : constant String := Image (Bond_Mode'Pos (Mode));
   begin
      Transmit_And_Check (This, "AT+TYPE" & S, Ok_Set & S, Status);
   end Set_Bond_Mode;

   -------------------
   -- Get_Bond_Mode --
   -------------------

   procedure Get_Bond_Mode
     (This   : in out HM11_Driver;
      Mode   : out Bond_Mode;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+SHOW?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Mode := Bond_Mode'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_Bond_Mode;

   ----------------------
   -- Set_Service_UUID --
   ----------------------

   procedure Set_Service_UUID
     (This   : in out HM11_Driver;
      Value  : UUID;
      Status : out UART_Status)
   is
      Local : constant UUID := Value;
      S     : constant String (UUID'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+UUID0x" & S, Ok_Set & "0x" & S, Status);
   end Set_Service_UUID;

   ----------------------
   -- Get_Service_UUID --
   ----------------------

   procedure Get_Service_UUID
     (This   : in out HM11_Driver;
      Result : out UUID;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+UUID?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + UUID'Length + 2, Status);

      if Status = Ok then
         declare
            L : UUID with Import,
              Address => This.Responce (Ok_Get'Length + 3)'Address;
         begin
            Result := L;
         end;
      end if;
   end Get_Service_UUID;

   -------------------------
   -- Set_UART_Sleep_Type --
   -------------------------

   procedure Set_UART_Sleep_Type
     (This   : in out HM11_Driver;
      Value  : UART_Sleep_Type;
      Status : out UART_Status)
   is
      S : constant String := Image (UART_Sleep_Type'Pos (Value));
   begin
      Transmit_And_Check (This, "AT+UART" & S, Ok_Set & S, Status);
   end Set_UART_Sleep_Type;

   -------------------------
   -- Get_UART_Sleep_Type --
   -------------------------

   procedure Get_UART_Sleep_Type
     (This   : in out HM11_Driver;
      Result : out UART_Sleep_Type;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+UART?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 1, Status);

      if Status = Ok then
         Result := UART_Sleep_Type'Val
           (Value (This.Responce (Ok_Get'Length + 1)));
      end if;
   end Get_UART_Sleep_Type;

   -----------------------------------
   -- Set_Module_Advertisement_Data --
   -----------------------------------

   procedure Set_Module_Advertisement_Data
     (This   : in out HM11_Driver;
      Data   : Advertisement_Data;
      Status : out UART_Status)
   is
      Local : constant Advertisement_Data := Data;
      S     : constant String (Advertisement_Data'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check (This, "AT+PACK" & S, Ok_Set & S, Status);
   end Set_Module_Advertisement_Data;

   ----------------------
   -- Software_Version --
   ----------------------

   function Software_Version
     (This : in out HM11_Driver)
      return String
   is
      Zero   : Natural;
      Status : UART_Status;

   begin
      Transmit (This, "AT+VERS?", Status);
      if Status /= Ok then
         return "";
      end if;

      This.Receive
        (This.Port, This.Responce'Address, This.Responce'Length, Status);

      Zero := Find_Zero (This, This.Responce'First);
      if Zero > 0 then
         declare
            V : String (This.Responce'First .. Zero - 1) with Import,
              Address => This.Responce'Address;
         begin
            return V;
         end;

      else
         return "";
      end if;

   exception
      when others =>
         return "";
   end Software_Version;

   ------------------------
   -- Set_Characteristic --
   ------------------------

   procedure Set_Characteristic
     (This   : in out HM11_Driver;
      Value  : Characteristic_Type;
      Status : out UART_Status)
   is
      Local : constant Characteristic_Type := Value;
      S     : constant String (Characteristic_Type'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+CHAR0x" & S, Ok_Set,
         This.Responce'Address, Ok_Set'Length + 6, Status);
   end Set_Characteristic;

   ------------------------
   -- Get_Characteristic --
   ------------------------

   procedure Get_Characteristic
     (This   : in out HM11_Driver;
      Result : out Characteristic_Type;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+CHAR?", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 6, Status);

      if Status = Ok then
         declare
            L : Characteristic_Type with Import,
              Address => This.Responce (Ok_Get'Length + 3)'Address;
         begin
            Result := L;
         end;
      end if;
   end Get_Characteristic;

   -----------------------------
   -- Set_PIO_Collection_Rate --
   -----------------------------

   procedure Set_PIO_Collection_Rate
     (This   : in out HM11_Driver;
      Value  : PIO_Collection_Rate;
      Status : out UART_Status)
   is
      I : constant String := Image (Integer (Value));
   begin
      Transmit_And_Check
        (This, "AT+CYC" & (if I'Length = 1 then "0" else "") & I, Ok_Get,
         This.Responce'Address, Ok_Get'Length + 2, Status);
   end Set_PIO_Collection_Rate;

   -----------------------------
   -- Get_PIO_Collection_Rate --
   -----------------------------

   procedure Get_PIO_Collection_Rate
     (This   : in out HM11_Driver;
      Result : out PIO_Collection_Rate;
      Status : out UART_Status)
   is
      use type HAL.UInt8;
   begin
      Transmit_And_Check
        (This, "AT+CYC??", Ok_Get,
         This.Responce'Address, Ok_Get'Length + 2, Status);

      if Status = Ok then
         declare
            I : String
              (1 .. (if This.Responce (Ok_Get'Length + 2) = 0 then 1 else 2))
                with Import,
                Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Result := PIO_Collection_Rate'Value (I);
         end;
      end if;
   end Get_PIO_Collection_Rate;

   --------------------------
   -- Set_Power_Pin_Output --
   --------------------------

   procedure Set_Power_Pin_Output
     (This   : in out HM11_Driver;
      Value  : PIO_Numbers;
      Status : out UART_Status)
   is
      Local : constant PIO_Numbers := Value;
      S     : constant String (PIO_Numbers'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+BEFC" & S, Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + PIO_Numbers'Length, Status);
   end Set_Power_Pin_Output;

   --------------------------
   -- Get_Power_Pin_Output --
   --------------------------

   procedure Get_Power_Pin_Output
     (This   : in out HM11_Driver;
      Result : out PIO_Numbers;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+BEFC?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + PIO_Numbers'Length, Status);

      if Status = Ok then
         declare
            I : PIO_Numbers with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Result := I;
         end;
      end if;
   end Get_Power_Pin_Output;

   ----------------------------
   -- Set_Connect_Pin_Output --
   ----------------------------

   procedure Set_Connect_Pin_Output
     (This   : in out HM11_Driver;
      Value  : PIO_Numbers;
      Status : out UART_Status)
   is
      Local : constant PIO_Numbers := Value;
      S     : constant String (PIO_Numbers'Range) with Import,
        Address => Local (Local'First)'Address;
   begin
      Transmit_And_Check
        (This, "AT+AFTC" & S, Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + PIO_Numbers'Length, Status);
   end Set_Connect_Pin_Output;

   ----------------------------
   -- Get_Connect_Pin_Output --
   ----------------------------

   procedure Get_Connect_Pin_Output
     (This   : in out HM11_Driver;
      Result : out PIO_Numbers;
      Status : out UART_Status) is
   begin
      Transmit_And_Check
        (This, "AT+AFTC?", Ok_Get,
         This.Responce'Address,
         Ok_Get'Length + PIO_Numbers'Length, Status);

      if Status = Ok then
         declare
            I : PIO_Numbers with Import,
              Address => This.Responce (Ok_Get'Length + 1)'Address;
         begin
            Result := I;
         end;
      end if;
   end Get_Connect_Pin_Output;

   ----------------
   -- Start_With --
   ----------------

   function Start_With
     (This  : HM11_Driver;
      Value : String;
      From  : Positive;
      To    : Positive)
      return Boolean
   is
      Last  : Positive := From + Value'Length - 1;
      Local : constant String := Value;
      V     : UART_Data_8b (Local'Range) with Import, Address => Local'Address;
   begin
      if From <= To then
         return Last <= To and then This.Responce (From .. Last) = V;

      else
         if Last <= This.Responce'Last then
            return This.Responce (From .. Last) = V;
         end if;

         Last := This.Responce'Last - From + 1;
         return Start_With
           (This, Value (Value'First .. Value'First + Last - 1),
            From, This.Responce'Last)
           and then Start_With (This, Value (Last .. Value'Last), 1, To);
      end if;
   end Start_With;

   -----------
   -- Image --
   -----------

   function Image (Value : Natural) return String is
      S : constant String := Value'Img;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Send_Data_Method) return String is
   begin
      case Value is
         when Write =>
            return "WR";
         when Write_Without_Response =>
            return "WN";
         when Indicate =>
            return "IN";
         when Notify =>
            return "NO";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Send_Data_Characteristic) return String is
   begin
      case Value is
         when Write =>
            return "WR";
         when Write_Without_Response =>
            return "WN";
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (V : HAL.UInt8) return Natural is
   begin
      return Natural'Value (Character'Val (V) & "");
   end Value;

   -----------
   -- Value --
   -----------

   function Image (Value : Boolean) return String is
   begin
      if Value then
         return "1";
      else
         return "0";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : MAC_Address_Type) return String is
   begin
      case Value is
         when Normal_Address =>
            return "N";
         when Static_MAC =>
            return "0";
         when Static_Random_MAC =>
            return "1";
         when Random_MAC =>
            return "2";
      end case;
   end Image;

   -----------------------
   -- To_Connect_Result --
   -----------------------

   function To_Connect_Result (C : Character) return Connect_Result is
   begin
      case C is
         when 'L'      => return Connecting;
         when 'E'      => return Connect_Error;
         when 'F'      => return Connect_Fail;
         when 'N'      => return No_Address;
         when ' ' | ASCII.NUL |
            '0' .. '5' => return Connected;
         when others   => return Other_Error;
      end case;
   end To_Connect_Result;

   ---------------
   -- To_String --
   ---------------

   function To_String (Handle : Handle_Type) return String is
      Local_Handle : Handle_Type := Handle;
      H            : String (Handle_Type'Range) with Import,
        Address => Local_Handle (Local_Handle'First)'Address;
   begin
      return H;
   end To_String;

   ---------------
   -- Find_Zero --
   ---------------

   function Find_Zero
     (This : HM11_Driver;
      From : Positive)
      return Natural
   is
      use type HAL.UInt8;
   begin
      for Index in From .. This.Responce'Last loop
         if This.Responce (Index) = 0 then
            return Index;
         end if;
      end loop;

      return This.Responce'Last + 1;
   end Find_Zero;

   ----------
   -- Find --
   ----------

   function Find
     (This   : HM11_Driver;
      Value  : String;
      From   : Positive;
      To     : Positive)
      return Natural
   is
      Local : constant String := Value;
      V     : UART_Data_8b (Local'Range) with Import, Address => Local'Address;
      Res   : Natural;

      function Do_Find (F, T : Positive) return Natural;
      function Do_Find (F, T : Positive) return Natural is
      begin
         for Index in F .. T loop
            if This.Responce (Index .. Index + V'Length - 1) = V then
               return Index;
            end if;
         end loop;

         return 0;
      end Do_Find;

   begin
      if From <= To then
         return Do_Find (From, To - Value'Length + 1);

      else
         Res := Do_Find (From, This.Responce'Last - Value'Length + 1);
         if Res /= 0 then
            return Res;
         end if;
         return Do_Find (This.Responce'First, To - Value'Length + 1);
      end if;
   end Find;

   -----------------
   -- Calc_Lenght --
   -----------------

   function Calc_Lenght
     (This : HM11_Driver;
      From : Positive;
      Zero : Positive)
      return Natural is
   begin
      if From = Zero then
         return 0;
      end if;

      if From < Zero then
         return Zero - From;
      else
         return (This.Responce'Last - From + 1) +
           (This.Responce'First + Zero - 2);
      end if;
   end Calc_Lenght;

   ----------
   -- Move --
   ----------

   procedure Move
     (This   : HM11_Driver;
      Result : in out Positive;
      Add    : Positive;
      Zero   : Positive;
      Length : out Natural) is
   begin
      Result := Result + Add;
      if Result > This.Responce'Last then
         Result := Result - This.Responce'Length;
      end if;
      Length := Calc_Lenght (This, Result, Zero);
   end Move;

   ------------
   -- Append --
   ------------

   procedure Append
     (This : HM11_Driver;
      Str  : in out Variable_String;
      From : Positive;
      To   : Positive)
   is
      procedure Do_Append (F, T : Positive);
      procedure Do_Append (F, T : Positive)
      is
         S : String (F .. T) with Import,
           Address => This.Responce (F)'Address;
      begin
         Str.Value (Str.Last + 1 .. Str.Last + S'Length) := S;
         Str.Last := Str.Last + S'Length;
      end Do_Append;

   begin
      if From <= To then
         Do_Append (From, To);
      else
         Do_Append (From, This.Responce'Last);
         Do_Append (This.Responce'First, To);
      end if;
   end Append;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (This : HM11_Driver;
      To   : out UART_Data_8b;
      From : Positive)
   is
      L : Positive;
   begin
      if From + To'Length - 1 <= This.Responce'Last then
         To := This.Responce (From .. From + To'Length - 1);

      else
         L := This.Responce'Last - From + 1;

         To (To'First .. To'First + L - 1) :=
           This.Responce (From .. This.Responce'Last);

         To (To'First + L .. To'Last) :=
           This.Responce (This.Responce'First ..
                            This.Responce'First + To'Last - To'First - L);
      end if;

   end Copy;

end HM11;
