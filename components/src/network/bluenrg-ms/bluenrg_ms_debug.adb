----------------------------------------------------------------------------
--                                                                        --
--  BlueNRG-MS.Debug                                                      --
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
with Bluetooth_Debug;

package body BlueNRG_MS_Debug is

   LF   : constant Character := Character'Val (16#0A#);
   CR   : constant Character := Character'Val (16#0D#);
   CRLF : constant String    := CR & LF;

   ------------
   -- To_Hex --
   ------------

   function To_Hex
     (Input : UInt8)
      return String
   is
      Output : String (1 .. 2);
   begin

      case Input and 16#0F# is
         when 16#00# =>
            Output (2) := '0';
         when 16#01# =>
            Output (2) := '1';
         when 16#02# =>
            Output (2) := '2';
         when 16#03# =>
            Output (2) := '3';
         when 16#04# =>
            Output (2) := '4';
         when 16#05# =>
            Output (2) := '5';
         when 16#06# =>
            Output (2) := '6';
         when 16#07# =>
            Output (2) := '7';
         when 16#08# =>
            Output (2) := '8';
         when 16#09# =>
            Output (2) := '9';
         when 16#0A# =>
            Output (2) := 'A';
         when 16#0B# =>
            Output (2) := 'B';
         when 16#0C# =>
            Output (2) := 'C';
         when 16#0D# =>
            Output (2) := 'D';
         when 16#0E# =>
            Output (2) := 'E';
         when 16#0F# =>
            Output (2) := 'F';
         when others =>
            Output (2) := '?';
      end case;

      case Input and 16#F0# is
         when 16#00# =>
            Output (1) := '0';
         when 16#10# =>
            Output (1) := '1';
         when 16#20# =>
            Output (1) := '2';
         when 16#30# =>
            Output (1) := '3';
         when 16#40# =>
            Output (1) := '4';
         when 16#50# =>
            Output (1) := '5';
         when 16#60# =>
            Output (1) := '6';
         when 16#70# =>
            Output (1) := '7';
         when 16#80# =>
            Output (1) := '8';
         when 16#90# =>
            Output (1) := '9';
         when 16#A0# =>
            Output (1) := 'A';
         when 16#B0# =>
            Output (1) := 'B';
         when 16#C0# =>
            Output (1) := 'C';
         when 16#D0# =>
            Output (1) := 'D';
         when 16#E0# =>
            Output (1) := 'E';
         when 16#F0# =>
            Output (1) := 'F';
         when others =>
            Output (1) := '?';
      end case;

      return Output;
   end To_Hex;

   ------------
   -- To_Hex --
   ------------

   function To_Hex
     (Input : UInt8_Array)
      return String
   is
      Output : String (1 .. Input'Length * 3);
   begin

      if Input'Length = 0 then
         return "";
      end if;

      for I in Integer range Input'Range loop
         Output (1 + 3 * (I - Input'First) .. 2 + 3 * (I - Input'First)) := To_Hex (Input (I));
         Output (3 + 3 * (I - Input'First)) := ' ';
      end loop;

      return Output;
   end To_Hex;

   -------------------------
   -- HCI_Event_To_String --
   -------------------------

   function HCI_Event_To_String
     (Data : UInt8_Array)
      return String
   is
   begin
      if Data'Length = 0 then
         return "No Data";
      end if;

      if Data (1) /= 16#04# then
         return "Not an Event";
      end if;

      if Data'Length < 2 then
         return "Not enough data";
      end if;

      if Data (2) /= 16#FF# then
         return Bluetooth_Debug.HCI_Event_To_String (Data);
      end if;

      if Data (5) = 16#08# and
         Data (4) = 16#00#
      then
         return "Evt_Blue_L2CAP_Conn_Upd_Resp";
      end if;
      if Data (5) = 16#08# and
         Data (4) = 16#01#
      then
         return "Evt_Blue_L2CAP_Procedure_Timeout";
      end if;
      if Data (5) = 16#08# and
         Data (4) = 16#02#
      then
         return "Evt_Blue_L2CAP_Conn_Upd_Req";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#00#
      then
         return "Evt_Blue_Gap_Limited_Discoverable";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#01#
      then
         return
         "Evt_Blue_Gap_Pairing_Cmplt" & CRLF &

         "Status      : " &
        (if    Data (8) = 16#00# then
            "Success"
         elsif Data (8) = 16#01# then
            "Pairing Timeout"
         elsif Data (8) = 16#02# then
            "Pairing Failed"
         else
            "Unknown Value");
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#02#
      then
         return "Evt_Blue_Gap_Pass_Key_Request";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#03#
      then
         return "Evt_Blue_Gap_Authorization_Request";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#04#
      then
         return "Evt_Blue_Gap_Slave_Security_Initiated";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#05#
      then
         return "Evt_Blue_Gap_Bond_Lost";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#07#
      then
         return "Evt_Blue_Gap_Procedure_Complete";
      end if;
      if Data (5) = 16#04# and
         Data (4) = 16#08#
      then
         return "Evt_Blue_Gap_Addr_Not_Resolved";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#01#
      then
         return
         "Evt_Blue_Gatt_Attribute_modified" & CRLF &
         "Conn_Handle : " & To_Hex (Data (6 .. 7)) & CRLF &
         "Attr_Handle : " & To_Hex (Data (8 .. 9)) & CRLF &
         "Data Length : " & UInt8'Image (Data (10)) & CRLF &
         "Offset      : " & Integer'Image (Integer (Data (11)) + Integer (Data (12)) * 16#FF#) & CRLF &
         "Value       : " & To_Hex (Data (13 .. Data'Last));
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#02#
      then
         return "Evt_Blue_Gatt_Procedure_Timeout";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#03#
      then
         return "Evt_Blue_Att_Exchange_MTU_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#04#
      then
         return "Evt_Blue_Att_Find_Information_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#05#
      then
         return "Evt_Blue_Att_Find_By_Type_Value_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#06#
      then
         return "Evt_Blue_Att_Read_By_Type_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#07#
      then
         return "Evt_Blue_Att_Read_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#08#
      then
         return "Evt_Blue_Att_Read_Blob_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#09#
      then
         return "Evt_Blue_Att_Read_Multiple_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#0A#
      then
         return "Evt_Blue_Att_Read_By_Group_Type_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#0C#
      then
         return "Evt_Blue_Att_Prepare_Write_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#0D#
      then
         return "Evt_Blue_Att_Exec_Write_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#0E#
      then
         return "Evt_Blue_Gatt_Indication";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#0F#
      then
         return "Evt_Blue_Gatt_notification";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#10#
      then
         return "Evt_Blue_Gatt_Procedure_Complete";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#11#
      then
         return "Evt_Blue_Gatt_Error_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#12#
      then
         return "Evt_Blue_Gatt_Disc_Read_Charac_By_UUID_Resp";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#13#
      then
         return "Evt_Blue_Gatt_Write_Permit_req";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#14#
      then
         return "Evt_Blue_Gatt_Read_Permit_Req";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#15#
      then
         return "Evt_Blue_Gatt_Read_Multi_Permit_Req";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#16#
      then
         return "Evt_Blue_Gatt_Tx_Pool_Available";
      end if;
      if Data (5) = 16#0C# and
         Data (4) = 16#17#
      then
         return "Evt_Blue_Gatt_Server_Confirmation";
      end if;
      return "Unknown Event";
   exception
      when others =>
         return "Malformed Packet";
   end HCI_Event_To_String;

end BlueNRG_MS_Debug;
