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

package body BlueNRG_MS.Debug is
   
   LF   : constant Character := Character'Val(16#0A#);
   CR   : constant Character := Character'Val(16#0D#);
   CRLF : constant String    := CR & LF;

   -------------------------
   -- HCI_Event_To_String --
   -------------------------

   function HCI_Event_To_String
     (Data : Byte_Array)
      return String
   is
   begin
      if Data'Length = 0 then
         return "No Data";
      end if;

      if Data(1) /= 16#04# then
         return "Not an Event";
      end if;

      if Data'Length < 2 then
         return "Not enough data";
      end if;

      if Data(2) /= 16#FF# then
         return Bluetooth.Debug.HCI_Event_To_String(Data);
      end if;

      if Data(5) = 16#08# and
         Data(4) = 16#00#
      then
         return "Evt_Blue_L2CAP_Conn_Upd_Resp";
      end if;
      if Data(5) = 16#08# and
         Data(4) = 16#01#
      then
         return "Evt_Blue_L2CAP_Procedure_Timeout";
      end if;
      if Data(5) = 16#08# and
         Data(4) = 16#02#
      then
         return "Evt_Blue_L2CAP_Conn_Upd_Req";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#00#
      then
         return "Evt_Blue_Gap_Limited_Discoverable";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#01#
      then
         return 
         "Evt_Blue_Gap_Pairing_Cmplt" & CRLF &
         
         "Status      : " & 
        (if    Data(8) = 16#00# then
            "Success"   
         elsif Data(8) = 16#01# then
            "Pairing Timeout"   
         elsif Data(8) = 16#02# then
            "Pairing Failed"
         else
            "Unknown Value");
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#02#
      then
         return "Evt_Blue_Gap_Pass_Key_Request";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#03#
      then
         return "Evt_Blue_Gap_Authorization_Request";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#04#
      then
         return "Evt_Blue_Gap_Slave_Security_Initiated";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#05#
      then
         return "Evt_Blue_Gap_Bond_Lost";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#07#
      then
         return "Evt_Blue_Gap_Procedure_Complete";
      end if;
      if Data(5) = 16#04# and
         Data(4) = 16#08#
      then
         return "Evt_Blue_Gap_Addr_Not_Resolved";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#01#
      then
         return 
         "Evt_Blue_Gatt_Attribute_modified" & CRLF &
         "Conn_Handle : " & To_Hex(Data(6 .. 7)) & CRLF &
         "Attr_Handle : " & To_Hex(Data(8 .. 9)) & CRLF &
         "Data Length : " & Unsigned_8'Image(Data(10)) & CRLF &
         "Offset      : " & Integer'Image(Integer(Data(11)) + Integer(Data(12)) * 16#FF#) & CRLF &
         "Value       : " & To_Hex(Data(13 .. Data'Last));
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#02#
      then
         return "Evt_Blue_Gatt_Procedure_Timeout";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#03#
      then
         return "Evt_Blue_Att_Exchange_MTU_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#04#
      then
         return "Evt_Blue_Att_Find_Information_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#05#
      then
         return "Evt_Blue_Att_Find_By_Type_Value_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#06#
      then
         return "Evt_Blue_Att_Read_By_Type_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#07#
      then
         return "Evt_Blue_Att_Read_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#08#
      then
         return "Evt_Blue_Att_Read_Blob_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#09#
      then
         return "Evt_Blue_Att_Read_Multiple_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#0A#
      then
         return "Evt_Blue_Att_Read_By_Group_Type_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#0C#
      then
         return "Evt_Blue_Att_Prepare_Write_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#0D#
      then
         return "Evt_Blue_Att_Exec_Write_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#0E#
      then
         return "Evt_Blue_Gatt_Indication";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#0F#
      then
         return "Evt_Blue_Gatt_notification";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#10#
      then
         return "Evt_Blue_Gatt_Procedure_Complete";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#11#
      then
         return "Evt_Blue_Gatt_Error_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#12#
      then
         return "Evt_Blue_Gatt_Disc_Read_Charac_By_UUID_Resp";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#13#
      then
         return "Evt_Blue_Gatt_Write_Permit_req";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#14#
      then
         return "Evt_Blue_Gatt_Read_Permit_Req";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#15#
      then
         return "Evt_Blue_Gatt_Read_Multi_Permit_Req";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#16#
      then
         return "Evt_Blue_Gatt_Tx_Pool_Available";
      end if;
      if Data(5) = 16#0C# and
         Data(4) = 16#17#
      then
         return "Evt_Blue_Gatt_Server_Confirmation";
      end if;
      return "Unknown Event";
   exception
      when others =>
         return "Malformed Packet";
   end HCI_Event_To_String;

end BlueNRG_MS.Debug;
