----------------------------------------------------------------------------
--                                                                        --
--  Bluetooth.Debug                                                       --
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

package body Bluetooth.Debug is

   LF   : constant Character := Character'Val(16#0A#);
   CR   : constant Character := Character'Val(16#0D#);
   CRLF : constant String    := CR & LF;
   
   --------------------------
   -- Error_Code_To_String --
   --------------------------

   function Error_Code_To_String
     (Code : Byte)
      return String
   is
   begin
      case Code is
         when 16#02# =>
            return "Unknown Connection Identifier";
         when 16#03# =>
            return "Hardware Failure";
         when 16#04# =>
            return "Page Timeout";
         when 16#05# =>
            return "Authentication Failure";
         when 16#06# =>
            return "PIN or Key Missing";
         when 16#07# =>
            return "Memory Capacity Exceeded";
         when 16#08# =>
            return "Connection Timeout";
         when 16#09# =>
            return "Connection Limit Exceeded";
         when 16#0A# =>
            return "Synchronous Connection Limit To A Device Exceeded";
         when 16#0B# =>
            return "Connection Already Exists";
         when 16#0C# =>
            return "Command Disallowed";
         when 16#0D# =>
            return "Connection Rejected due to Limited Resources";
         when 16#0E# =>
            return "Connection Rejected Due To Security Reasons";
         when 16#0F# =>
            return "Connection Rejected due to Unacceptable BD_ADDR";
         when 16#10# =>
            return "Connection Accept Timeout Exceeded";
         when 16#11# =>
            return "Unsupported Feature or Parameter Value";
         when 16#12# =>
            return "Invalid HCI Command Parameters";
         when 16#13# =>
            return "Remote User Terminated Connection";
         when 16#14# =>
            return "Remote Device Terminated Connection due to Low Resources";
         when 16#15# =>
            return "Remote Device Terminated Connection due to Power Off";
         when 16#16# =>
            return "Connection Terminated By Local Host";
         when 16#17# =>
            return "Repeated Attempts";
         when 16#18# =>
            return "Pairing Not Allowed";
         when 16#19# =>
            return "Unknown LMP PDU";
         when 16#1A# =>
            return "Unsupported Remote Feature / Unsupported LMP Feature";
         when 16#1B# =>
            return "SCO Offset Rejected";
         when 16#1C# =>
            return "SCO Interval Rejected";
         when 16#1D# =>
            return "SCO Air Mode Rejected";
         when 16#1E# =>
            return "Invalid LMP Parameters / Invalid LL Parameters";
         when 16#1F# =>
            return "Unspecified Error";
         when 16#20# =>
            return "Unsupported LMP Parameter Value / Unsupported LL Parameter Value";
         when 16#21# =>
            return "Role Change Not Allowed";
         when 16#22# =>
            return "LMP Response Timeout / LL Response Timeout";
         when 16#23# =>
            return "LMP Error Transaction Collision / LL Procedure Collision";
         when 16#24# =>
            return "LMP PDU Not Allowed";
         when 16#25# =>
            return "Encryption Mode Not Acceptable";
         when 16#26# =>
            return "Link Key cannot be Changed";
         when 16#27# =>
            return "Requested QoS Not Supported";
         when 16#28# =>
            return "Instant Passed";
         when 16#29# =>
            return "Pairing With Unit Key Not Supported";
         when 16#2A# =>
            return "Different Transaction Collision";
         when 16#2B# =>
            return "Reserved for Future Use";
         when 16#2C# =>
            return "QoS Unacceptable Parameter";
         when 16#2D# =>
            return "QoS Rejected";
         when 16#2E# =>
            return "Channel Classification Not Supported";
         when 16#2F# =>
            return "Insufficient Security";
         when 16#30# =>
            return "Parameter Out Of Mandatory Range";
         when 16#31# =>
            return "Reserved for Future Use";
         when 16#32# =>
            return "Role Switch Pending";
         when 16#33# =>
            return "Reserved for Future Use";
         when 16#34# =>
            return "Reserved Slot Violation";
         when 16#35# =>
            return "Role Switch Failed";
         when 16#36# =>
            return "Extended Inquiry Response Too Large";
         when 16#37# =>
            return "Secure Simple Pairing Not Supported By Host";
         when 16#38# =>
            return "Host Busy - Pairing";
         when 16#39# =>
            return "Connection Rejected due to No Suitable Channel Found";
         when 16#3A# =>
            return "Controller Busy";
         when 16#3B# =>
            return "Unacceptable Connection Parameters";
         when 16#3C# =>
            return "Advertising Timeout";
         when 16#3D# =>
            return "Connection Terminated due to MIC Failure";
         when 16#3E# =>
            return "Connection Failed to be Established";
         when 16#3F# =>
            return "MAC Connection Failed";
         when 16#40# =>
            return "Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock";
         when 16#41# =>
            return "Type0 Submap Not Defined";
         when 16#42# =>
            return "Unknown Advertising Identifier";
         when 16#43# =>
            return "Limit Reached";
         when 16#44# =>
            return "Operation Cancelled by Host";
         when others =>
            return "Unknown Error Code";
      end case;
   end Error_Code_To_String;

   -------------------------
   -- HCI_Event_To_String --
   -------------------------

   function HCI_Event_To_String
     (Data : Byte_Array)
      return String
   is
      --------------------------------------
      -- LE_Connection_Complete_To_String --
      --------------------------------------
     
      function LE_Connection_Complete_To_String 
         (Data : Byte_Array)
          return String
      is
      begin
         return "Event_LE_Conn_Complete " & CRLF &

         "Status      : " &
         (if Data(5) = 16#00# then
            "Success"
         else
             "Error:" & Error_Code_To_String(Data(5))) & CRLF &
            
         "Handle      : " & To_Hex(Data(6 .. 7)) & CRLF &
         
         "Role        : " & 
         (if Data(8) = 16#00# then
           "Master "
         else
           "Slave ") & CRLF &
           
         "Address     : " & To_Hex(Data(9 .. 15)) & CRLF &

         "ConnInt     :" & Integer'Image(Integer(Data(16) + Data(17) * 16#FF#)) & CRLF &

         "SuperTimeout:" & Integer'Image(Integer(Data(20) + Data(21) * 16#FF#));

      end LE_Connection_Complete_To_String;

      ---------------------------------------------
      -- LE_Connection_Update_Complete_To_String --
      ---------------------------------------------

      function LE_Connection_Update_Complete_To_String
        (Data : Byte_Array)
         return String
      is
      begin
         return "LE_Connection_Update_Complete" & CRLF &

         "Status      : " &
         (if Data(5) = 16#00# then
             "Success"
          else
             "Error: " & Error_Code_To_String(Data(5)));

      end LE_Connection_Update_Complete_To_String;

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

      case Data(2) is
         when 16#05# =>
            return "Event_Disconn_Complete" & CRLF &

           (if Data(4) = 16#00# then
               "Disconnection Occured"
            else
               Error_Code_To_String(Data(4))) & CRLF &

            "Handle      : " & To_Hex(Data(5 .. 6)) & CRLF &

            Error_Code_To_String(Data(7));

         when 16#08# =>
            return "Event_Encrypt_Change";
         when 16#0C# =>
            return "Event_Read_Remote_Version_Complete";
         when 16#0E# =>
            return "Event_Cmd_Complete";
         when 16#0F# =>
            return "Event_Cmd_Status";
         when 16#10# =>
            return "Event_Hardware_Error";
         when 16#13# =>
            return "Event_Num_Comp_Pkts";
         when 16#1A# =>
            return "Event_Data_Buffer_Overflow";
         when 16#30# =>
            return "Event_Encryption_Key_Refresh_Complete";
         when 16#3E# =>
            case Data(4) is
               when 16#01# =>
                  return LE_Connection_Complete_To_String(Data);
               when 16#02# =>
                  return "Event_LE_Advertising_Report";
               when 16#03# =>
                  return LE_Connection_Update_Complete_To_String(Data);
               when 16#04# =>
                  return "Event_LE_Read_Remote_Used_Features_Complete";
               when 16#05# =>
                  return "Event_LE_LTK_Request";
               when others =>
                  return "Unknown LE Event";
            end case;
         when 16#FF# =>
            return "Vendor Specific Event";
         when others =>
            return "Unknown Event";
      end case;
   exception
      when others =>
         return "Malformed Packet";
   end HCI_Event_To_String;

end Bluetooth.Debug;
