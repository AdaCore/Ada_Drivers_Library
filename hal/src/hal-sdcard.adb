with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body HAL.SDCard is
   procedure Convert_Card_Identification_Data_Register
     (W0, W1, W2, W3 : Unsigned_32;
      Res: out Card_Identification_Data_Register);
   --  Convert the R2 reply to CID

   procedure Convert_Card_Specific_Data_Register
     (W0, W1, W2, W3 : Unsigned_32;
      Info : in out Card_Information);
   --  Convert the R2 reply to Info.CD_CSD

   procedure Send_Cmd
     (This : in out SDCard_Driver'Class;
      Cmd : SD_Command;
      Arg : Unsigned_32;
      Status : out SD_Error) is
   begin
      Send_Cmd (This, Cmd_Desc (Cmd), Arg, Status);
   end Send_Cmd;

   procedure Send_ACmd
     (This : in out SDCard_Driver'Class;
      Cmd : SD_Specific_Command;
      S_Arg : Unsigned_32;
      A_Arg : Unsigned_32;
      Status : out SD_Error) is
   begin
      Send_Cmd (This, Cmd_Desc (APP_Cmd), S_Arg, Status);
      if Status /= OK then
         return;
      end if;
      Send_Cmd (This, Acmd_Desc (Cmd), A_Arg, Status);
   end Send_ACmd;

   --  Generic initialization procedure.
   procedure Initialize (Driver : in out SDCard_Driver'Class;
                         Info : out Card_Information;
                         Status : out SD_Error)
   is
      Rsp : Unsigned_32;
      W0, W1, W2, W3 : Unsigned_32;
      Rca : Unsigned_32;
   begin
      --  Reset controller
      Driver.Reset (Status);
      if Status /= OK then
         return;
      end if;

      Send_Cmd (Driver, Go_Idle_State, 0, Status);
      if Status /= OK then
         return;
      end if;

      --  CMD8: IF_Cond, voltage supplied: 0x1 (2.7V - 3.6V)
      Send_Cmd (Driver, Send_If_Cond, 16#1a5#, Status);
      if Status = OK then
         --  at least SD Card 2.0
         Info.Card_Type := STD_Capacity_SD_Card_v2_0;

         Read_Rsp48 (Driver, Rsp);
         if (Rsp and 16#fff#) /= 16#1a5# then
            --  Bad voltage or bad pattern.
            Put_Line ("if_cond: bad reply");
            Status := Error;
            return;
         end if;
      else
         --  If SD Card, then it's v1.1
         Info.Card_Type := STD_Capacity_SD_Card_V1_1;
      end if;

      --  ACMD41: SD_SEND_OP_COND (no crc check)
      --  Arg: HCS=1, XPC=0, S18R=0
      for I in 1 .. 5 loop
         Put_Line ("wait...");
         delay until Clock + Milliseconds (200);

         Send_Acmd (Driver, SD_App_Send_Op_Cond, 0, 16#40ff_0000#, Status);
         if Status /= Ok then
            Put_Line ("send_op_cond failed");
         end if;

         Read_Rsp48 (Driver, Rsp);
         if (Rsp and SD_OCR_High_Capacity) = SD_OCR_High_Capacity then
            Info.Card_Type := High_Capacity_SD_Card;
         end if;
         if (Rsp and SD_OCR_Power_Up) = 0 then
            Put_Line ("op_cond: card is busy");
            Status := Error;
         else
            Status := OK;
            exit;
         end if;
      end loop;
      if Status /= Ok then
         return;
      end if;

      --  TODO: Switch voltage

      --  CMD2: ALL_SEND_CID (136 bits)
      Send_Cmd (Driver, All_Send_CID, 0, Status);
      if Status /= OK then
         Put_Line ("send_sid: timeout");
         return;
      end if;
      Read_Rsp136 (Driver, W0, W1, W2, W3);
      Convert_Card_Identification_Data_Register (W0, W1, W2, W3, Info.SD_CID);

      --  CMD3: SEND_RELATIVE_ADDR
      Send_Cmd (Driver, Send_Relative_Addr, 0, Status);
      if Status /= OK then
         Put_Line ("send_relative_addr: timeout");
         return;
      end if;
      Read_Rsp48 (Driver, Rsp);
      Rca := Rsp and 16#ffff_0000#;
      Info.RCA := Uint16 (Shift_Right (Rca, 16));

      if (Rsp and 16#e100#) /= 16#0100# then
         Put_Line ("card is not ready");
         return;
      end if;

      --  CMD9: SEND_CSD
      Send_Cmd (Driver, Send_CSD, Rca, Status);
      if Status /= OK then
         Put_Line ("send_csd: timeout");
         return;
      end if;
      Read_Rsp136 (Driver, W0, W1, W2, W3);
      Convert_Card_Specific_Data_Register (W0, W1, W2, W3, Info);

      --  Switch to 25Mhz
      Set_Clock (Driver, 25_000_000);

      --  CMD7: SELECT
      Send_Cmd (Driver, Select_Card, Rca, Status);
      if Status /= Ok then
         Put_Line ("select: timeout");
         return;
      end if;

      --  CMD13: STATUS
      Send_Cmd (Driver, Send_Status, Rca, Status);
      if Status /= Ok then
         Put_Line ("send_status: timeout");
         return;
      end if;

      --  ACMD51: Read SCR
      declare
         type SD_SCR is array (1 .. 2) of Uint32;

         SCR : SD_SCR;
         Tmp  : SD_SCR;

         SD_0TO7BITS                 : constant := 16#0000_00FF#;
         SD_8TO715ITS                : constant := 16#0000_FF00#;
         SD_16TO23BITS               : constant := 16#00FF_0000#;
         SD_24TO31BITS               : constant := 16#FF00_0000#;
      begin
         Send_Cmd (Driver, App_Cmd, Rca, Status);
         if Status /= Ok then
            Put_Line ("app_specific failed");
            return;
         end if;

         Read_Cmd (Driver, Acmd_Desc (SD_App_Send_SCR), 0,
                   Tmp'Address, 8, Status);
         if Status /= Ok then
            Put_Line ("send_scr failed");
            return;
         end if;

         SCR (1) := Shift_Left (Tmp (2) and SD_0TO7BITS, 24)
           or Shift_Left (Tmp (2) and SD_8TO715ITS, 8)
           or Shift_Right (Tmp (2) and SD_16TO23BITS, 8)
           or Shift_Right (Tmp (2) and SD_24TO31BITS, 24);
         SCR (2) := Shift_Left (Tmp (1) and SD_0TO7BITS, 24)
           or Shift_Left (Tmp (1) and SD_8TO715ITS, 8)
           or Shift_Right (Tmp (1) and SD_16TO23BITS, 8)
           or Shift_Right (Tmp (1) and SD_24TO31BITS, 24);
      end;

      --  Bus size
      case Info.Card_Type is
         when STD_Capacity_SD_Card_V1_1
           | STD_Capacity_SD_Card_v2_0
           | High_Capacity_SD_Card =>
            Send_ACmd (Driver, SD_App_Set_Bus_Width, Rca, 2, Status);
            if Status /= OK then
               Put_Line ("set bus width failed");
               return;
            else
               Set_Bus_Size (Driver, Wide_Bus_4B);
            end if;
         when others =>
            null;
      end case;
   end Initialize;

   procedure Convert_Card_Identification_Data_Register
     (W0, W1, W2, W3 : Unsigned_32;
      Res: out Card_Identification_Data_Register)
   is
      Tmp : Byte;
   begin
      --  Byte 0
      Tmp := Byte (Shift_Right (W0 and 16#FF00_0000#, 24));
      Res.Manufacturer_ID := Tmp;

      --  Byte 1 & 2
      Tmp := Byte (Shift_Right (W0 and 16#00FF_0000#, 16));
      Res.OEM_Application_ID (1) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (W0 and 16#0000_FF00#, 8));
      Res.OEM_Application_ID (2) := Character'Val (Tmp);

      --  Byte 3-7
      Tmp := Byte (W0 and 16#0000_00FF#);
      Res.Product_Name (1) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (W1 and 16#FF00_0000#, 24));
      Res.Product_Name (2) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (W1 and 16#00FF_0000#, 16));
      Res.Product_Name (3) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (W1 and 16#0000_FF00#, 8));
      Res.Product_Name (4) := Character'Val (Tmp);
      Tmp := Byte (W1 and 16#0000_00FF#);
      Res.Product_Name (5) := Character'Val (Tmp);

      --  Byte 8
      Tmp := Byte (Shift_Right (W2 and 16#FF00_0000#, 24));
      Res.Product_Revision.Major := UInt4 (Shift_Right (Tmp, 4));
      Res.Product_Revision.Minor := UInt4 (Tmp and 16#0F#);

      --  Byte 9 - 12
      Res.Product_Serial_Number :=
        Shift_Left (W2 and 16#00FF_FFFF#, 8) or
        Shift_Right (W3 and 16#FF00_0000#, 24);

      --  Byte 13 - 14
      Res.Manufacturing_Date.Month :=
        Manufacturing_Month'Val (Shift_Right (W3 and 16#0000_0F00#, 8) - 1);
      Res.Manufacturing_Date.Year :=
        Manufacturing_Year (2000 + Shift_Right (W3 and 16#000F_F000#, 12));

      --  Byte 15
      Tmp := Byte (W3 and 16#0000_00FF#);
      Res.CID_CRC := Shift_Right (Tmp and 16#FE#, 1);
   end Convert_Card_Identification_Data_Register;

   procedure Convert_Card_Specific_Data_Register
     (W0, W1, W2, W3 : Unsigned_32;
      Info : in out Card_Information)
   is
      Tmp : Byte;
   begin
      --  Analysis of CSD Byte 0
      Tmp := Byte (Shift_Right (W0 and 16#FF00_0000#, 24));
      Info.SD_CSD.CSD_Structure := Shift_Right (Tmp and 16#C0#, 6);
      Info.SD_CSD.System_Specification_Version :=
        Shift_Right (Tmp and 16#3C#, 2);
      Info.SD_CSD.Reserved := Tmp and 16#03#;

      --  Byte 1
      Tmp := Byte (Shift_Right (W0 and 16#00FF_0000#, 16));
      Info.SD_CSD.Data_Read_Access_Time_1 := Tmp;

      --  Byte 2
      Tmp := Byte (Shift_Right (W0 and 16#0000_FF00#, 8));
      Info.SD_CSD.Data_Read_Access_Time_2 := Tmp;

      --  Byte 3
      Tmp := Byte (W0 and 16#0000_00FF#);
      Info.SD_CSD.Max_Bus_Clock_Frequency := Tmp;

      --  Byte 4 & 5
      Info.SD_CSD.Card_Command_Class :=
        Uint16 (Shift_Right (W1 and 16#FFF0_0000#, 20));
      Info.SD_CSD.Max_Read_Data_Block_Length :=
        Byte (Shift_Right (W1 and 16#000F_0000#, 16));

      --  Byte 6
      Tmp := Byte (Shift_Right (W1 and 16#0000_FF00#, 8));
      Info.SD_CSD.Partial_Block_For_Read_Allowed := (Tmp and 16#80#) /= 0;
      Info.SD_CSD.Write_Block_Missalignment := (Tmp and 16#40#) /= 0;
      Info.SD_CSD.Read_Block_Missalignment := (Tmp and 16#20#) /= 0;
      Info.SD_CSD.DSR_Implemented := (Tmp and 16#10#) /= 0;
      Info.SD_CSD.Reserved_2 := 0;

      if Info.Card_Type = STD_Capacity_SD_Card_V1_1
        or else Info.Card_Type = STD_Capacity_SD_Card_v2_0
      then
         Info.SD_CSD.Device_Size := Shift_Left (Uint32 (Tmp) and 16#03#, 10);

         --  Byte 7
         Tmp := Byte (W1 and 16#0000_00FF#);
         Info.SD_CSD.Device_Size := Info.SD_CSD.Device_Size or
           Shift_Left (Uint32 (Tmp), 2);

         --  Byte 8
         Tmp := Byte (Shift_Right (W2 and 16#FF00_0000#, 24));
         Info.SD_CSD.Device_Size := Info.SD_CSD.Device_Size or
           Shift_Right (Uint32 (Tmp and 16#C0#), 6);
         Info.SD_CSD.Max_Read_Current_At_VDD_Min :=
           Shift_Right (Tmp and 16#38#, 3);
         Info.SD_CSD.Max_Read_Current_At_VDD_Max :=
           Tmp and 16#07#;

         --  Byte 9
         Tmp := Byte (Shift_Right (W2 and 16#00FF_0000#, 16));
         Info.SD_CSD.Max_Write_Current_At_VDD_Min :=
           Shift_Right (Tmp and 16#E0#, 5);
         Info.SD_CSD.Max_Write_Current_At_VDD_Max :=
           Shift_Right (Tmp and 16#1C#, 2);
         Info.SD_CSD.Device_Size_Multiplier :=
           Shift_Left (Tmp and 16#03#, 2);

         --  Byte 10
         Tmp := Byte (Shift_Right (W2 and 16#0000_FF00#, 8));
         Info.SD_CSD.Device_Size_Multiplier :=
           Info.SD_CSD.Device_Size_Multiplier
           or Shift_Right (Tmp and 16#80#, 7);

         Info.Card_Block_Size :=
           2 ** Natural (Info.SD_CSD.Max_Read_Data_Block_Length);
         Info.Card_Capacity :=
           Unsigned_64 (Info.SD_CSD.Device_Size + 1) *
           2 ** Natural (Info.SD_CSD.Device_Size_Multiplier + 2) *
           Unsigned_64 (Info.Card_Block_Size);

      elsif Info.Card_Type = High_Capacity_SD_Card then
         --  Byte 7
         Tmp := Byte (W1 and 16#0000_00FF#);
         Info.SD_CSD.Device_Size := Shift_Left (Uint32 (Tmp), 16);

         --  Byte 8 & 9
         Info.SD_CSD.Device_Size := Info.SD_CSD.Device_Size or
           (Shift_Right (W2 and 16#FFFF_0000#, 16));

         Info.Card_Capacity :=
           Unsigned_64 (Info.SD_CSD.Device_Size + 1) * 512 * 1024;
         Info.Card_Block_Size := 512;

         --  Byte 10
         Tmp := Byte (Shift_Right (W2 and 16#0000_FF00#, 8));
      else
         --  Unsupported
         null;
         --  return Unsupported_Card;
      end if;

      Info.SD_CSD.Erase_Group_Size := Shift_Right (Tmp and 16#40#, 6);
      Info.SD_CSD.Erase_Group_Size_Multiplier :=
        Shift_Left (Tmp and 16#3F#, 1);

      --  Byte 11
      Tmp := Byte (W2 and 16#0000_00FF#);
      Info.SD_CSD.Erase_Group_Size_Multiplier :=
        Info.SD_CSD.Erase_Group_Size_Multiplier
        or Shift_Right (Tmp and 16#80#, 7);
      Info.SD_CSD.Write_Protect_Group_Size := Tmp and 16#7F#;

      --  Byte 12
      Tmp := Byte (Shift_Right (W3 and 16#FF00_0000#, 24));
      Info.SD_CSD.Write_Protect_Group_Enable := (Tmp and 16#80#) /= 0;
      Info.SD_CSD.Manufacturer_Default_ECC := Shift_Right (Tmp and 16#60#, 5);
      Info.SD_CSD.Write_Speed_Factor := Shift_Right (Tmp and 16#1C#, 2);
      Info.SD_CSD.Max_Write_Data_Block_Length :=
        Shift_Left (Tmp and 16#03#, 2);

      --  Byte 13
      Tmp := Byte (Shift_Right (W3 and 16#00FF_0000#, 16));
      Info.SD_CSD.Max_Write_Data_Block_Length :=
        Info.SD_CSD.Max_Read_Data_Block_Length or
        Shift_Right (Tmp and 16#C0#, 6);
      Info.SD_CSD.Partial_Blocks_For_Write_Allowed := (Tmp and 16#20#) /= 0;
      Info.SD_CSD.Reserved_3 := 0;
      Info.SD_CSD.Content_Protection_Application := (Tmp and 16#01#) /= 0;

      --  Byte 14
      Tmp := Byte (Shift_Right (W3 and 16#0000_FF00#, 8));
      Info.SD_CSD.File_Format_Group := (Tmp and 16#80#) /= 0;
      Info.SD_CSD.Copy_Flag := (Tmp and 16#40#) /= 0;
      Info.SD_CSD.Permanent_Write_Protection := (Tmp and 16#20#) /= 0;
      Info.SD_CSD.Temporary_Write_Protection := (Tmp and 16#10#) /= 0;
      Info.SD_CSD.File_Format := Shift_Right (Tmp and 16#0C#, 2);
      Info.SD_CSD.ECC_Code := Tmp and 16#03#;

      --  Byte 15
      Tmp := Byte (W3 and 16#0000_00FF#);
      Info.SD_CSD.CSD_CRC := Shift_Right (Tmp and 16#FE#, 1);
      Info.SD_CSD.Reserved_4 := 0;
   end Convert_Card_Specific_Data_Register;
end HAL.SDCard;
