------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with System;

package body SDMMC_Init is

   procedure Convert_Card_Identification_Data_Register
     (W0, W1, W2, W3 : UInt32;
      Res            : out Card_Identification_Data_Register);
   --  Convert the R2 reply to CID

   procedure Convert_Card_Specific_Data_Register
     (W0, W1, W2, W3 : UInt32;
      Card_Type : Supported_SD_Memory_Cards;
      CSD : out Card_Specific_Data_Register);
   --  Convert the R2 reply to CSD

   procedure Convert_SDCard_Configuration_Register
     (W0, W1 : UInt32;
      SCR : out SDCard_Configuration_Register);
   --  Convert W0 (MSB) / W1 (LSB) to SCR.

   function Compute_Card_Capacity
     (CSD       : Card_Specific_Data_Register;
      Card_Type : Supported_SD_Memory_Cards) return UInt64;
   --  Compute the card capacity (in bytes) from the CSD

   function Compute_Card_Block_Size
     (CSD       : Card_Specific_Data_Register;
      Card_Type : Supported_SD_Memory_Cards) return UInt32;
   --  Compute the card block size (in bytes) from the CSD.

   function Get_Transfer_Rate
     (CSD : Card_Specific_Data_Register) return Natural;
   --  Compute transfer rate from CSD

   function Swap32 (Val : UInt32) return UInt32 with Inline_Always;

   function BE32_To_Host (Val : UInt32) return UInt32 with Inline_Always;

   --  Swap bytes in a word
   ------------
   -- Swap32 --
   ------------

   function Swap32 (Val : UInt32) return UInt32 is
   begin
      return Shift_Left  (Val and 16#00_00_00_ff#, 24)
        or   Shift_Left  (Val and 16#00_00_ff_00#, 8)
        or   Shift_Right (Val and 16#00_ff_00_00#, 8)
        or   Shift_Right (Val and 16#ff_00_00_00#, 24);
   end Swap32;

   ------------------
   -- BE32_To_Host --
   ------------------

   function BE32_To_Host (Val : UInt32) return UInt32 is
      use System;
   begin
      if Default_Bit_Order = Low_Order_First then
         return Swap32 (Val);
      else
         return Val;
      end if;
   end BE32_To_Host;

   ---------------------------------
   -- Card_Identification_Process --
   ---------------------------------

   procedure Card_Identification_Process
     (This   : in out SDMMC_Driver'Class;
      Info   : out Card_Information;
      Status : out SD_Error)
   is
      Rsp            : UInt32;
      W0, W1, W2, W3 : UInt32;
      Rca            : UInt32;

   begin
      --  Reset controller
      This.Reset (Status);

      if Status /= OK then
         return;
      end if;

      --  CMD0: Sets the SDCard state to Idle
      Send_Cmd (This, Go_Idle_State, 0, Status);

      if Status /= OK then
         return;
      end if;

      --  CMD8: IF_Cond, voltage supplied: 0x1 (2.7V - 3.6V)
      --  It is mandatory for the host compliant to Physical Spec v2.00
      --  to send CMD8 before ACMD41
      Send_Cmd (This, Send_If_Cond, 16#1a5#, Status);

      if Status = OK then
         --  at least SD Card 2.0
         Info.Card_Type := STD_Capacity_SD_Card_v2_0;

         Read_Rsp48 (This, Rsp);

         if (Rsp and 16#fff#) /= 16#1a5# then
            --  Bad voltage or bad pattern.
            Status := Error;
            return;
         end if;
      else
         --  If SD Card, then it's v1.1
         Info.Card_Type := STD_Capacity_SD_Card_V1_1;
      end if;

      for I in 1 .. 5 loop
         This.Delay_Milliseconds (200);

         --  CMD55: APP_CMD
         --  This is done manually to handle error (this command is not
         --  supported by mmc).
         Send_Cmd (This, Cmd_Desc (App_Cmd), 0, Status);

         if Status /= OK then
            if Status = Command_Timeout_Error
              and then I = 1
              and then Info.Card_Type = STD_Capacity_SD_Card_V1_1
            then
               --  Not an SDCard.  Suppose MMC.
               Info.Card_Type := Multimedia_Card;
               exit;
            end if;
            return;
         end if;

         --  ACMD41: SD_SEND_OP_COND (no crc check)
         --  Arg: HCS=1, XPC=0, S18R=0
         Send_Cmd
           (This, Acmd_Desc (SD_App_Send_Op_Cond), 16#40ff_0000#, Status);
         if Status /= OK then
            return;
         end if;

         Read_Rsp48 (This, Rsp);

         if (Rsp and SD_OCR_High_Capacity) = SD_OCR_High_Capacity then
            Info.Card_Type := High_Capacity_SD_Card;
         end if;

         if (Rsp and SD_OCR_Power_Up) = 0 then
            Status := Error;

         else
            Status := OK;
            exit;
         end if;
      end loop;

      if Status = Command_Timeout_Error
        and then Info.Card_Type = Multimedia_Card
      then
         for I in 1 .. 5 loop
            This.Delay_Milliseconds (200);

            --  CMD1: SEND_OP_COND query voltage
            Send_Cmd (This, Cmd_Desc (Send_Op_Cond), 16#00ff_8000#, Status);
            if Status /= OK then
               return;
            end if;

            Read_Rsp48 (This, Rsp);

            if (Rsp and SD_OCR_Power_Up) = 0 then
               Status := Error;
            else
               if (Rsp and 16#00ff_8000#) /= 16#00ff_8000# then
                  Status := Error;
                  return;
               end if;
               Status := OK;
               exit;
            end if;
         end loop;
      end if;

      if Status /= OK then
         return;
      end if;

      --  TODO: Switch voltage

      --  CMD2: ALL_SEND_CID (136 bits)
      Send_Cmd (This, All_Send_CID, 0, Status);
      if Status /= OK then
         return;
      end if;

      Read_Rsp136 (This, W0, W1, W2, W3);
      Convert_Card_Identification_Data_Register (W0, W1, W2, W3, Info.SD_CID);

      --  CMD3: SEND_RELATIVE_ADDR
      case Info.Card_Type is
         when Multimedia_Card =>
            Rca := 16#01_0000#;
         when others =>
            Rca := 0;
      end case;

      Send_Cmd (This, Send_Relative_Addr, Rca, Status);
      if Status /= OK then
         return;
      end if;
      case Info.Card_Type is
         when Multimedia_Card =>
            null;
         when others =>
            Read_Rsp48 (This, Rsp);
            Rca := Rsp and 16#ffff_0000#;
            if (Rsp and 16#e100#) /= 16#0100# then
               return;
            end if;
      end case;
      Info.RCA := UInt16 (Shift_Right (Rca, 16));

      --  Switch to 25Mhz
      case Info.Card_Type is
         when Multimedia_Card =>
            Set_Clock (This, Get_Transfer_Rate (Info.SD_CSD));
         when STD_Capacity_SD_Card_V1_1
           | STD_Capacity_SD_Card_v2_0
           | High_Capacity_SD_Card =>
            Set_Clock (This, 25_000_000);
         when others =>
            --  Not yet handled
            raise Program_Error;
      end case;

      --  CMD10: SEND_CID (136 bits)
      Send_Cmd (This, Send_CID, Rca, Status);
      if Status /= OK then
         return;
      end if;

      --  CMD9: SEND_CSD
      Send_Cmd (This, Send_CSD, Rca, Status);
      if Status /= OK then
         return;
      end if;
      Read_Rsp136 (This, W0, W1, W2, W3);
      Convert_Card_Specific_Data_Register
        (W0, W1, W2, W3, Info.Card_Type, Info.SD_CSD);
      Info.Card_Capacity :=
        Compute_Card_Capacity (Info.SD_CSD, Info.Card_Type);
      Info.Card_Block_Size :=
        Compute_Card_Block_Size (Info.SD_CSD, Info.Card_Type);

      --  CMD7: SELECT
      Send_Cmd (This, Select_Card, Rca, Status);
      if Status /= OK then
         return;
      end if;

      --  CMD13: STATUS
      Send_Cmd (This, Send_Status, Rca, Status);
      if Status /= OK then
         return;
      end if;

      --  Bus size
      case Info.Card_Type is
         when STD_Capacity_SD_Card_V1_1
           | STD_Capacity_SD_Card_v2_0
           | High_Capacity_SD_Card =>
            Send_ACmd (This, SD_App_Set_Bus_Width, Info.RCA, 2, Status);
            if Status /= OK then
               return;
            else
               Set_Bus_Size (This, Wide_Bus_4B);
            end if;
         when others =>
            null;
      end case;

      if (Info.SD_CSD.Card_Command_Class and 2**10) /= 0 then
         --  Class 10 supported.
         declare
            subtype Switch_Status_Type is UInt32_Array (1 .. 16);

            Switch_Status : Switch_Status_Type;
         begin
            --  CMD6
            Read_Cmd (This, Cmd_Desc (Switch_Func), 16#00_fffff0#,
                      Switch_Status, Status);
            if Status /= OK then
               return;
            end if;

            --  Handle endianness
            for I in Switch_Status'Range loop
               Switch_Status (I) := BE32_To_Host (Switch_Status (I));
            end loop;

            --  Switch tp 50Mhz if possible.
            if (Switch_Status (4) and 2**(16 + 1)) /= 0 then
               Read_Cmd (This, Cmd_Desc (Switch_Func), 16#80_fffff1#,
                         Switch_Status, Status);
               if Status /= OK then
                  return;
               end if;

               --  Switch to 50Mhz
               Set_Clock (This, 50_000_000);
            end if;
         end;
      end if;
   end Card_Identification_Process;

   --------------
   -- Read_SCR --
   --------------

   procedure Read_SCR
     (This   : in out SDMMC_Driver'Class;
      Info   : Card_Information;
      SCR    : out SDCard_Configuration_Register;
      Status : out SD_Error)
   is
      subtype SD_SCR is UInt32_Array (1 .. 2);
      Tmp  : SD_SCR;
      Rca  : UInt32;

   begin
      Rca := Shift_Left (UInt32 (Info.RCA), 16);

      Send_Cmd (This, App_Cmd, Rca, Status);
      if Status /= OK then
         return;
      end if;

      Read_Cmd
        (This,
         Cmd    => Acmd_Desc (SD_App_Send_SCR),
         Arg    => 0,
         Buf    => Tmp,
         Status => Status);

      if Status /= OK then
         return;
      end if;

      Convert_SDCard_Configuration_Register
        (BE32_To_Host (Tmp (1)), BE32_To_Host (Tmp (2)), SCR);
   end Read_SCR;

   -----------------------------------------------
   -- Convert_Card_Identification_Data_Register --
   -----------------------------------------------

   procedure Convert_Card_Identification_Data_Register
     (W0, W1, W2, W3 : UInt32;
      Res : out Card_Identification_Data_Register)
   is
      Tmp : UInt8;
   begin
      --  Byte 0
      Tmp := UInt8 (Shift_Right (W0 and 16#FF00_0000#, 24));
      Res.Manufacturer_ID := Tmp;

      --  Byte 1 & 2
      Tmp := UInt8 (Shift_Right (W0 and 16#00FF_0000#, 16));
      Res.OEM_Application_ID (1) := Character'Val (Tmp);
      Tmp := UInt8 (Shift_Right (W0 and 16#0000_FF00#, 8));
      Res.OEM_Application_ID (2) := Character'Val (Tmp);

      --  Byte 3-7
      Tmp := UInt8 (W0 and 16#0000_00FF#);
      Res.Product_Name (1) := Character'Val (Tmp);
      Tmp := UInt8 (Shift_Right (W1 and 16#FF00_0000#, 24));
      Res.Product_Name (2) := Character'Val (Tmp);
      Tmp := UInt8 (Shift_Right (W1 and 16#00FF_0000#, 16));
      Res.Product_Name (3) := Character'Val (Tmp);
      Tmp := UInt8 (Shift_Right (W1 and 16#0000_FF00#, 8));
      Res.Product_Name (4) := Character'Val (Tmp);
      Tmp := UInt8 (W1 and 16#0000_00FF#);
      Res.Product_Name (5) := Character'Val (Tmp);

      --  Byte 8
      Tmp := UInt8 (Shift_Right (W2 and 16#FF00_0000#, 24));
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
      Tmp := UInt8 (W3 and 16#0000_00FF#);
      Res.CID_CRC := Shift_Right (Tmp and 16#FE#, 1);
   end Convert_Card_Identification_Data_Register;

   -----------------------------------------
   -- Convert_Card_Specific_Data_Register --
   -----------------------------------------

   procedure Convert_Card_Specific_Data_Register
     (W0, W1, W2, W3 : UInt32;
      Card_Type      : Supported_SD_Memory_Cards;
      CSD            : out Card_Specific_Data_Register)
   is
      Tmp : UInt8;
   begin
      --  Analysis of CSD Byte 0
      Tmp := UInt8 (Shift_Right (W0 and 16#FF00_0000#, 24));
      CSD.CSD_Structure := Shift_Right (Tmp and 16#C0#, 6);
      CSD.System_Specification_Version := Shift_Right (Tmp and 16#3C#, 2);
      CSD.Reserved := Tmp and 16#03#;

      --  Byte 1
      Tmp := UInt8 (Shift_Right (W0 and 16#00FF_0000#, 16));
      CSD.Data_Read_Access_Time_1 := Tmp;

      --  Byte 2
      Tmp := UInt8 (Shift_Right (W0 and 16#0000_FF00#, 8));
      CSD.Data_Read_Access_Time_2 := Tmp;

      --  Byte 3
      Tmp := UInt8 (W0 and 16#0000_00FF#);
      CSD.Max_Data_Transfer_Rate := Tmp;

      --  Byte 4 & 5
      CSD.Card_Command_Class :=
        UInt16 (Shift_Right (W1 and 16#FFF0_0000#, 20));
      CSD.Max_Read_Data_Block_Length :=
        UInt8 (Shift_Right (W1 and 16#000F_0000#, 16));

      --  Byte 6
      Tmp := UInt8 (Shift_Right (W1 and 16#0000_FF00#, 8));
      CSD.Partial_Block_For_Read_Allowed := (Tmp and 16#80#) /= 0;
      CSD.Write_Block_Missalignment := (Tmp and 16#40#) /= 0;
      CSD.Read_Block_Missalignment := (Tmp and 16#20#) /= 0;
      CSD.DSR_Implemented := (Tmp and 16#10#) /= 0;
      CSD.Reserved_2 := 0;

      if Card_Type = Multimedia_Card
        or else CSD.CSD_Structure = 0
      then
         CSD.Device_Size := Shift_Left (UInt32 (Tmp) and 16#03#, 10);

         --  Byte 7
         Tmp := UInt8 (W1 and 16#0000_00FF#);
         CSD.Device_Size := CSD.Device_Size or Shift_Left (UInt32 (Tmp), 2);

         --  Byte 8
         Tmp := UInt8 (Shift_Right (W2 and 16#FF00_0000#, 24));
         CSD.Device_Size := CSD.Device_Size or
           Shift_Right (UInt32 (Tmp and 16#C0#), 6);
         CSD.Max_Read_Current_At_VDD_Min := Shift_Right (Tmp and 16#38#, 3);
         CSD.Max_Read_Current_At_VDD_Max := Tmp and 16#07#;

         --  Byte 9
         Tmp := UInt8 (Shift_Right (W2 and 16#00FF_0000#, 16));
         CSD.Max_Write_Current_At_VDD_Min := Shift_Right (Tmp and 16#E0#, 5);
         CSD.Max_Write_Current_At_VDD_Max := Shift_Right (Tmp and 16#1C#, 2);
         CSD.Device_Size_Multiplier := Shift_Left (Tmp and 16#03#, 1);

         --  Byte 10
         Tmp := UInt8 (Shift_Right (W2 and 16#0000_FF00#, 8));
         CSD.Device_Size_Multiplier :=
           CSD.Device_Size_Multiplier or Shift_Right (Tmp and 16#80#, 7);
      elsif CSD.CSD_Structure = 1 then
         --  Byte 7
         Tmp := UInt8 (W1 and 16#0000_00FF#);
         CSD.Device_Size := Shift_Left (UInt32 (Tmp), 16);

         --  Byte 8 & 9
         CSD.Device_Size := CSD.Device_Size or
           (Shift_Right (W2 and 16#FFFF_0000#, 16));

         --  Byte 10
         Tmp := UInt8 (Shift_Right (W2 and 16#0000_FF00#, 8));
      else
         --  Unsupported
         null;
         --  return Unsupported_Card;
      end if;

      CSD.Erase_Group_Size := Shift_Right (Tmp and 16#40#, 6);
      CSD.Erase_Group_Size_Multiplier := Shift_Left (Tmp and 16#3F#, 1);

      --  Byte 11
      Tmp := UInt8 (W2 and 16#0000_00FF#);
      CSD.Erase_Group_Size_Multiplier :=
        CSD.Erase_Group_Size_Multiplier or Shift_Right (Tmp and 16#80#, 7);
      CSD.Write_Protect_Group_Size := Tmp and 16#7F#;

      --  Byte 12
      Tmp := UInt8 (Shift_Right (W3 and 16#FF00_0000#, 24));
      CSD.Write_Protect_Group_Enable := (Tmp and 16#80#) /= 0;
      CSD.Manufacturer_Default_ECC := Shift_Right (Tmp and 16#60#, 5);
      CSD.Write_Speed_Factor := Shift_Right (Tmp and 16#1C#, 2);
      CSD.Max_Write_Data_Block_Length := Shift_Left (Tmp and 16#03#, 2);

      --  Byte 13
      Tmp := UInt8 (Shift_Right (W3 and 16#00FF_0000#, 16));
      CSD.Max_Write_Data_Block_Length :=
        CSD.Max_Read_Data_Block_Length or Shift_Right (Tmp and 16#C0#, 6);
      CSD.Partial_Blocks_For_Write_Allowed := (Tmp and 16#20#) /= 0;
      CSD.Reserved_3 := 0;
      CSD.Content_Protection_Application := (Tmp and 16#01#) /= 0;

      --  Byte 14
      Tmp := UInt8 (Shift_Right (W3 and 16#0000_FF00#, 8));
      CSD.File_Format_Group := (Tmp and 16#80#) /= 0;
      CSD.Copy_Flag := (Tmp and 16#40#) /= 0;
      CSD.Permanent_Write_Protection := (Tmp and 16#20#) /= 0;
      CSD.Temporary_Write_Protection := (Tmp and 16#10#) /= 0;
      CSD.File_Format := Shift_Right (Tmp and 16#0C#, 2);
      CSD.ECC_Code := Tmp and 16#03#;

      --  Byte 15
      Tmp := UInt8 (W3 and 16#0000_00FF#);
      CSD.CSD_CRC := Shift_Right (Tmp and 16#FE#, 1);
      CSD.Reserved_4 := 0;
   end Convert_Card_Specific_Data_Register;

   ---------------------------
   -- Compute_Card_Capacity --
   ---------------------------

   function Compute_Card_Capacity
     (CSD       : Card_Specific_Data_Register;
      Card_Type : Supported_SD_Memory_Cards) return UInt64
   is
   begin
      if Card_Type = Multimedia_Card
        or else CSD.CSD_Structure = 0
      then
         return UInt64 (CSD.Device_Size + 1) *
           2 ** Natural (CSD.Device_Size_Multiplier + 2) *
           2 ** Natural (CSD.Max_Read_Data_Block_Length);
      elsif CSD.CSD_Structure = 1 then
         return UInt64 (CSD.Device_Size + 1) * 512 * 1024;
      else
         return 0;
      end if;
   end Compute_Card_Capacity;

   -----------------------------
   -- Compute_Card_Block_Size --
   -----------------------------

   function Compute_Card_Block_Size
     (CSD       : Card_Specific_Data_Register;
      Card_Type : Supported_SD_Memory_Cards) return UInt32
   is
   begin
      if Card_Type = Multimedia_Card
        or else CSD.CSD_Structure = 0
      then
         return 2 ** Natural (CSD.Max_Read_Data_Block_Length);
      elsif CSD.CSD_Structure = 1 then
         return 512;
      else
         return 0;
      end if;
   end Compute_Card_Block_Size;

   -------------------------------------------
   -- Convert_SDCard_Configuration_Register --
   -------------------------------------------

   procedure Convert_SDCard_Configuration_Register
     (W0, W1 : UInt32;
      SCR    : out SDCard_Configuration_Register)
   is
   begin
      SCR := (SCR_Structure         => UInt8 (Shift_Right (W0, 28) and 16#f#),
              SD_Spec               => UInt8 (Shift_Right (W0, 24) and 16#f#),
              Data_Stat_After_Erase => UInt8 (Shift_Right (W0, 23) and 1),
              SD_Security           => UInt8 (Shift_Right (W0, 20) and 7),
              SD_Bus_Widths         => UInt8 (Shift_Right (W0, 16) and 16#f#),
              SD_Spec3              => (Shift_Right (W0, 15) and 1) /= 0,
              Ex_Security           => UInt8 (Shift_Right (W0, 11) and 16#f#),
              SD_Spec4              => (Shift_Right (W0, 10) and 1) /= 0,
              Reserved_1            => UInt8 (Shift_Right (W0, 4) and 16#3f#),
              CMD_Support           => UInt8 (Shift_Right (W0, 0) and 16#f#),
              Reserved_2            => W1);
   end Convert_SDCard_Configuration_Register;

   -----------------------
   -- Get_Transfer_Rate --
   -----------------------

   function Get_Transfer_Rate
     (CSD : Card_Specific_Data_Register) return Natural
   is
      Res : Natural;
   begin
      case Shift_Right (CSD.Max_Data_Transfer_Rate, 3) and 15 is
         when 16#1# => Res := 10;
         when 16#2# => Res := 12;
         when 16#3# => Res := 13;
         when 16#4# => Res := 15;
         when 16#5# => Res := 20;
         when 16#6# => Res := 25;
         when 16#7# => Res := 30;
         when 16#8# => Res := 35;
         when 16#9# => Res := 40;
         when 16#a# => Res := 45;
         when 16#b# => Res := 50;
         when 16#c# => Res := 55;
         when 16#d# => Res := 60;
         when 16#e# => Res := 70;
         when 16#f# => Res := 80;
         when others => return 400_000;
      end case;

      case CSD.Max_Data_Transfer_Rate and 7 is
         when 0 => return Res * 100_000 / 10;
         when 1 => return Res * 1_000_000 / 10;
         when 2 => return Res * 10_000_000 / 10;
         when 3 => return Res * 100_000_000 / 10;
         when others => return 400_000;
      end case;
   end Get_Transfer_Rate;

end SDMMC_Init;
