with Ada.Unchecked_Conversion;
with Ada.Real_Time;   use Ada.Real_Time;

with STM32_SVD.SDMMC; use STM32_SVD.SDMMC;

package body STM32.SDMMC is

   --  Mask for errors Card Status R1 (OCR Register)
   SD_OCR_ADDR_OUT_OF_RANGE     : constant := 16#8000_0000#;
   SD_OCR_ADDR_MISALIGNED       : constant := 16#4000_0000#;
   SD_OCR_BLOCK_LEN_ERR         : constant := 16#2000_0000#;
   SD_OCR_ERASE_SEQ_ERR         : constant := 16#1000_0000#;
   SD_OCR_BAD_ERASE_PARAM       : constant := 16#0800_0000#;
   SD_OCR_WRITE_PROT_VIOLATION  : constant := 16#0400_0000#;
   SD_OCR_LOCK_UNLOCK_FAILED    : constant := 16#0100_0000#;
   SD_OCR_COM_CRC_FAILED        : constant := 16#0080_0000#;
   SD_OCR_ILLEGAL_CMD           : constant := 16#0040_0000#;
   SD_OCR_CARD_ECC_FAILED       : constant := 16#0020_0000#;
   SD_OCR_CC_ERROR              : constant := 16#0010_0000#;
   SD_OCR_GENERAL_UNKNOWN_ERROR : constant := 16#0008_0000#;
   SD_OCR_STREAM_READ_UNDERRUN  : constant := 16#0004_0000#;
   SD_OCR_STREAM_WRITE_UNDERRUN : constant := 16#0002_0000#;
   SD_OCR_CID_CSD_OVERWRITE     : constant := 16#0001_0000#;
   SD_OCR_WP_ERASE_SKIP         : constant := 16#0000_8000#;
   SD_OCR_CARD_ECC_DISABLED     : constant := 16#0000_4000#;
   SD_OCR_ERASE_RESET           : constant := 16#0000_2000#;
   SD_OCR_AKE_SEQ_ERROR         : constant := 16#0000_0008#;
   SD_OCR_ERRORMASK             : constant := 16#FDFF_E008#;

   --  Masks for R6 responses.
   SD_R6_General_Unknown_Error : constant := 16#0000_2000#;
   SD_R6_Illegal_Cmd           : constant := 16#0000_4000#;
   SD_R6_Com_CRC_Failed        : constant := 16#0000_8000#;

   SD_Voltage_Window_SD        : constant := 16#8010_0000#;
   SD_High_Capacity            : constant := 16#4000_0000#;
   SD_Std_Capacity             : constant := 16#0000_0000#;
   SD_Check_Pattern            : constant := 16#0000_01AA#;

   SD_MAX_VOLT_TRIAL           : constant := 16#0000_FFFF#;

   SD_WIDE_BUS_SUPPORT         : constant := 16#0004_0000#;
   SD_SINGLE_BUS_SUPPORT       : constant := 16#0001_0000#;
   SD_CARD_LOCKED              : constant := 16#0200_0000#;

   SD_DATATIMEOUT              : constant := 16#FFFF_FFFF#;
   SD_0TO7BITS                 : constant := 16#0000_00FF#;
   SD_8TO715ITS                : constant := 16#0000_FF00#;
   SD_16TO23BITS               : constant := 16#00FF_0000#;
   SD_24TO31BITS               : constant := 16#FF00_0000#;

   type SD_SCR is array (1 .. 2) of Word;
   type Data_Direction is (Read, Write);

   procedure Clear_Static_Flags (Controller : in out SDMMC_Controller);

   procedure Send_Command
     (Controller         : in out SDMMC_Controller;
      Command_Index      : SDMMC_Command;
      Argument           : Word;
      Response           : WAITRESP_Field;
      CPSM               : Boolean;
      Wait_For_Interrupt : Boolean);

   procedure Configure_Data
     (Controller         : in out SDMMC_Controller;
      Data_Timeout       : Word; --  Data timeout period in card bus clk period
      Data_Length        : UInt25;
      Data_Block_Size    : DBLOCKSIZE_Field;
      Transfer_Direction : Data_Direction;
      Transfer_Mode      : DTMODE_Field;
      DPSM               : Boolean);

   function Read_FIFO
     (Controller : in out SDMMC_Controller) return Word;

   function Command_Error
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Response_R1_Error
     (Controller    : in out SDMMC_Controller;
      Command_Index : SDMMC_Command) return SD_Error;
   --  Checks for error conditions for R1 response

   function Response_R2_Error
     (Controller : in out SDMMC_Controller) return SD_Error;
   --  Checks for error conditions for R2 (CID or CSD) response.

   function Response_R3_Error
     (Controller : in out SDMMC_Controller) return SD_Error;
   --  Checks for error conditions for R3 (OCR) response.

   function Response_R6_Error
     (Controller    : in out SDMMC_Controller;
      Command_Index : SDMMC_Command;
      RCA           :    out Word) return SD_Error;

   function Response_R7_Error
     (Controller : in out SDMMC_Controller) return SD_Error;
   --  Checks for error conditions for R7 response.

   function SD_Select_Deselect
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Power_On
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Power_Off
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Initialize_Cards
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Read_Card_Info
     (Controller : in out SDMMC_Controller;
      Info       :    out Card_Information) return SD_Error;

   function Find_SCR
     (Controller : in out SDMMC_Controller;
      SCR        :    out SD_SCR) return SD_Error;

   function Stop_Transfer
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Disable_Wide_Bus
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Enable_Wide_Bus
     (Controller : in out SDMMC_Controller) return SD_Error;

   ------------------------
   -- Clear_Static_Flags --
   ------------------------

   procedure Clear_Static_Flags (Controller : in out SDMMC_Controller)
   is
   begin
      Controller.Periph.ICR :=
        (CCRCFAILC => True,
         DCRCFAILC => True,
         CTIMEOUTC => True,
         DTIMEOUTC => True,
         TXUNDERRC => True,
         RXOVERRC  => True,
         CMDRENDC  => True,
         CMDSENTC  => True,
         DATAENDC  => True,
         STBITERRC => True,
         DBCKENDC  => True,
         SDIOITC   => True,
         CEATAENDC => True,
         others    => <>);
   end Clear_Static_Flags;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (Controller         : in out SDMMC_Controller;
      Command_Index      : SDMMC_Command;
      Argument           : Word;
      Response           : WAITRESP_Field;
      CPSM               : Boolean;
      Wait_For_Interrupt : Boolean)
   is
      CMD : CMD_Register  := Controller.Periph.CMD;
   begin
      Controller.Periph.ARG := Argument;
      CMD.CMDINDEX := CMD_CMDINDEX_Field (Command_Index);
      CMD.WAITRESP := Response;
      CMD.WAITINT  := Wait_For_Interrupt;
      CMD.CPSMEN   := CPSM;
      Controller.Periph.CMD := CMD;
   end Send_Command;

   --------------------
   -- Configure_Data --
   --------------------

   procedure Configure_Data
     (Controller         : in out SDMMC_Controller;
      Data_Timeout       : Word; --  Data timeout period in card bus clk period
      Data_Length        : UInt25;
      Data_Block_Size    : DBLOCKSIZE_Field;
      Transfer_Direction : Data_Direction;
      Transfer_Mode      : DTMODE_Field;
      DPSM               : Boolean)
   is
   begin
      Controller.Periph.DTIMER := Data_Timeout;
      Controller.Periph.DLEN.DATALENGTH := Data_Length;
      Controller.Periph.DCTRL :=
        (DTEN       => DPSM,
         DTDIR      => (if Transfer_Direction = Read then Card_To_Controller
                        else Controller_To_Card),
         DTMODE     => Transfer_Mode,
         DBLOCKSIZE => Data_Block_Size,
         others     => <>);
   end Configure_Data;

   ---------------
   -- Read_FIFO --
   ---------------

   function Read_FIFO
     (Controller : in out SDMMC_Controller) return Word
   is
   begin
      return Controller.Periph.FIFO;
   end Read_FIFO;

   -------------------
   -- Command_Error --
   -------------------

   function Command_Error
     (Controller : in out SDMMC_Controller) return SD_Error
   is
      Start : constant Time := Clock;
   begin
      while not Controller.Periph.STA.CMDSENT loop
         if Clock - Start > Milliseconds (1000) then
            return Timeout_Error;
         end if;
      end loop;

      Clear_Static_Flags (Controller);

      return OK;
   end Command_Error;

   -----------------------
   -- Response_R1_Error --
   -----------------------

   function Response_R1_Error
     (Controller    : in out SDMMC_Controller;
      Command_Index : SDMMC_Command) return SD_Error
   is
      Start   : constant Time := Clock;
      Timeout : Boolean := False;
      R1      : Word;
   begin
      while not Controller.Periph.STA.CCRCFAIL
        and then not Controller.Periph.STA.CMDREND
        and then not Controller.Periph.STA.CTIMEOUT
      loop
         if Clock - Start > Milliseconds (1000) then
            Timeout := True;

            exit;
         end if;
      end loop;

      if Timeout or else Controller.Periph.STA.CTIMEOUT then
         --  Card is not v2.0 compliant or card does not support the set
         --  voltage range
         Controller.Periph.ICR.CTIMEOUTC := True;

         return Timeout_Error;

      elsif Controller.Periph.STA.CCRCFAIL then
         Controller.Periph.ICR.CCRCFAILC := True;

         return CRC_Check_Fail;
      end if;

      if SDMMC_Command (Controller.Periph.RESPCMD.RESPCMD) /=
        Command_Index
      then
         return Illegal_Cmd;
      end if;

      Clear_Static_Flags (Controller);

      R1 := Controller.Periph.RESP1;

      if (R1 and SD_OCR_ERRORMASK) = 0 then
         return OK;
      end if;

      if (R1 and SD_OCR_ADDR_OUT_OF_RANGE) /= 0 then
         return Address_Out_Of_Range;
      elsif (R1 and SD_OCR_ADDR_MISALIGNED) /= 0 then
         return Address_Missaligned;
      elsif (R1 and SD_OCR_BLOCK_LEN_ERR) /= 0 then
         return Block_Length_Error;
      elsif (R1 and SD_OCR_ERASE_SEQ_ERR) /= 0 then
         return Erase_Seq_Error;
      elsif (R1 and SD_OCR_BAD_ERASE_PARAM) /= 0 then
         return Bad_Erase_Parameter;
      elsif (R1 and SD_OCR_WRITE_PROT_VIOLATION) /= 0 then
         return Write_Protection_Violation;
      elsif (R1 and SD_OCR_LOCK_UNLOCK_FAILED) /= 0 then
         return Lock_Unlock_Failed;
      elsif (R1 and SD_OCR_COM_CRC_FAILED) /= 0 then
         return CRC_Check_Fail;
      elsif (R1 and SD_OCR_ILLEGAL_CMD) /= 0 then
         return Illegal_Cmd;
      elsif (R1 and SD_OCR_CARD_ECC_FAILED) /= 0 then
         return Card_ECC_Failed;
      elsif (R1 and SD_OCR_CC_ERROR) /= 0 then
         return CC_Error;
      elsif (R1 and SD_OCR_GENERAL_UNKNOWN_ERROR) /= 0 then
         return General_Unknown_Error;
      elsif (R1 and SD_OCR_STREAM_READ_UNDERRUN) /= 0 then
         return Stream_Read_Underrun;
      elsif (R1 and SD_OCR_STREAM_WRITE_UNDERRUN) /= 0 then
         return Stream_Write_Underrun;
      elsif (R1 and SD_OCR_CID_CSD_OVERWRITE) /= 0 then
         return CID_CSD_Overwrite;
      elsif (R1 and SD_OCR_WP_ERASE_SKIP) /= 0 then
         return WP_Erase_Skip;
      elsif (R1 and SD_OCR_CARD_ECC_DISABLED) /= 0 then
         return Card_ECC_Disabled;
      elsif (R1 and SD_OCR_ERASE_RESET) /= 0 then
         return Erase_Reset;
      elsif (R1 and SD_OCR_AKE_SEQ_ERROR) /= 0 then
         return AKE_SEQ_Error;
      else
         return General_Unknown_Error;
      end if;
   end Response_R1_Error;

   -----------------------
   -- Response_R2_Error --
   -----------------------

   function Response_R2_Error
     (Controller : in out SDMMC_Controller) return SD_Error
   is
   begin
      while not Controller.Periph.STA.CCRCFAIL
        and then not Controller.Periph.STA.CMDREND
        and then not Controller.Periph.STA.CTIMEOUT
      loop
         null;
      end loop;

      if Controller.Periph.STA.CTIMEOUT then
         --  Card is not v2.0 compliant or card does not support the set
         --  voltage range
         Controller.Periph.ICR.CTIMEOUTC := True;

         return Timeout_Error;

      elsif Controller.Periph.STA.CCRCFAIL then
         Controller.Periph.ICR.CCRCFAILC := True;

         return CRC_Check_Fail;
      end if;

      Clear_Static_Flags (Controller);

      return OK;
   end Response_R2_Error;

   -----------------------
   -- Response_R3_Error --
   -----------------------

   function Response_R3_Error
     (Controller : in out SDMMC_Controller) return SD_Error
   is
   begin
      while not Controller.Periph.STA.CCRCFAIL
        and then not Controller.Periph.STA.CMDREND
        and then not Controller.Periph.STA.CTIMEOUT
      loop
         null;
      end loop;

      if Controller.Periph.STA.CTIMEOUT then
         --  Card is not v2.0 compliant or card does not support the set
         --  voltage range
         Controller.Periph.ICR.CTIMEOUTC := True;

         return Timeout_Error;
      end if;

      Clear_Static_Flags (Controller);

      return OK;
   end Response_R3_Error;

   -----------------------
   -- Response_R6_Error --
   -----------------------

   function Response_R6_Error
     (Controller    : in out SDMMC_Controller;
      Command_Index : SDMMC_Command;
      RCA           :    out Word) return SD_Error
   is
      Response : Word;
   begin
      while not Controller.Periph.STA.CCRCFAIL
        and then not Controller.Periph.STA.CMDREND
        and then not Controller.Periph.STA.CTIMEOUT
      loop
         null;
      end loop;

      if Controller.Periph.STA.CTIMEOUT then
         --  Card is not v2.0 compliant or card does not support the set
         --  voltage range
         Controller.Periph.ICR.CTIMEOUTC := True;

         return Timeout_Error;

      elsif Controller.Periph.STA.CCRCFAIL then
         Controller.Periph.ICR.CCRCFAILC := True;

         return CRC_Check_Fail;
      end if;

      if SDMMC_Command (Controller.Periph.RESPCMD.RESPCMD) /=
        Command_Index
      then
         return Illegal_Cmd;
      end if;

      Clear_Static_Flags (Controller);

      Response := Controller.Periph.RESP1;

      if (Response and SD_R6_Illegal_Cmd) = SD_R6_Illegal_Cmd then
         return Illegal_Cmd;

      elsif (Response and SD_R6_General_Unknown_Error) =
        SD_R6_General_Unknown_Error
      then
         return General_Unknown_Error;

      elsif (Response and SD_R6_Com_CRC_Failed) = SD_R6_Com_CRC_Failed then
         return CRC_Check_Fail;
      end if;

      RCA := Response and 16#FFFF_0000#;

      return OK;
   end Response_R6_Error;

   -----------------------
   -- Response_R7_Error --
   -----------------------

   function Response_R7_Error
     (Controller : in out SDMMC_Controller) return SD_Error
   is
      Start : constant Time := Clock;
      Timeout : Boolean := False;
   begin
      while not Controller.Periph.STA.CCRCFAIL
        and then not Controller.Periph.STA.CMDREND
        and then not Controller.Periph.STA.CTIMEOUT
      loop
         if Clock - Start > Milliseconds (1000) then
            Timeout := True;

            exit;
         end if;
      end loop;

      if Timeout or else Controller.Periph.STA.CTIMEOUT then
         --  Card is not v2.0 compliant or card does not support the set
         --  voltage range
         Controller.Periph.ICR.CTIMEOUTC := True;

         return Timeout_Error;

      elsif Controller.Periph.STA.CCRCFAIL then
         Controller.Periph.ICR.CCRCFAILC := True;

         return CRC_Check_Fail;

      elsif Controller.Periph.STA.CMDREND then
         Controller.Periph.ICR.CMDRENDC := True;

         return OK;

      else
         return Error;
      end if;
   end Response_R7_Error;

   ------------------------
   -- SD_Select_Deselect --
   ------------------------

   function SD_Select_Deselect
     (Controller : in out SDMMC_Controller) return SD_Error
   is
   begin
      Send_Command
        (Controller,
         Command_Index      => Sel_Desel_Card,
         Argument           => Controller.RCA,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);

      return Response_R1_Error (Controller, Sel_Desel_Card);
   end SD_Select_Deselect;

   --------------
   -- Power_On --
   --------------

   function Power_On
     (Controller : in out SDMMC_Controller) return SD_Error
   is
      Ret           : SD_Error;
      Valid_Voltage : Boolean;
      Card_Type     : Word := SD_Std_Capacity;
      Response      : Word;
   begin
      Controller.Periph.CLKCR.CLKEN := False;

      Controller.Periph.POWER.PWRCTRL := STM32_SVD.SDMMC.Power_On;

      --  1ms: required power up waiting time before starting the SD
      --  initialization sequence
      delay until Clock + Milliseconds (5);

      Controller.Periph.CLKCR.CLKEN := True;

      --  CMD0: Go idle state
      --  no CMD reponse required
      Send_Command (Controller,
                    Command_Index      => Go_Idle_State,
                    Argument           => 0,
                    Response           => No_Response,
                    CPSM               => True,
                    Wait_For_Interrupt => False);

      Ret := Command_Error (Controller);

      if Ret /= OK then
         return Ret;
      end if;

      --  CMD8: Send Interface condition
      --  Send CMD8 to verify SD card interface operating condition
      --  Argument:
      --  - [31:12]: reserved, shall be set to '0'
      --  - [11:8]:  Supply voltage (VHS) 0x1 (range: 2.7-3.6V)
      --  - [7:0]:   Check Pattern (recommended 0xAA)
      Send_Command (Controller,
                    Command_Index      => HS_Send_Ext_CSD,
                    Argument           => SD_Check_Pattern,
                    Response           => Short_Response,
                    CPSM               => True,
                    Wait_For_Interrupt => False);

      Ret := Response_R7_Error (Controller);

      if Ret = OK then
         --  at least SD Card 2.0
         Controller.Card_Type := STD_Capacity_SD_Card_v2_0;
         Card_Type := SD_High_Capacity;

      else
         --  If SD Card, then it's v1.1
         Controller.Card_Type := STD_Capacity_SD_Card_V1_1;
      end if;

      --  Send CMD55
      Send_Command
        (Controller,
         Command_Index      => App_Cmd,
         Argument           => 0,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);

      Ret := Response_R1_Error (Controller, App_Cmd);

      if Ret /= OK then
         Controller.Card_Type := Multimedia_Card;
         --  Only support SD card for now
         return Unsupported_Card;

      else
         --  SD Card case: Send ACMD41 SD_App_Op_Cond with argument
         --  16#8010_0000#
         for J in 1 .. SD_MAX_VOLT_TRIAL loop
            Send_Command
              (Controller,
               Command_Index      => App_Cmd,
               Argument           => 0,
               Response           => Short_Response,
               CPSM               => True,
               Wait_For_Interrupt => False);

            Ret := Response_R1_Error (Controller, App_Cmd);
            if Ret /= OK then
               return Ret;
            end if;

            Send_Command
              (Controller,
               Command_Index      => SD_App_Op_Cond,
               Argument           => SD_Voltage_Window_SD or Card_Type,
               Response           => Short_Response,
               CPSM               => True,
               Wait_For_Interrupt => False);

            Ret := Response_R3_Error (Controller);

            if Ret /= OK then
               return Ret;
            end if;

            Response := Controller.Periph.RESP1;

            if Shift_Right (Response, 31) = 1 then
               Valid_Voltage := True;
               exit;
            end if;
         end loop;

         if not Valid_Voltage then
            return Invalid_Voltage_Range;
         end if;

         if (Response and SD_High_Capacity) = SD_High_Capacity then
            Controller.Card_Type := High_Capacity_SD_Card;
         end if;
      end if;

      return Ret;
   end Power_On;

   ---------------
   -- Power_Off --
   ---------------

   function Power_Off
     (Controller : in out SDMMC_Controller) return SD_Error
   is
   begin
      Controller.Periph.POWER.PWRCTRL := Power_Off;

      return OK;
   end Power_Off;

   ----------------------
   -- Initialize_Cards --
   ----------------------

   function Initialize_Cards
     (Controller : in out SDMMC_Controller) return SD_Error
   is
      SD_RCA : Word;
      Err    : SD_Error;
   begin
      if not Controller.Periph.CLKCR.CLKEN then
         return Request_Not_Applicable;
      end if;

      if Controller.Card_Type /= Secure_Digital_IO_Card then
         Send_Command
           (Controller,
            Command_Index      => All_Send_CID,
            Argument           => 0,
            Response           => Long_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);

         Err := Response_R2_Error (Controller);

         if Err /= OK then
            return Err;
         end if;

         Controller.CID :=
           (Controller.Periph.RESP1,
            Controller.Periph.RESP2,
            Controller.Periph.RESP3,
            Controller.Periph.RESP4);
      end if;

      if Controller.Card_Type = STD_Capacity_SD_Card_V1_1
        or else Controller.Card_Type = STD_Capacity_SD_Card_v2_0
        or else Controller.Card_Type = Secure_Digital_IO_Combo_Card
        or else Controller.Card_Type = High_Capacity_SD_Card
      then
         --  Send CMD3 Set_Rel_Addr with argument 0
         Send_Command
           (Controller,
            Command_Index      => Set_Rel_Addr,
            Argument           => 0,
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);

         Err := Response_R6_Error (Controller, Set_Rel_Addr, SD_RCA);

         if Err /= OK then
            return Err;
         end if;
      end if;

      if Controller.Card_Type /= Secure_Digital_IO_Card then
         Controller.RCA := SD_RCA;

         --  Send CMD9 Send_CSD with argument as card's RCA

         Send_Command
           (Controller,
            Command_Index      => Send_CSD,
            Argument           => SD_RCA,
            Response           => Long_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);

         Err := Response_R2_Error (Controller);

         if Err /= OK then
            return Err;
         end if;

         Controller.CSD :=
           (Controller.Periph.RESP1,
            Controller.Periph.RESP2,
            Controller.Periph.RESP3,
            Controller.Periph.RESP4);
      end if;

      return Err;
   end Initialize_Cards;

   --------------------
   -- Read_Card_Info --
   --------------------

   function Read_Card_Info
     (Controller : in out SDMMC_Controller;
      Info       :    out Card_Information) return SD_Error
   is
      Tmp : Byte;
   begin
      Info.Card_Type := Controller.Card_Type;
      Info.RCA       := Short (Shift_Right (Controller.RCA, 16));

      --  Analysis of CSD Byte 0
      Tmp := Byte (Shift_Right (Controller.CSD (0) and 16#FF00_0000#, 24));
      Info.SD_CSD.CSD_Structure := Shift_Right (Tmp and 16#C0#, 6);
      Info.SD_CSD.System_Specification_Version :=
        Shift_Right (Tmp and 16#3C#, 2);
      Info.SD_CSD.Reserved := Tmp and 16#03#;

      --  Byte 1
      Tmp := Byte (Shift_Right (Controller.CSD (0) and 16#00FF_0000#, 16));
      Info.SD_CSD.Data_Read_Access_Time_1 := Tmp;

      --  Byte 2
      Tmp := Byte (Shift_Right (Controller.CSD (0) and 16#0000_FF00#, 8));
      Info.SD_CSD.Data_Read_Access_Time_2 := Tmp;

      --  Byte 3
      Tmp := Byte (Controller.CSD (0) and 16#0000_00FF#);
      Info.SD_CSD.Max_Bus_Clock_Frequency := Tmp;

      --  Byte 4 & 5
      Info.SD_CSD.Card_Command_Class :=
        Short (Shift_Right (Controller.CSD (1) and 16#FFF0_0000#, 20));
      Info.SD_CSD.Max_Read_Data_Block_Length :=
        Byte (Shift_Right (Controller.CSD (1) and 16#000F_0000#, 16));

      --  Byte 6
      Tmp := Byte (Shift_Right (Controller.CSD (1) and 16#0000_FF00#, 8));
      Info.SD_CSD.Partial_Block_For_Read_Allowed := (Tmp and 16#80#) /= 0;
      Info.SD_CSD.Write_Block_Missalignment := (Tmp and 16#40#) /= 0;
      Info.SD_CSD.Read_Block_Missalignment := (Tmp and 16#20#) /= 0;
      Info.SD_CSD.DSR_Implemented := (Tmp and 16#10#) /= 0;
      Info.SD_CSD.Reserved_2 := 0;

      if Controller.Card_Type = STD_Capacity_SD_Card_V1_1
        or else Controller.Card_Type = STD_Capacity_SD_Card_v2_0
      then
         Info.SD_CSD.Device_Size := Shift_Left (Word (Tmp) and 16#03#, 10);

         --  Byte 7
         Tmp := Byte (Controller.CSD (1) and 16#0000_00FF#);
         Info.SD_CSD.Device_Size := Info.SD_CSD.Device_Size or
           Shift_Left (Word (Tmp), 2);

         --  Byte 8
         Tmp := Byte (Shift_Right (Controller.CSD (2) and 16#FF00_0000#, 24));
         Info.SD_CSD.Device_Size := Info.SD_CSD.Device_Size or
           Shift_Right (Word (Tmp and 16#C0#), 6);
         Info.SD_CSD.Max_Read_Current_At_VDD_Min :=
           Shift_Right (Tmp and 16#38#, 3);
         Info.SD_CSD.Max_Read_Current_At_VDD_Max :=
           Tmp and 16#07#;

         --  Byte 9
         Tmp := Byte (Shift_Right (Controller.CSD (2) and 16#00FF_0000#, 16));
         Info.SD_CSD.Max_Write_Current_At_VDD_Min :=
           Shift_Right (Tmp and 16#E0#, 5);
         Info.SD_CSD.Max_Write_Current_At_VDD_Max :=
           Shift_Right (Tmp and 16#1C#, 2);
         Info.SD_CSD.Device_Size_Multiplier :=
           Shift_Left (Tmp and 16#03#, 2);

         --  Byte 10
         Tmp := Byte (Shift_Right (Controller.CSD (2) and 16#0000_FF00#, 8));
         Info.SD_CSD.Device_Size_Multiplier :=
           Info.SD_CSD.Device_Size_Multiplier or
           Shift_Right (Tmp and 16#80#, 7);

         Info.Card_Block_Size :=
           2 ** Natural (Info.SD_CSD.Max_Read_Data_Block_Length);
         Info.Card_Capacity :=
           Unsigned_64 (Info.SD_CSD.Device_Size + 1) *
           2 ** Natural (Info.SD_CSD.Device_Size_Multiplier + 2) *
           Unsigned_64 (Info.Card_Block_Size);

      elsif Controller.Card_Type = High_Capacity_SD_Card then
         --  Byte 7
         Tmp := Byte (Controller.CSD (1) and 16#0000_00FF#);
         Info.SD_CSD.Device_Size := Shift_Left (Word (Tmp), 16);

         --  Byte 8 & 9
         Info.SD_CSD.Device_Size := Info.SD_CSD.Device_Size or
           (Shift_Right (Controller.CSD (2) and 16#FFFF_0000#, 16));

         Info.Card_Capacity :=
           Unsigned_64 (Info.SD_CSD.Device_Size + 1) * 512 * 1024;
         Info.Card_Block_Size := 512;

         --  Byte 10
         Tmp := Byte (Shift_Right (Controller.CSD (2) and 16#0000_FF00#, 8));
      else
         return Unsupported_Card;
      end if;

      Info.SD_CSD.Erase_Group_Size := Shift_Right (Tmp and 16#40#, 6);
      Info.SD_CSD.Erase_Group_Size_Multiplier :=
        Shift_Left (Tmp and 16#3F#, 1);

      --  Byte 11
      Tmp := Byte (Controller.CSD (2) and 16#0000_00FF#);
      Info.SD_CSD.Erase_Group_Size_Multiplier :=
        Info.SD_CSD.Erase_Group_Size_Multiplier or
        Shift_Right (Tmp and 16#80#, 7);
      Info.SD_CSD.Write_Protect_Group_Size := Tmp and 16#7F#;

      --  Byte 12
      Tmp := Byte (Shift_Right (Controller.CSD (3) and 16#FF00_0000#, 24));
      Info.SD_CSD.Write_Protect_Group_Enable := (Tmp and 16#80#) /= 0;
      Info.SD_CSD.Manufacturer_Default_ECC := Shift_Right (Tmp and 16#60#, 5);
      Info.SD_CSD.Write_Speed_Factor := Shift_Right (Tmp and 16#1C#, 2);
      Info.SD_CSD.Max_Write_Data_Block_Length :=
        Shift_Left (Tmp and 16#03#, 2);

      --  Byte 13
      Tmp := Byte (Shift_Right (Controller.CSD (3) and 16#00FF_0000#, 16));
      Info.SD_CSD.Max_Write_Data_Block_Length :=
        Info.SD_CSD.Max_Read_Data_Block_Length or
        Shift_Right (Tmp and 16#C0#, 6);
      Info.SD_CSD.Partial_Blocks_For_Write_Allowed := (Tmp and 16#20#) /= 0;
      Info.SD_CSD.Reserved_3 := 0;
      Info.SD_CSD.Content_Protection_Application := (Tmp and 16#01#) /= 0;

      --  Byte 14
      Tmp := Byte (Shift_Right (Controller.CSD (3) and 16#0000_FF00#, 8));
      Info.SD_CSD.File_Format_Group := (Tmp and 16#80#) /= 0;
      Info.SD_CSD.Copy_Flag := (Tmp and 16#40#) /= 0;
      Info.SD_CSD.Permanent_Write_Protection := (Tmp and 16#20#) /= 0;
      Info.SD_CSD.Temporary_Write_Protection := (Tmp and 16#10#) /= 0;
      Info.SD_CSD.File_Format := Shift_Right (Tmp and 16#0C#, 2);
      Info.SD_CSD.ECC_Code := Tmp and 16#03#;

      --  Byte 15
      Tmp := Byte (Controller.CSD (3) and 16#0000_00FF#);
      Info.SD_CSD.CSD_CRC := Shift_Right (Tmp and 16#FE#, 1);
      Info.SD_CSD.Reserved_4 := 0;

      --  Byte 0
      Tmp := Byte (Shift_Right (Controller.CID (0) and 16#FF00_0000#, 24));
      Info.SD_CID.Manufacturer_ID := Tmp;

      --  Byte 1 & 2
      Tmp := Byte (Shift_Right (Controller.CID (0) and 16#00FF_0000#, 16));
      Info.SD_CID.OEM_Application_ID (1) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (Controller.CID (0) and 16#0000_FF00#, 8));
      Info.SD_CID.OEM_Application_ID (2) := Character'Val (Tmp);

      --  Byte 3-7
      Tmp := Byte (Controller.CID (0) and 16#0000_00FF#);
      Info.SD_CID.Product_Name (1) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (Controller.CID (1) and 16#FF00_0000#, 24));
      Info.SD_CID.Product_Name (2) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (Controller.CID (1) and 16#00FF_0000#, 16));
      Info.SD_CID.Product_Name (3) := Character'Val (Tmp);
      Tmp := Byte (Shift_Right (Controller.CID (1) and 16#0000_FF00#, 8));
      Info.SD_CID.Product_Name (4) := Character'Val (Tmp);
      Tmp := Byte (Controller.CID (1) and 16#0000_00FF#);
      Info.SD_CID.Product_Name (5) := Character'Val (Tmp);

      --  Byte 8
      Tmp := Byte (Shift_Right (Controller.CID (2) and 16#FF00_0000#, 24));
      Info.SD_CID.Product_Revision.Major := UInt4 (Shift_Right (Tmp, 4));
      Info.SD_CID.Product_Revision.Minor := UInt4 (Tmp and 16#0F#);

      --  Byte 9 - 12
      Info.SD_CID.Product_Serial_Number :=
        Shift_Left (Controller.CID (2) and 16#00FF_FFFF#, 8) or
        Shift_Right (Controller.CID (3) and 16#FF00_0000#, 24);

      --  Byte 13 - 14
      Info.SD_CID.Manufacturing_Date.Month :=
        Manufacturing_Month'Val
          (Shift_Right (Controller.CID (3) and 16#0000_0F00#, 8) - 1);
      Info.SD_CID.Manufacturing_Date.Year :=
        Manufacturing_Year
          (2000 + Shift_Right (Controller.CID (3) and 16#000F_F000#, 12));

      --  Byte 15
      Tmp := Byte (Controller.CID (3) and 16#0000_00FF#);
      Info.SD_CID.CID_CRC := Shift_Right (Tmp and 16#FE#, 1);

      return OK;
   end Read_Card_Info;

   --------------
   -- Find_SCR --
   --------------

   function Find_SCR
     (Controller : in out SDMMC_Controller;
      SCR        :    out SD_SCR) return SD_Error
   is
      Err : SD_Error;
      Idx : Natural := SCR'First;
      Tmp : SD_SCR;
   begin
      Send_Command
        (Controller,
         Command_Index      => Set_Blocklen,
         Argument           => 8,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (Controller, Set_Blocklen);

      if Err /= OK then
         return Err;
      end if;

      Send_Command
        (Controller,
         Command_Index      => App_Cmd,
         Argument           => Controller.RCA,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (Controller, App_Cmd);

      if Err /= OK then
         return Err;
      end if;

      Configure_Data
        (Controller,
         Data_Timeout       => SD_DATATIMEOUT,
         Data_Length        => 8,
         Data_Block_Size    => Block_8B,
         Transfer_Direction => Read,
         Transfer_Mode      => Block,
         DPSM               => True);

      Send_Command
        (Controller,
         Command_Index      => SD_App_Send_SCR,
         Argument           => 0,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (Controller, SD_App_Send_SCR);

      if Err /= OK then
         return Err;
      end if;

      while not Controller.Periph.STA.RXOVERR
        and then not Controller.Periph.STA.DCRCFAIL
        and then not Controller.Periph.STA.DTIMEOUT
        and then not Controller.Periph.STA.DBCKEND
      loop
         if Controller.Periph.STA.RXDAVL then
            Tmp (Idx) := Read_FIFO (Controller);
            exit when Idx = Tmp'Last;
            Idx := Idx + 1;
         end if;
      end loop;

      if Controller.Periph.STA.DTIMEOUT then
         Controller.Periph.ICR.DTIMEOUTC := True;

         return Timeout_Error;

      elsif Controller.Periph.STA.DCRCFAIL then
         Controller.Periph.ICR.DCRCFAILC := True;

         return CRC_Check_Fail;

      elsif Controller.Periph.STA.RXOVERR then
         Controller.Periph.ICR.RXOVERRC := True;

         return Rx_Overrun;
      end if;

      Clear_Static_Flags (Controller);

      --  Translate into LSB
      SCR (1) := Shift_Left (Tmp (2) and SD_0TO7BITS, 24)
        or Shift_Left (Tmp (2) and SD_8TO715ITS, 8)
        or Shift_Right (Tmp (2) and SD_16TO23BITS, 8)
        or Shift_Right (Tmp (2) and SD_24TO31BITS, 24);
      SCR (2) := Shift_Left (Tmp (1) and SD_0TO7BITS, 24)
        or Shift_Left (Tmp (1) and SD_8TO715ITS, 8)
        or Shift_Right (Tmp (1) and SD_16TO23BITS, 8)
        or Shift_Right (Tmp (1) and SD_24TO31BITS, 24);

      return OK;
   end Find_SCR;

   -------------------
   -- Stop_Transfer --
   -------------------

   function Stop_Transfer
     (Controller : in out SDMMC_Controller) return SD_Error
   is
   begin
      Send_Command
        (Controller,
         Command_Index      => Stop_Transmission,
         Argument           => 0,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      return Response_R1_Error (Controller, Stop_Transmission);
   end Stop_Transfer;

   ----------------------
   -- Disable_Wide_Bus --
   ----------------------

   function Disable_Wide_Bus
     (Controller : in out SDMMC_Controller) return SD_Error
   is
      Err : SD_Error := OK;
      SCR : SD_SCR;
   begin
      if (Controller.Periph.RESP1 and SD_CARD_LOCKED) = SD_CARD_LOCKED then
         return Lock_Unlock_Failed;
      end if;

      Err := Find_SCR (Controller, SCR);

      if Err /= OK then
         return Err;
      end if;

      if (SCR (2) and SD_SINGLE_BUS_SUPPORT) /= 0 then
         Send_Command
           (Controller,
            Command_Index      => App_Cmd,
            Argument           => Controller.RCA,
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (Controller, App_Cmd);

         if Err /= OK then
            return Err;
         end if;

         Send_Command
           (Controller,
            Command_Index      => SD_App_Set_Buswidth,
            Argument           => 0,
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (Controller, SD_App_Set_Buswidth);

         if Err /= OK then
            return Err;
         end if;
      else
         return Request_Not_Applicable;
      end if;

      return Err;
   end Disable_Wide_Bus;

   ---------------------
   -- Enable_Wide_Bus --
   ---------------------

   function Enable_Wide_Bus
     (Controller : in out SDMMC_Controller) return SD_Error
   is
      Err : SD_Error := OK;
      SCR : SD_SCR;
   begin
      if (Controller.Periph.RESP1 and SD_CARD_LOCKED) = SD_CARD_LOCKED then
         return Lock_Unlock_Failed;
      end if;

      Err := Find_SCR (Controller, SCR);

      if Err /= OK then
         return Err;
      end if;

      if (SCR (2) and SD_WIDE_BUS_SUPPORT) /= 0 then
         Send_Command
           (Controller,
            Command_Index      => App_Cmd,
            Argument           => Controller.RCA,
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (Controller, App_Cmd);

         if Err /= OK then
            return Err;
         end if;

         Send_Command
           (Controller,
            Command_Index      => SD_App_Set_Buswidth,
            Argument           => 2,
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (Controller, SD_App_Set_Buswidth);

         if Err /= OK then
            return Err;
         end if;
      else
         return Request_Not_Applicable;
      end if;

      return Err;
   end Enable_Wide_Bus;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Controller : in out SDMMC_Controller;
      Info       : out Card_Information) return SD_Error
   is
      Ret : SD_Error;
   begin
      Ret := Power_Off (Controller);

      if Ret /= OK then
         return Ret;
      end if;

      --  Use the Default SDMMC peripheral configuration for SD card init
      Controller.Periph.CLKCR :=
        (CLKDIV         => 16#76#, --  400 kHz max
         --  Clock enable bit
         CLKEN          => False,
         --  Power saving configuration bit
         PWRSAV         => False,
         --  Clock divider bypass enable bit
         BYPASS         => False,
         --  Wide bus mode enable bit
         WIDBUS         => Bus_Wide_1B,
         --  SDIO_CK dephasing selection bit
         NEGEDGE        => Edge_Rising,
         --  HW Flow Control enable
         HWFC_EN        => False,
         others         => <>);

      Ret := Power_On (Controller);

      if Ret /= OK then
         return Ret;
      end if;

      Ret := Initialize_Cards (Controller);

      if Ret /= OK then
         return Ret;
      end if;

      Ret := SD_Select_Deselect (Controller);

      if Ret /= OK then
         return Ret;
      end if;

      --  Now use the card to nominal speed : 25MHz
      Controller.Periph.CLKCR.CLKDIV := 0;
      Clear_Static_Flags (Controller);

      Ret := Read_Card_Info (Controller, Info);

      if Ret /= OK then
         return Ret;
      end if;

      return Ret;
   end Initialize;

   -----------------------------
   -- Configure_Wide_Bus_Mode --
   -----------------------------

   function Configure_Wide_Bus_Mode
     (Controller : in out SDMMC_Controller;
      Wide_Mode  : Wide_Bus_Mode) return SD_Error
   is
      function To_WIDBUS_Field is new Ada.Unchecked_Conversion
        (Wide_Bus_Mode, WIDBUS_Field);
      Err : SD_Error;
   begin
      if Controller.Card_Type = STD_Capacity_SD_Card_V1_1
        or else Controller.Card_Type = STD_Capacity_SD_Card_v2_0
        or else Controller.Card_Type = High_Capacity_SD_Card
      then
         case Wide_Mode is
            when Wide_Bus_1B =>
               Err := Disable_Wide_Bus (Controller);
            when Wide_Bus_4B =>
               Err := Enable_Wide_Bus (Controller);
            when Wide_Bus_8B =>
               return Request_Not_Applicable;
         end case;

         if Err = OK then
            Controller.Periph.CLKCR.WIDBUS := To_WIDBUS_Field (Wide_Mode);
         end if;

      elsif Controller.Card_Type = Multimedia_Card then
         return Unsupported_Card;
      end if;

      return Err;
   end Configure_Wide_Bus_Mode;

   -----------------
   -- Read_Blocks --
   -----------------

   function Read_Blocks
     (Controller : in out SDMMC_Controller;
      Addr       : Unsigned_64;
      Data       : out SD_Data) return SD_Error
   is
      subtype Word_Data is SD_Data (1 .. 4);
      function To_Data is new Ada.Unchecked_Conversion
        (Word, Word_Data);
      R_Addr   : Unsigned_64 := Addr;
      N_Blocks : Positive;
      Err      : SD_Error;
      Idx      : Unsigned_32 := Data'First;
      Dead     : Word with Unreferenced;

   begin
      Controller.Periph.DCTRL := (others => <>);

      if Controller.Card_Type = High_Capacity_SD_Card then
         R_Addr := Addr / 512;
      end if;

      N_Blocks := Data'Length / 512;

      Send_Command
        (Controller,
         Command_Index      => Set_Blocklen,
         Argument           => 512,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (Controller, Set_Blocklen);

      if Err /= OK then
         return Err;
      end if;

      Configure_Data
        (Controller,
         Data_Timeout       => SD_DATATIMEOUT,
         Data_Length        => Data'Length,
         Data_Block_Size    => Block_512B,
         Transfer_Direction => Read,
         Transfer_Mode      => Block,
         DPSM               => True);

      if N_Blocks > 1 then
         Send_Command
           (Controller,
            Command_Index      => Read_Multi_Block,
            Argument           => Word (R_Addr),
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (Controller, Read_Multi_Block);
      else
         Send_Command
           (Controller,
            Command_Index      => Read_Single_Block,
            Argument           => Word (R_Addr),
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (Controller, Read_Single_Block);
      end if;

      if Err /= OK then
         return Err;
      end if;

      if N_Blocks > 1 then
         --  Poll on SDMMC flags
         while not Controller.Periph.STA.RXOVERR
           and then not Controller.Periph.STA.DCRCFAIL
           and then not Controller.Periph.STA.DTIMEOUT
           and then not Controller.Periph.STA.DATAEND
         loop
            if Controller.Periph.STA.RXFIFOHF then
               for J in 1 .. 8 loop
                  Data (Idx .. Idx + 3) :=
                    To_Data (Read_FIFO (Controller));
                  Idx := Idx + 4;
               end loop;
            end if;
         end loop;

      else
         --  Poll on SDMMC flags
         while not Controller.Periph.STA.RXOVERR
           and then not Controller.Periph.STA.DCRCFAIL
           and then not Controller.Periph.STA.DTIMEOUT
           and then not Controller.Periph.STA.DBCKEND
         loop
            if Controller.Periph.STA.RXFIFOHF then
               for J in 1 .. 8 loop
                  Data (Idx .. Idx + 3) :=
                    To_Data (Read_FIFO (Controller));
                  Idx := Idx + 4;
               end loop;
            end if;
         end loop;
      end if;

      if N_Blocks > 1 and then Controller.Periph.STA.DATAEND then
         Err := Stop_Transfer (Controller);
      end if;

      if Controller.Periph.STA.DTIMEOUT then
         Controller.Periph.ICR.DTIMEOUTC := True;
         return Timeout_Error;

      elsif Controller.Periph.STA.DCRCFAIL then
         Controller.Periph.ICR.DCRCFAILC := True;
         return CRC_Check_Fail;

      elsif Controller.Periph.STA.RXOVERR then
         Controller.Periph.ICR.RXOVERRC := True;
         return Rx_Overrun;
      end if;

      for J in Unsigned_32'(1) .. SD_DATATIMEOUT loop
         exit when not Controller.Periph.STA.RXDAVL;
         Dead := Read_FIFO (Controller);
      end loop;

      Clear_Static_Flags (Controller);

      return Err;
   end Read_Blocks;

end STM32.SDMMC;
