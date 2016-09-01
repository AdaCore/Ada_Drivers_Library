with Ada.Unchecked_Conversion;
with Ada.Real_Time;   use Ada.Real_Time;
with System;          use System;
with System.Machine_Code;

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

   procedure Send_Command
     (Controller         : in out SDMMC_Controller;
      Command_Index      : SDMMC_Command;
      Argument           : Word;
      Response           : CMD_WAITRESP_Field;
      CPSM               : Boolean;
      Wait_For_Interrupt : Boolean);

   procedure Configure_Data
     (Controller         : in out SDMMC_Controller;
      Data_Length        : UInt25;
      Data_Block_Size    : DCTRL_DBLOCKSIZE_Field;
      Transfer_Direction : Data_Direction;
      Transfer_Mode      : DCTRL_DTMODE_Field;
      DPSM               : Boolean;
      DMA_Enabled        : Boolean);

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

   function Disable_Wide_Bus
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Enable_Wide_Bus
     (Controller : in out SDMMC_Controller) return SD_Error;

   procedure DCTRL_Write_Delay with Inline_Always;
   --  The DCFGR register cannot be written 2 times in a row: we need to
   --  wait 3 48MHz periods + 2 90MHz periods. So instead of inserting a 1ms
   --  delay statement (which would be overkill), we just issue a few
   --  nop instructions to let the CPU wait this period.

   procedure DCTRL_Write_Delay
   is
      use System.Machine_Code;
   begin
      for J in 1 .. 30 loop
         Asm ("nop", Volatile => True);
      end loop;
   end DCTRL_Write_Delay;

   ------------------------
   -- Clear_Static_Flags --
   ------------------------

   procedure Clear_Static_Flags (This : in out SDMMC_Controller)
   is
   begin
      This.Periph.ICR :=
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

   ----------------
   -- Clear_Flag --
   ----------------

   procedure Clear_Flag
     (This : in out SDMMC_Controller;
      Flag       : SDMMC_Clearable_Flags)
   is
   begin
      case Flag is
         when Data_End =>
            This.Periph.ICR.DATAENDC  := True;
         when Data_CRC_Fail =>
            This.Periph.ICR.DCRCFAILC := True;
         when Data_Timeout =>
            This.Periph.ICR.DTIMEOUTC := True;
         when RX_Overrun =>
            This.Periph.ICR.RXOVERRC  := True;
         when TX_Underrun =>
            This.Periph.ICR.TXUNDERRC := True;
      end case;
   end Clear_Flag;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This : in out SDMMC_Controller;
      Interrupt  : SDMMC_Interrupts)
   is
   begin
      case Interrupt is
         when Data_End_Interrupt =>
            This.Periph.MASK.DATAENDIE   := True;
         when Data_CRC_Fail_Interrupt =>
            This.Periph.MASK.DCRCFAILIE  := True;
         when Data_Timeout_Interrupt =>
            This.Periph.MASK.DTIMEOUTIE  := True;
         when TX_FIFO_Empty_Interrupt =>
            This.Periph.MASK.TXFIFOEIE   := True;
         when RX_FIFO_Full_Interrupt =>
            This.Periph.MASK.RXFIFOFIE   := True;
         when TX_Underrun_Interrupt =>
            This.Periph.MASK.TXUNDERRIE  := True;
         when RX_Overrun_Interrupt =>
            This.Periph.MASK.RXOVERRIE   := True;
      end case;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This : in out SDMMC_Controller;
      Interrupt  : SDMMC_Interrupts)
   is
   begin
      case Interrupt is
         when Data_End_Interrupt =>
            This.Periph.MASK.DATAENDIE   := False;
         when Data_CRC_Fail_Interrupt =>
            This.Periph.MASK.DCRCFAILIE  := False;
         when Data_Timeout_Interrupt =>
            This.Periph.MASK.DTIMEOUTIE  := False;
         when TX_FIFO_Empty_Interrupt =>
            This.Periph.MASK.TXFIFOEIE   := False;
         when RX_FIFO_Full_Interrupt =>
            This.Periph.MASK.RXFIFOFIE   := False;
         when TX_Underrun_Interrupt =>
            This.Periph.MASK.TXUNDERRIE  := False;
         when RX_Overrun_Interrupt =>
            This.Periph.MASK.RXOVERRIE   := False;
      end case;
   end Disable_Interrupt;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (Controller         : in out SDMMC_Controller;
      Command_Index      : SDMMC_Command;
      Argument           : Word;
      Response           : CMD_WAITRESP_Field;
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
      Data_Length        : UInt25;
      Data_Block_Size    : DCTRL_DBLOCKSIZE_Field;
      Transfer_Direction : Data_Direction;
      Transfer_Mode      : DCTRL_DTMODE_Field;
      DPSM               : Boolean;
      DMA_Enabled        : Boolean)
   is
      Tmp : DCTRL_Register;
   begin
      Controller.Periph.DLEN.DATALENGTH  := Data_Length;

      --  DCTRL cannot be written during 3 SDMMCCLK (48MHz) clock periods
      --  Minimum wait time is 1 Milliseconds, so let's do that
      DCTRL_Write_Delay;

      Tmp := Controller.Periph.DCTRL;
      Tmp.DTDIR      :=
        (if Transfer_Direction = Read then Card_To_Controller
         else Controller_To_Card);
      Tmp.DTMODE     := Transfer_Mode;
      Tmp.DBLOCKSIZE := Data_Block_Size;
      Tmp.DTEN       := DPSM;
      Tmp.DMAEN      := DMA_Enabled;
      Controller.Periph.DCTRL := Tmp;
   end Configure_Data;

   ------------------
   -- Disable_Data --
   ------------------

   procedure Disable_Data
     (This : in out SDMMC_Controller)
   is
   begin
      This.Periph.DCTRL := (others => <>);
   end Disable_Data;

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

      Controller.Periph.POWER.PWRCTRL := Power_On;

      --  power up waiting time before starting the SD
      --  initialization sequence
      DCTRL_Write_Delay;

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
      Err  : SD_Error;
      Idx  : Natural;
      Tmp  : SD_SCR;
      Dead : Unsigned_32 with Unreferenced;

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
         Data_Length        => 8,
         Data_Block_Size    => Block_8B,
         Transfer_Direction => Read,
         Transfer_Mode      => Block,
         DPSM               => True,
         DMA_Enabled        => False);

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

      Idx := Tmp'First;

      while not Controller.Periph.STA.RXOVERR
        and then not Controller.Periph.STA.DCRCFAIL
        and then not Controller.Periph.STA.DTIMEOUT
        and then not Controller.Periph.STA.DBCKEND
      loop
         while Controller.Periph.STA.RXDAVL loop
            if Idx <= Tmp'Last then
               Tmp (Idx) := Read_FIFO (Controller);
               Idx := Idx + 1;
            else
               --  Flush the FIFO
               Dead := Read_FIFO (Controller);
            end if;
         end loop;
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
     (This : in out SDMMC_Controller) return SD_Error
   is
   begin
      Send_Command
        (This,
         Command_Index      => Stop_Transmission,
         Argument           => 0,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      return Response_R1_Error (This, Stop_Transmission);
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
     (This : in out SDMMC_Controller;
      Info       : out Card_Information) return SD_Error
   is
      Ret : SD_Error;
   begin
      Ret := Power_Off (This);

      if Ret /= OK then
         return Ret;
      end if;

      --  Make sure the POWER register is writable by waiting a bit after
      --  the Power_Off command
      DCTRL_Write_Delay;

      --  Use the Default SDMMC peripheral configuration for SD card init
      This.Periph.CLKCR :=
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
         NEGEDGE        => Edge_Rising, -- Errata sheet STM: NEGEDGE=1 (falling) should *not* be used
         --  HW Flow Control enable
         HWFC_EN        => False, -- Errata sheet STM: glitches => DCRCFAIL asserted. Do not use.
         others         => <>);

      This.Periph.DTIMER := SD_DATATIMEOUT;

      Ret := Power_On (This);

      if Ret /= OK then
         return Ret;
      end if;

      delay until Clock +  Milliseconds (50);

      Ret := Initialize_Cards (This);

      if Ret /= OK then
         return Ret;
      end if;

      Ret := SD_Select_Deselect (This);

      if Ret /= OK then
         return Ret;
      end if;

      --  Now use the card to nominal speed : 25MHz
      --  Make sure CLKCR is writable by waiting a bit for the previous write
      --  to propagate if needed
      DCTRL_Write_Delay;
      This.Periph.CLKCR.CLKDIV := 0;
      Clear_Static_Flags (This);

      Ret := Read_Card_Info (This, Info);

      if Ret /= OK then
         return Ret;
      end if;

      Ret := Configure_Wide_Bus_Mode (This, Wide_Bus_4B);

      return Ret;
   end Initialize;

   -----------------------------
   -- Configure_Wide_Bus_Mode --
   -----------------------------

   function Configure_Wide_Bus_Mode
     (This : in out SDMMC_Controller;
      Wide_Mode  : Wide_Bus_Mode) return SD_Error
   is
      function To_WIDBUS_Field is new Ada.Unchecked_Conversion
        (Wide_Bus_Mode, CLKCR_WIDBUS_Field);
      Err : SD_Error;
   begin
      if This.Card_Type = STD_Capacity_SD_Card_V1_1
        or else This.Card_Type = STD_Capacity_SD_Card_v2_0
        or else This.Card_Type = High_Capacity_SD_Card
      then
         case Wide_Mode is
            when Wide_Bus_1B =>
               Err := Disable_Wide_Bus (This);
            when Wide_Bus_4B =>
               Err := Enable_Wide_Bus (This);
            when Wide_Bus_8B =>
               return Request_Not_Applicable;
         end case;

         if Err = OK then
            This.Periph.CLKCR.WIDBUS := To_WIDBUS_Field (Wide_Mode);
         end if;

      elsif This.Card_Type = Multimedia_Card then
         return Unsupported_Card;
      end if;

      return Err;
   end Configure_Wide_Bus_Mode;

   -----------------
   -- Read_Blocks --
   -----------------

   function Read_Blocks
     (This : in out SDMMC_Controller;
      Addr : Unsigned_64;
      Data : out SD_Data) return SD_Error
   is
      subtype Word_Data is SD_Data (1 .. 4);
      function To_Data is new Ada.Unchecked_Conversion
        (Word, Word_Data);
      R_Addr   : Unsigned_64 := Addr;
      N_Blocks : Positive;
      Err      : SD_Error;
      Idx      : Unsigned_16 := Data'First;
      Dead     : Word with Unreferenced;

   begin
      DCTRL_Write_Delay;
      This.Periph.DCTRL := (others => <>);

      if This.Card_Type = High_Capacity_SD_Card then
         R_Addr := Addr / 512;
      end if;

      N_Blocks := Data'Length / 512;

      Send_Command
        (This,
         Command_Index      => Set_Blocklen,
         Argument           => 512,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (This, Set_Blocklen);

      if Err /= OK then
         return Err;
      end if;

      Configure_Data
        (This,
         Data_Length        => Data'Length,
         Data_Block_Size    => Block_512B,
         Transfer_Direction => Read,
         Transfer_Mode      => Block,
         DPSM               => True,
         DMA_Enabled        => False);

      if N_Blocks > 1 then
         This.Operation := Read_Multiple_Blocks_Operation;
         Send_Command
           (This,
            Command_Index      => Read_Multi_Block,
            Argument           => Word (R_Addr),
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (This, Read_Multi_Block);
      else
         This.Operation := Read_Single_Block_Operation;
         Send_Command
           (This,
            Command_Index      => Read_Single_Block,
            Argument           => Word (R_Addr),
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (This, Read_Single_Block);
      end if;

      if Err /= OK then
         return Err;
      end if;

      if N_Blocks > 1 then
         --  Poll on SDMMC flags
         while not This.Periph.STA.RXOVERR
           and then not This.Periph.STA.DCRCFAIL
           and then not This.Periph.STA.DTIMEOUT
           and then not This.Periph.STA.DATAEND
         loop
            if This.Periph.STA.RXFIFOHF then
               for J in 1 .. 8 loop
                  Data (Idx .. Idx + 3) :=
                    To_Data (Read_FIFO (This));
                  Idx := Idx + 4;
               end loop;
            end if;
         end loop;

      else
         --  Poll on SDMMC flags
         while not This.Periph.STA.RXOVERR
           and then not This.Periph.STA.DCRCFAIL
           and then not This.Periph.STA.DTIMEOUT
           and then not This.Periph.STA.DBCKEND
         loop
            if This.Periph.STA.RXFIFOHF then
               for J in 1 .. 8 loop
                  Data (Idx .. Idx + 3) :=
                    To_Data (Read_FIFO (This));
                  Idx := Idx + 4;
               end loop;
            end if;
         end loop;
      end if;

      if N_Blocks > 1 and then This.Periph.STA.DATAEND then
         Err := Stop_Transfer (This);
      end if;

      if This.Periph.STA.DTIMEOUT then
         This.Periph.ICR.DTIMEOUTC := True;
         return Timeout_Error;

      elsif This.Periph.STA.DCRCFAIL then
         This.Periph.ICR.DCRCFAILC := True;
         return CRC_Check_Fail;

      elsif This.Periph.STA.RXOVERR then
         This.Periph.ICR.RXOVERRC := True;
         return Rx_Overrun;

      elsif This.Periph.STA.STBITERR then
         This.Periph.ICR.STBITERRC := True;
         return Startbit_Not_Detected;

      end if;

      for J in Unsigned_32'(1) .. SD_DATATIMEOUT loop
         exit when not This.Periph.STA.RXDAVL;
         Dead := Read_FIFO (This);
      end loop;

      Clear_Static_Flags (This);

      return Err;
   end Read_Blocks;

   ---------------------
   -- Read_Blocks_DMA --
   ---------------------

   function Read_Blocks_DMA
     (This : in out SDMMC_Controller;
      Addr       : Unsigned_64;
      DMA        : STM32.DMA.DMA_Controller;
      Stream     : STM32.DMA.DMA_Stream_Selector;
      Data       : out SD_Data) return SD_Error
   is
      Read_Address : constant Unsigned_64 :=
                       (if This.Card_Type = High_Capacity_SD_Card
                        then Addr / 512 else Addr);

      Data_Len_Bytes : constant Natural := (Data'Length / 512) * 512;
      Data_Len_Words : constant Natural := Data_Len_Bytes / 4;
      N_Blocks       : constant Natural := Data_Len_Bytes / 512;
      Data_Addr      : constant Address := Data (Data'First)'Address;

      Err            : SD_Error;
      Command        : SDMMC_Command;
      use STM32.DMA;
   begin
      if not STM32.DMA.Compatible_Alignments
        (DMA,
         Stream,
         This.Periph.FIFO'Address,
         Data_Addr)
      then
         return DMA_Alignment_Error;
      end if;

      --  After a data write, data cannot be written to this register
      --  for three SDMMCCLK (@ 48 MHz) clock periods plus two PCLK2 clock
      --  periods (@ ~90MHz).
      --  So here we make sure the DCTRL is writable
      DCTRL_Write_Delay;
      This.Periph.DCTRL := (DTEN   => False,
                            others => <>);

      --  switch to nominal speed, in case polling was active before
      This.Periph.CLKCR.CLKDIV := 16#0#;

      Enable_Interrupt (This, Data_CRC_Fail_Interrupt);
      Enable_Interrupt (This, Data_Timeout_Interrupt);
      Enable_Interrupt (This, Data_End_Interrupt);
      Enable_Interrupt (This, RX_Overrun_Interrupt);

      STM32.DMA.Start_Transfer_with_Interrupts
        (This               => DMA,
         Stream             => Stream,
         Source             => This.Periph.FIFO'Address,
         Destination        => Data_Addr,
         Data_Count         => Unsigned_16 (Data_Len_Words), -- because DMA is set up with words
         Enabled_Interrupts => (Transfer_Error_Interrupt    => True,
                                FIFO_Error_Interrupt        => True,
                                Transfer_Complete_Interrupt => True,
                                others                      => False));

      Send_Command
        (This,
         Command_Index      => Set_Blocklen,
         Argument           => 512,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (This, Set_Blocklen);

      if Err /= OK then
         return Err;
      end if;

      Configure_Data
        (This,
         Data_Length        => UInt25 (N_Blocks) * 512,
         Data_Block_Size    => Block_512B,
         Transfer_Direction => Read,
         Transfer_Mode      => Block,
         DPSM               => True,
         DMA_Enabled        => True);

      if N_Blocks > 1 then
         Command := Read_Multi_Block;
         This.Operation := Read_Multiple_Blocks_Operation;
      else
         Command := Read_Single_Block;
         This.Operation := Read_Single_Block_Operation;
      end if;

      Send_Command
        (This,
         Command_Index      => Command,
         Argument           => Word (Read_Address),
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (This, Command);

      return Err;
   end Read_Blocks_DMA;

   ---------------------
   --  Write_Blocks_DMA
   ---------------------

   function Write_Blocks_DMA
     (This : in out SDMMC_Controller;
      Addr       : Unsigned_64;
      DMA        : STM32.DMA.DMA_Controller;
      Stream     : STM32.DMA.DMA_Stream_Selector;
      Data       : SD_Data) return SD_Error
   is
      Write_Address : constant Unsigned_64 :=
                       (if This.Card_Type = High_Capacity_SD_Card
                        then Addr / 512 else Addr); -- 512 is the min. block size of SD 2.0 card

      Data_Len_Bytes : constant Natural := (Data'Length / 512) * 512;
      Data_Len_Words : constant Natural := Data_Len_Bytes / 4;
      N_Blocks       : constant Natural := Data_Len_Bytes / 512;
      Data_Addr      : constant Address := Data (Data'First)'Address;

      Err        : SD_Error;
      cardstatus : HAL.Word;
      start      : constant Time := Clock;
      Timeout    : Boolean := False;
      Command    : SDMMC_Command;

      use STM32.DMA;
   begin

      if not STM32.DMA.Compatible_Alignments (DMA,
                                              Stream,
                                              This.Periph.FIFO'Address,
                                              Data_Addr)
      then
         return DMA_Alignment_Error;
      end if;

      DCTRL_Write_Delay;
      This.Periph.DCTRL := (DTEN   => False,
                                  others => <>);
      --  After a data write, data cannot be written to this register
      --  for three SDMMCCLK (48 MHz) clock periods plus two PCLK2 clock
      --  periods.
      DCTRL_Write_Delay;

      Clear_Static_Flags (This);

      --  wait until card is ready for data added
      Wait_Ready_Loop :
      loop
         declare
            now : constant Time := Clock;
         begin
            if now - start > Milliseconds (100) then
               Timeout := True;
               exit Wait_Ready_Loop;
            end if;
         end;

         Send_Command
           (This,
            Command_Index      => Send_Status,
            Argument           => This.RCA,
            Response           => Short_Response,
            CPSM               => True,
            Wait_For_Interrupt => False);
         Err := Response_R1_Error (This, Send_Status);

         if Err /= OK then
            return Err;
         end if;

         cardstatus := This.Periph.RESP1;
         exit Wait_Ready_Loop when (cardstatus and 16#100#) /= 0;
      end loop Wait_Ready_Loop;

      if Timeout then
         return Timeout_Error;
      end if;

      Enable_Interrupt (This, Data_CRC_Fail_Interrupt);
      Enable_Interrupt (This, Data_Timeout_Interrupt);
      Enable_Interrupt (This, Data_End_Interrupt);
      Enable_Interrupt (This, TX_Underrun_Interrupt);

      --  start DMA first (gives time to setup)
      STM32.DMA.Start_Transfer_with_Interrupts
        (This               => DMA,
         Stream             => Stream,
         Destination        => This.Periph.FIFO'Address,
         Source             => Data_Addr,
         Data_Count         => Unsigned_16 (Data_Len_Words), -- DMA uses words
         Enabled_Interrupts => (Transfer_Error_Interrupt    => True,
                                FIFO_Error_Interrupt        => True,
                                Transfer_Complete_Interrupt => True,
                                others                      => False));

      --  set block size
      Send_Command
        (This,
         Command_Index      => Set_Blocklen,
         Argument           => 512,
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (This, Set_Blocklen);

      if Err /= OK then
         return Err;
      end if;

      --  set write address & single/multi mode
      if N_Blocks > 1 then
         Command := Write_Multi_Block;
         This.Operation := Write_Multiple_Blocks_Operation;
      else
         Command := Write_Single_Block;
         This.Operation := Write_Single_Block_Operation;
      end if;
      Send_Command
        (This,
         Command_Index      => Command,
         Argument           => Word (Write_Address),
         Response           => Short_Response,
         CPSM               => True,
         Wait_For_Interrupt => False);
      Err := Response_R1_Error (This, Command);
      --  according to RM0090 we should wait for SDIO_STA[6] = CMDREND
      --  interrupt, which is this:
      if Err /= OK then
         return Err;
      end if;

      --  and now enable the card with DTEN, which is this:
      Configure_Data
        (This,
         Data_Length        => UInt25 (N_Blocks) * 512,
         Data_Block_Size    => Block_512B,
         Transfer_Direction => Write,
         Transfer_Mode      => Block,
         DPSM               => True,
         DMA_Enabled        => True);

      --  according to RM0090: wait for STA[10]=DBCKEND
      --  check that no channels are still enabled by polling DMA Enabled
      --  Channel Status Reg

      return Err;
   end Write_Blocks_DMA;

   -------------------------
   -- Get_Transfer_Status --
   -------------------------

   function Get_Transfer_Status
     (This : in out SDMMC_Controller) return SD_Error
   is
   begin
      if This.Periph.STA.DTIMEOUT then
         This.Periph.ICR.DTIMEOUTC := True;
         return Timeout_Error;

      elsif This.Periph.STA.DCRCFAIL then
         This.Periph.ICR.DCRCFAILC := True; -- clear
         return CRC_Check_Fail;

      elsif This.Periph.STA.TXUNDERR then
         This.Periph.ICR.TXUNDERRC := True;
         return Tx_Underrun;

      elsif This.Periph.STA.STBITERR then
         This.Periph.ICR.STBITERRC := True;
         return Startbit_Not_Detected;

      elsif This.Periph.STA.RXOVERR then
         This.Periph.ICR.RXOVERRC := True;
         return Rx_Overrun;
      end if;

      return OK;
   end Get_Transfer_Status;

end STM32.SDMMC;
