------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Real_Time;       use Ada.Real_Time;
with System;              use System;
with System.Machine_Code;

with STM32_SVD.RCC; use STM32_SVD.RCC;
pragma Warnings (Off);
--  Shut down the warning on internal unit usage
with System.BB.Board_Parameters;
pragma Warnings (On);

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

   SD_DATATIMEOUT              : constant := 16#FFFF_FFFF#;

   procedure Configure_Data
     (Controller         : in out SDMMC_Controller;
      Data_Length        : UInt25;
      Data_Block_Size    : DCTRL_DBLOCKSIZE_Field;
      Transfer_Direction : Data_Direction;
      Transfer_Mode      : DCTRL_DTMODE_Field;
      DPSM               : Boolean;
      DMA_Enabled        : Boolean);

   function Read_FIFO
     (Controller : in out SDMMC_Controller) return UInt32;

   function Response_R1_Error
     (Controller    : in out SDMMC_Controller;
      Command_Index : SD_Command) return SD_Error;
   --  Checks for error conditions for R1 response

   function Response_R2_Error
     (Controller : in out SDMMC_Controller) return SD_Error;
   --  Checks for error conditions for R2 (CID or CSD) response.

   function Response_R3_Error
     (Controller : in out SDMMC_Controller) return SD_Error;
   --  Checks for error conditions for R3 (OCR) response.

   function Response_R6_Error
     (Controller    : in out SDMMC_Controller;
      Command_Index : SD_Command;
      RCA           :    out UInt32) return SD_Error;

   function Response_R7_Error
     (Controller : in out SDMMC_Controller) return SD_Error;
   --  Checks for error conditions for R7 response.

   procedure DCTRL_Write_Delay with Inline_Always;
   --  The DCFGR register cannot be written 2 times in a row: we need to
   --  wait 3 48MHz periods + 2 90MHz periods. So instead of inserting a 1ms
   --  delay statement (which would be overkill), we just issue a few
   --  nop instructions to let the CPU wait this period.

   -----------------------
   -- DCTRL_Write_Delay --
   -----------------------

   procedure DCTRL_Write_Delay
   is
      use System.Machine_Code;
   begin
      for J in 1 .. 20 loop
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

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (This   : in out SDMMC_Controller;
      Status : out SD_Error)
   is
   begin
      --  Make sure the POWER register is writable by waiting a bit after
      --  the Power_Off command
      DCTRL_Write_Delay;

      This.Periph.POWER.PWRCTRL := Power_Off;

      --  Use the Default SDMMC peripheral configuration for SD card init
      This.Periph.CLKCR := (others => <>);
      This.Set_Clock (400_000);
      This.Periph.DTIMER := SD_DATATIMEOUT;

      This.Periph.CLKCR.CLKEN   := False;
      DCTRL_Write_Delay;
      This.Periph.POWER.PWRCTRL := Power_On;

      --  Wait for the clock to stabilize.
      DCTRL_Write_Delay;

      This.Periph.CLKCR.CLKEN := True;

      delay until Clock + Milliseconds (20);

      Status := OK;
   end Reset;

   ---------------
   -- Set_Clock --
   ---------------

   overriding procedure Set_Clock
     (This   : in out SDMMC_Controller;
      Freq   : Natural)
   is
      use System.BB.Board_Parameters;

      Div : Natural;
   begin
      Div := (This.CLK_In + Freq - 1) / Freq;

      --  Make sure the POWER register is writable by waiting a bit after
      --  the Power_Off command
      DCTRL_Write_Delay;

      if Div <= 1 then
         This.Periph.CLKCR.BYPASS := True;
      else
         Div := Div - 2;

         if Div > Natural (CLKCR_CLKDIV_Field'Last) then
            This.Periph.CLKCR.CLKDIV := CLKCR_CLKDIV_Field'Last;
         else
            This.Periph.CLKCR.CLKDIV := CLKCR_CLKDIV_Field (Div);
         end if;

         This.Periph.CLKCR.BYPASS := False;
      end if;
   end Set_Clock;

   ------------------
   -- Set_Bus_Size --
   ------------------

   overriding procedure Set_Bus_Size
     (This : in out SDMMC_Controller;
      Mode : Wide_Bus_Mode)
   is
      function To_WIDBUS_Field is new Ada.Unchecked_Conversion
        (Wide_Bus_Mode, CLKCR_WIDBUS_Field);
   begin
      This.Periph.CLKCR.WIDBUS := To_WIDBUS_Field (Mode);
   end Set_Bus_Size;

   ------------------
   -- Send_Command --
   ------------------

   overriding procedure Send_Cmd
     (This   : in out SDMMC_Controller;
      Cmd    : Cmd_Desc_Type;
      Arg    : Unsigned_32;
      Status : out SD_Error)
   is
      CMD_Reg : CMD_Register  := This.Periph.CMD;
   begin
      This.Periph.ARG := Arg;
      CMD_Reg.CMDINDEX := CMD_CMDINDEX_Field (Cmd.Cmd);
      CMD_Reg.WAITRESP := (case Cmd.Rsp is
                              when Rsp_No => No_Response,
                              when Rsp_R2 => Long_Response,
                              when others => Short_Response);
      CMD_Reg.WAITINT  := False;
      CMD_Reg.CPSMEN   := True;
      This.Periph.CMD := CMD_Reg;

      case Cmd.Rsp is
         when Rsp_No =>
            Status := This.Command_Error;

         when Rsp_R1 | Rsp_R1B =>
            Status := This.Response_R1_Error (Cmd.Cmd);

         when Rsp_R2 =>
            Status := This.Response_R2_Error;

         when Rsp_R3 =>
            Status := This.Response_R3_Error;

         when Rsp_R6 =>
            declare
               RCA : UInt32;
            begin
               Status := This.Response_R6_Error (Cmd.Cmd, RCA);
               This.RCA := Unsigned_16 (Shift_Right (RCA, 16));
            end;

         when Rsp_R7 =>
            Status := This.Response_R7_Error;

         when Rsp_Invalid =>
            Status := HAL.SDCard.Error;
      end case;
   end Send_Cmd;

   --------------
   -- Read_Cmd --
   --------------

   overriding procedure Read_Cmd
     (This   : in out SDMMC_Controller;
      Cmd    : Cmd_Desc_Type;
      Arg    : Unsigned_32;
      Buf    : System.Address;
      Len    : Unsigned_32;
      Status : out SD_Error)
   is
      Block_Size : DCTRL_DBLOCKSIZE_Field;
      BS         : Unsigned_32;
      RLen       : constant Natural := Natural (Len / 4);
      Tmp        : Word_Array (1 .. RLen) with Import, Address => Buf;
      Idx        : Natural := Tmp'First;
   begin
      if Len = 0 then
         Status := Error;

         return;
      end if;

      --  ??? For now this implementation only supports reading full words.
      --  So let's prevent any confusion in case the data to read is not
      --  aligned on words
      if Len mod 4 /= 0 then
         Status := Address_Missaligned;

         return;
      end if;

      for J in DCTRL_DBLOCKSIZE_Field'Range loop
         BS := 2 ** J'Enum_Rep;
         exit when Len < BS;
         if Len mod BS = 0 then
            Block_Size := J;
         end if;
      end loop;

      Configure_Data
        (This,
         Data_Length        => UInt25 (Len),
         Data_Block_Size    => Block_Size,
         Transfer_Direction => Read,
         Transfer_Mode      => Block,
         DPSM               => True,
         DMA_Enabled        => False);

      This.Send_Cmd (Cmd, Arg, Status);

      if Status /= OK then
         return;
      end if;

      Idx := Tmp'First;

      while not This.Periph.STA.RXOVERR
        and then not This.Periph.STA.DCRCFAIL
        and then not This.Periph.STA.DTIMEOUT
        and then not This.Periph.STA.DBCKEND
      loop
         if Tmp'Last - Idx > 8
           and then This.Periph.STA.RXFIFOHF
         then
            for J in 0 .. 7 loop
               Tmp (Idx + J) := Read_FIFO (This);
            end loop;

            Idx := Idx + 8;

         elsif Idx <= Tmp'Last
           and then This.Periph.STA.RXDAVL
         then
            Tmp (Idx) := Read_FIFO (This);
            Idx := Idx + 1;
         end if;
      end loop;

      while This.Periph.STA.RXDAVL loop
         Tmp (Idx) := Read_FIFO (This);
         Idx := Idx + 1;
      end loop;

      if This.Periph.STA.DTIMEOUT then
         This.Periph.ICR.DTIMEOUTC := True;

         Status := Timeout_Error;

      elsif This.Periph.STA.DCRCFAIL then
         This.Periph.ICR.DCRCFAILC := True;

         Status := CRC_Check_Fail;

      elsif This.Periph.STA.RXOVERR then
         This.Periph.ICR.RXOVERRC := True;

         Status := Rx_Overrun;

      else
         Status := OK;
      end if;

      Clear_Static_Flags (This);
   end Read_Cmd;

   ----------------
   -- Read_Rsp48 --
   ----------------

   overriding procedure Read_Rsp48
     (This : in out SDMMC_Controller;
      Rsp  : out Unsigned_32)
   is
   begin
      Rsp := This.Periph.RESP1;
   end Read_Rsp48;


   overriding procedure Read_Rsp136
     (This           : in out SDMMC_Controller;
      W0, W1, W2, W3 : out Unsigned_32)
   is
   begin
      W0 := This.Periph.RESP1;
      W1 := This.Periph.RESP2;
      W2 := This.Periph.RESP3;
      W3 := This.Periph.RESP4;
   end Read_Rsp136;

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
     (Controller : in out SDMMC_Controller) return UInt32
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
      Command_Index : SD_Command) return SD_Error
   is
      Start   : constant Time := Clock;
      Timeout : Boolean := False;
      R1      : UInt32;
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

      if SD_Command (Controller.Periph.RESPCMD.RESPCMD) /=
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
      Command_Index : SD_Command;
      RCA           :    out UInt32) return SD_Error
   is
      Response : UInt32;
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

      if SD_Command (Controller.Periph.RESPCMD.RESPCMD) /=
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

   -------------------
   -- Stop_Transfer --
   -------------------

   function Stop_Transfer
     (This : in out SDMMC_Controller) return SD_Error
   is
      Ret : SD_Error;
   begin
      Send_Cmd (This, Cmd_Desc (Stop_Transmission), 0, Ret);

      return Ret;
   end Stop_Transfer;


   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (This      : in out SDMMC_Controller;
      SDMMC_CLK : Natural;
      Info      : out Card_Information) return SD_Error
   is
      Ret : SD_Error;
   begin
      This.CLK_In    := SDMMC_CLK;
      HAL.SDCard.Card_Identification_Process (This, Info, Ret);
      This.Card_Type := Info.Card_Type;
      This.RCA       := Info.RCA;

      return Ret;
   end Initialize;

   -----------------
   -- Read_Blocks --
   -----------------

   function Read_Blocks
     (This : in out SDMMC_Controller;
      Addr : Unsigned_64;
      Data : out SD_Data) return SD_Error
   is
      subtype UInt32_Data is SD_Data (1 .. 4);
      function To_Data is new Ada.Unchecked_Conversion
        (UInt32, UInt32_Data);
      R_Addr   : Unsigned_64 := Addr;
      N_Blocks : Positive;
      Err      : SD_Error;
      Idx      : Unsigned_16 := Data'First;
      Dead     : UInt32 with Unreferenced;

   begin
      DCTRL_Write_Delay;
      This.Periph.DCTRL := (others => <>);

      if This.Card_Type = High_Capacity_SD_Card then
         R_Addr := Addr / 512;
      end if;

      N_Blocks := Data'Length / 512;

      Send_Cmd
        (This,
         Cmd    => Set_Blocklen,
         Arg    => 512,
         Status => Err);

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
         Send_Cmd (This, Read_Multi_Block, UInt32 (R_Addr), Err);
      else
         This.Operation := Read_Single_Block_Operation;
         Send_Cmd (This, Read_Single_Block, UInt32 (R_Addr), Err);
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
      Command        : SD_Command;
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

      Enable_Interrupt (This, Data_CRC_Fail_Interrupt);
      Enable_Interrupt (This, Data_Timeout_Interrupt);
      Enable_Interrupt (This, Data_End_Interrupt);
      Enable_Interrupt (This, RX_Overrun_Interrupt);

      STM32.DMA.Start_Transfer_with_Interrupts
        (This               => DMA,
         Stream             => Stream,
         Source             => This.Periph.FIFO'Address,
         Destination        => Data_Addr,
         Data_Count         => Unsigned_16 (Data_Len_Words),
         --  because DMA is set up with words
         Enabled_Interrupts => (Transfer_Error_Interrupt    => True,
                                FIFO_Error_Interrupt        => True,
                                Transfer_Complete_Interrupt => True,
                                others                      => False));

      Send_Cmd (This, Set_Blocklen, 512, Err);

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

      Send_Cmd (This, Command, UInt32 (Read_Address), Err);

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
                        then Addr / 512 else Addr);
      --  512 is the min. block size of SD 2.0 card

      Data_Len_Bytes : constant Natural := (Data'Length / 512) * 512;
      Data_Len_Words : constant Natural := Data_Len_Bytes / 4;
      N_Blocks       : constant Natural := Data_Len_Bytes / 512;
      Data_Addr      : constant Address := Data (Data'First)'Address;

      Err        : SD_Error;
      Cardstatus : HAL.UInt32;
      Start      : constant Time := Clock;
      Timeout    : Boolean := False;
      Command    : SD_Command;
      Rca        : constant UInt32 := Shift_Left (UInt32 (This.RCA), 16);

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
         if Clock - Start > Milliseconds (100) then
            Timeout := True;
            exit Wait_Ready_Loop;
         end if;

         Send_Cmd (This, Send_Status, Rca, Err);

         if Err /= OK then
            return Err;
         end if;

         Cardstatus := This.Periph.RESP1;
         exit Wait_Ready_Loop when (Cardstatus and 16#100#) /= 0;
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
      Send_Cmd (This, Set_Blocklen, 512, Err);

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

      Send_Cmd (This, Command, UInt32 (Write_Address), Err);

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
