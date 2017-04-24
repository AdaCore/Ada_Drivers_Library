------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

pragma Warnings (Off);
with Interfaces.Cache;         use Interfaces.Cache;
pragma Warnings (On);

with System;                   use System;
with System.Storage_Elements;

with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Text_IO;              use Ada.Text_IO;

with RPi.Regs.EMMC;            use RPi.Regs.EMMC;

with SDMMC_Init;
--  with RPi.Firmware;             use RPi.Firmware;

package body RPi.SDMMC is

   procedure Read_Multi_Cmd
     (This    : in out SDCard_Driver;
      Cmd     : Cmd_Desc_Type;
      Arg     : UInt32;
      Buf     : System.Address;
      Len     : UInt32;
      Nbr_Blk : UInt32;
      Status  : out SD_Error);

--     function Get_Mmc_Clock return Natural;
   --  Get the driving clock of the controller.

   procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural;
      Wait : Boolean);
   --  If WAIT is true, wait until last command is completed

   --------------
   -- Send_Cmd --
   --------------

   overriding procedure Send_Cmd
     (This   : in out SDCard_Driver;
      Cmd    : Cmd_Desc_Type;
      Arg    : UInt32;
      Status : out SD_Error)
   is
      pragma Unreferenced (This);
      use EMMC_Bits;
      Irpts   : UInt32;
      Cmd_Val : UInt32;
   begin

      --  Wait until cmd line not used by previous command.
      while (EMMC_Periph.Status and CMD_INHIBIT) /= 0 loop
         null;
      end loop;

      Cmd_Val := UInt32 (Cmd.Cmd) * 2**24;
      case Cmd.Rsp is
         when Rsp_Invalid =>
            Status := Illegal_Cmd;
            return;
         when Rsp_No =>
            null;
         when Rsp_R1
           | Rsp_R6
           | Rsp_R7 =>
            --  CRC, IXCHK, 48 bits
            Cmd_Val := Cmd_Val or 16#1a_0000#;
         when Rsp_R1B =>
            --  CRC, IXCHK, 48 bits with busy
            Cmd_Val := Cmd_Val or 16#1b_0000#;
         when Rsp_R2 =>
            --  CRC, 136 bits
            Cmd_Val := Cmd_Val or 16#09_0000#;
         when Rsp_R3 =>
            --  48 bits
            Cmd_Val := Cmd_Val or 16#02_0000#;
      end case;

      case Cmd.Tfr is
         when Tfr_Invalid =>
            Status := Illegal_Cmd;
            return;
         when Tfr_No =>
            null;
         when Tfr_Read =>
            Cmd_Val := Cmd_Val or 16#20_0010#;
         when Tfr_Read_Multi =>
            Cmd_Val := Cmd_Val or 16#20_0036#;
         when Tfr_Write =>
            Cmd_Val := Cmd_Val or 16#20_0000#;
         when Tfr_Write_Multi =>
            Cmd_Val := Cmd_Val or 16#20_0026#;
      end case;

      EMMC_Periph.Arg1 := Arg;
      EMMC_Periph.CMDTM := Cmd_Val;

      if Cmd.Cmd = Send_If_Cond then
         delay until Clock + Milliseconds (1);
      end if;

      --  Wait for command complete interrupt
      loop
         Irpts := EMMC_Periph.Interrupt;
         exit when (Irpts and (CMD_DONE or ERR)) /= 0;
      end loop;

      --  Clear interrupts
      EMMC_Periph.Interrupt := 16#ffff_0001#;
      if (Irpts and 16#ffff_0001#) /= 1 then
         New_Line;
         Status := Error;
      end if;

      if Cmd.Rsp = Rsp_R1B then
         loop
            Irpts := EMMC_Periph.Interrupt;
            exit when (Irpts and (DATA_DONE or ERR)) /= 0;
         end loop;
         if (Irpts and ERR) /= 0 then
            Status := Error;
            return;
         end if;
         EMMC_Periph.Interrupt := 16#ffff_0000# or DATA_DONE;
      end if;

      Status := OK;
   end Send_Cmd;

   ----------------
   -- Read_Rsp48 --
   ----------------

   overriding procedure Read_Rsp48
     (This : in out SDCard_Driver;
      Rsp  :    out UInt32)
   is
      pragma Unreferenced (This);
   begin
      Rsp := EMMC_Periph.RSP0;
   end Read_Rsp48;

   -----------------
   -- Read_Rsp136 --
   -----------------

   overriding procedure Read_Rsp136
     (This           : in out SDCard_Driver;
      W0, W1, W2, W3 : out UInt32) is
   begin
      pragma Unreferenced (This);
      W0 := Shift_Left (EMMC_Periph.RSP3, 8) or Shift_Right (EMMC_Periph.RSP2, 24);
      W1 := Shift_Left (EMMC_Periph.RSP2, 8) or Shift_Right (EMMC_Periph.RSP1, 24);
      W2 := Shift_Left (EMMC_Periph.RSP1, 8) or Shift_Right (EMMC_Periph.RSP0, 24);
      W3 := Shift_Left (EMMC_Periph.RSP0, 8);
   end Read_Rsp136;

   --------------------
   -- Read_Multi_Cmd --
   --------------------

   procedure Read_Multi_Cmd
     (This    : in out SDCard_Driver;
      Cmd     : Cmd_Desc_Type;
      Arg     : UInt32;
      Buf     : System.Address;
      Len     : UInt32;
      Nbr_Blk : UInt32;
      Status  : out SD_Error)
   is
      use EMMC_Bits;
      use System.Storage_Elements;
      Irpts : UInt32;
      Addr  : Address := Buf;
      L     : UInt32 := Len;

   begin
      EMMC_Periph.BLKSIZECNT := Nbr_Blk * 2**16 + Len;

      Send_Cmd (This, Cmd, Arg, Status);

      if Status /= OK then
         return;
      end if;

      --  Wait for data complete interrupt
      Addr := Buf;

      for I in 1 .. Nbr_Blk loop
         loop
            Irpts := EMMC_Periph.Interrupt;
            exit when (Irpts and (READ_RDY or ERR)) /= 0;
         end loop;

         if (Irpts and ERR) /= 0 then
            Status := Error;
            return;
         end if;

         EMMC_Periph.Interrupt := 16#ffff_0000# or READ_RDY;

         pragma Assert (Len mod 4 = 0);

         L := Len;

         while L > 0 loop
            declare
               V : UInt32 with Address => Addr, Import, Volatile;
            begin
               V := EMMC_Periph.Data;
            end;
            Addr := Addr + 4;
            L := L - 4;
         end loop;
      end loop;

      loop
         Irpts := EMMC_Periph.Interrupt;
         exit when (Irpts and (DATA_DONE or ERR)) /= 0;
      end loop;

      if (Irpts and ERR) /= 0 then
         Status := Error;
         return;
      end if;

      EMMC_Periph.Interrupt := 16#ffff_0000# or DATA_DONE;
   end Read_Multi_Cmd;

   --------------
   -- Read_Cmd --
   --------------

   overriding procedure Read_Cmd
     (This   : in out SDCard_Driver;
      Cmd    : Cmd_Desc_Type;
      Arg    : UInt32;
      Buf    : out UInt32_Array;
      Status : out SD_Error)
   is
   begin
      Read_Multi_Cmd (This, Cmd, Arg, Buf'Address, Buf'Length * 4, 1, Status);
   end Read_Cmd;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This         : in out SDCard_Driver;
      Block_Number : UInt64;
      Data         : out HAL.Block_Drivers.Block) return Boolean
   is
      Status   : SD_Error;
      Blk_Size : constant UInt32 := 512;
      Len      : constant UInt32 := Data'Length;
      Nbr_Blks : constant UInt32 := Len / Blk_Size;

   begin
      pragma Assert (Len = Nbr_Blks * Blk_Size);

      if Nbr_Blks = 1 then
         Read_Multi_Cmd (This, Cmd_Desc (Read_Single_Block),
                         UInt32 (Block_Number),
                         Data'Address, Blk_Size, 1, Status);
      else
         Read_Multi_Cmd (This, Cmd_Desc (Read_Multi_Block),
                         UInt32 (Block_Number),
                         Data'Address, Blk_Size, Nbr_Blks, Status);
      end if;

      return Status = OK;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This         : in out SDCard_Driver;
      Block_Number : UInt64;
      Data         : HAL.Block_Drivers.Block) return Boolean
   is
      pragma Unreferenced (This, Block_Number, Data);
   begin
      --  ??? SDMMC write not implemented
      return False;
   end Write;

--     -------------------
--     -- Get_Mmc_Clock --
--     -------------------
--
--     function Get_Mmc_Clock return Natural
--     is
--        Data : UInt32_Array := (1 => Clock_Id'Enum_Rep (EMMC_Clock), 2 => 0);
--     begin
--        Fw_Request (Tag_Get_Clock_Rate, Data);
--
--        return Natural (Data (2));
--     end Get_Mmc_Clock;

   ---------------
   -- Set_Clock --
   ---------------

   procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural;
      Wait : Boolean)
   is
      use EMMC_Bits;
      Mmc_Clk : UInt32 renames This.CLK_In;
      Div     : Natural;
      Ctrl    : UInt32;

   begin
      if Mmc_Clk = 0 or else Freq = 0 then
         return;
      end if;

      Div := Natural (Mmc_Clk) / Freq;

      --  Divider must be a multiple of 2.
      if Div mod 2 /= 0 then
         Div := Div + 1;
      end if;

      --  Max divider value.
      Div := Natural'Min (Div, 2046) / 2;

      --  Wait until data and command lines are quiet
      if Wait then
         while (EMMC_Periph.Status and (DAT_INHIBIT or CMD_INHIBIT)) /= 0 loop
            null;
         end loop;
      end if;

      Ctrl := EMMC_Periph.Control1;

      --  Disable clock.
      Ctrl := Ctrl and not CLK_EN;
      EMMC_Periph.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);

      --  Write new value.
      Ctrl := Ctrl and not 16#fffe0#;
      Ctrl := Ctrl or Shift_Left (14, 16); --  Timeout
      Ctrl := Ctrl or Shift_Left (UInt32 (Div) and 16#ff#, 8);
      Ctrl := Ctrl or Shift_Left (Shift_Right (UInt32 (Div), 8) and 3, 6);
      Ctrl := Ctrl or CLK_INTLEN;
      EMMC_Periph.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);

      --  Wait until stabilized.
      while (EMMC_Periph.Control1 and CLK_STABLE) = 0 loop
         null;
      end loop;

      --  Enable the clock
      Ctrl := Ctrl or CLK_EN;
      EMMC_Periph.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);
   end Set_Clock;

   ---------------
   -- Set_Clock --
   ---------------

   overriding procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural) is
   begin
      Set_Clock (This, Freq, True);
   end Set_Clock;

   ------------------
   -- Set_Bus_Size --
   ------------------

   overriding procedure Set_Bus_Size
     (This : in out SDCard_Driver;
      Mode : Wide_Bus_Mode)
   is
      pragma Unreferenced (This);
      V : UInt32;
   begin
      V := EMMC_Periph.Control0;
      V := V and not 16#22#;

      case Mode is
         when Wide_Bus_1B =>
            null;
         when Wide_Bus_4B =>
            V := V or 2;
         when Wide_Bus_8B =>
            V := V or 16#20#;
      end case;

      EMMC_Periph.Control0 := V;
   end Set_Bus_Size;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   overriding procedure Delay_Milliseconds
     (This   : SDCard_Driver;
      Amount : Natural)
   is
      pragma Unreferenced (This);
   begin
      delay until Clock + Milliseconds (Amount);
   end Delay_Milliseconds;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (This   : in out SDCard_Driver;
      Status : out SD_Error)
   is
      use EMMC_Bits;
   begin
      --  Reset controller
      EMMC_Periph.Control0 := 0;
      EMMC_Periph.Control1 := 16#070ffa20#;
      EMMC_Periph.Control2 := 0;

      --  Wait until end of reset
      while (EMMC_Periph.Control1 and (SRST_DATA or SRST_CMD or SRST_HC)) /= 0 loop
         null;
      end loop;

      Set_Clock (This, 400_000, False);

      --  Enable int
      EMMC_Periph.IRPT_Mask := 16#ffff_ffff#;
      EMMC_Periph.IRPT_En := 16#ffff_ffff#;

      Status := OK;
   end Reset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This    : in out SDCard_Driver;
      MMC_Clk : UInt32;
      Info    : out Card_Information;
      Status  : out SD_Error) is
   begin
      This.CLK_In := MMC_Clk;
      SDMMC_Init.Card_Identification_Process (This, Info, Status);
   end Initialize;

end RPi.SDMMC;
