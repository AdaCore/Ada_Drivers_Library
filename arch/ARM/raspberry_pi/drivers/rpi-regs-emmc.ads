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

with RPi.Regs;

package RPi.Regs.EMMC is

   type EMMC_Registers_Type is record
      --  At 0x00
      ACMD           : UInt32;
      BLKSIZECNT     : UInt32;
      Arg1           : UInt32;
      CMDTM          : UInt32;

      --  At 0x10
      RSP0           : UInt32;
      RSP1           : UInt32;
      RSP2           : UInt32;
      RSP3           : UInt32;

      --  At 0x20
      Data           : UInt32;
      Status         : UInt32;
      Control0       : UInt32;
      Control1       : UInt32;

      --  At 0x30
      Interrupt      : UInt32;
      IRPT_Mask      : UInt32;
      IRPT_En        : UInt32;
      Control2       : UInt32;

      --  At 0x40
      Pad_40         : UInt32;
      Pad_44         : UInt32;
      Pad_48         : UInt32;
      Pad_4c         : UInt32;

      --  At 0x50
      Force_IRPT     : UInt32;
      Pad_54         : UInt32;
      Pad_58         : UInt32;
      Pad_5c         : UInt32;

      --  At 0x60
      Pad_60         : UInt32;
      Pad_64         : UInt32;
      Pad_68         : UInt32;
      Pad_6c         : UInt32;

      --  At 0x70
      Boot_Timeout   : UInt32;
      DBG_Sel        : UInt32;
      Pad_78         : UInt32;
      Pad_7c         : UInt32;

      --  At 0x80
      EXRDFIFO_CFG   : UInt32;
      EXRDFIFO_En    : UInt32;
      Tune_Step      : UInt32;
      Tune_Steps_STD : UInt32;

      --  At 0x90
      Tune_Steps_DDR : UInt32;
      Pad_94         : UInt32;
      Pad_98         : UInt32;
      Pad_9c         : UInt32;

      --  At 0xa0
      Pad_a0         : UInt32;
      Pad_a4         : UInt32;
      Pad_a8         : UInt32;
      Pad_ac         : UInt32;

      --  At 0xb0
      Pad_b0         : UInt32;
      Pad_b4         : UInt32;
      Pad_b8         : UInt32;
      Pad_bc         : UInt32;

      --  At 0xc0
      Pad_c0         : UInt32;
      Pad_c4         : UInt32;
      Pad_c8         : UInt32;
      Pad_cc         : UInt32;

      --  At 0xd0
      Pad_d0         : UInt32;
      Pad_d4         : UInt32;
      Pad_d8         : UInt32;
      Pad_dc         : UInt32;

      --  At 0xe0
      Pad_e0         : UInt32;
      Pad_e4         : UInt32;
      Pad_e8         : UInt32;
      Pad_ec         : UInt32;

      --  At 0xf0
      Spi_Int_Spt    : UInt32;
      Pad_f4         : UInt32;
      Pad_f8         : UInt32;
      SlotISR_Ver    : UInt32;
   end record;

   package EMMC_Commands is
      GO_IDLE_STATE  : constant := 0;
      IO_SET_OP_COND : constant := 5;
      CMD_IF_COND    : constant := 8;
   end EMMC_Commands;

   package EMMC_Bits is
      --  Status
      CMD_INHIBIT : constant := 2**0;
      DAT_INHIBIT : constant := 2**1;

      --  Control 1
      SRST_DATA   : constant := 2 ** 26;
      SRST_CMD    : constant := 2 ** 25;
      SRST_HC     : constant := 2 ** 24;
      CLK_INTLEN  : constant := 2 ** 0;
      CLK_STABLE  : constant := 2 ** 1;
      CLK_EN      : constant := 2 ** 2;

      --  Interrupt
      CMD_DONE    : constant := 2 ** 0;
      DATA_DONE   : constant := 2 ** 1;
      WRITE_RDY   : constant := 2 ** 4;
      READ_RDY    : constant := 2 ** 5;
      ERR         : constant := 2 ** 15;
   end EMMC_Bits;

   EMMC_Periph : EMMC_Registers_Type
     with Import, Volatile, Address => System'To_Address (RPi.Regs.EMMC_Base);

end RPi.Regs.EMMC;
