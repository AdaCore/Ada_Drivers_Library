------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

package body OV7725 is
   type Addr_And_Data is record
      Addr, Data : UInt8;
   end record;

   type Command_Array is array (Natural range <>) of Addr_And_Data;

   Setup_Commands : constant Command_Array :=
     ((REG_COM3,          COM3_SWAP_YUV),
      (REG_COM7,          COM7_RES_VGA or COM7_FMT_RGB565 or COM7_FMT_RGB),

      (REG_COM4,          16#41#),
      (REG_CLKRC,         16#C0#),

      (REG_HSTART,        16#23#),
      (REG_HSIZE,         16#A0#),
      (REG_VSTART,        16#07#),
      (REG_VSIZE,         16#F0#),
      (REG_HREF,          16#00#),

      (REG_HOUTSIZE,      16#50#),
      (REG_VOUTSIZE,      16#78#),

      (REG_COM12,         16#03#),
      (REG_EXHCH,         16#00#),
      (REG_TGT_B,         16#7F#),
      (REG_FIXGAIN,       16#09#),
      (REG_AWB_CTRL0,     16#E0#),
      (REG_DSP_CTRL1,     16#FF#),

      (REG_DSP_CTRL2,     DSP_CTRL2_VDCW_EN or DSP_CTRL2_HDCW_EN or
           DSP_CTRL2_HZOOM_EN or DSP_CTRL2_VZOOM_EN),

      (REG_DSP_CTRL3,     16#00#),
      (REG_DSP_CTRL4,     16#00#),
      (REG_DSPAUTO,       16#FF#),

      (REG_COM8,          16#F0#),
      (REG_COM6,          16#C5#),
      (REG_COM9,          16#21#),
      (REG_BDBASE,        16#7F#),
      (REG_DBSTEP,        16#03#),
      (REG_AEW,           16#96#),
      (REG_AEB,           16#64#),
      (REG_VPT,           16#A1#),
      (REG_EXHCL,         16#00#),
      (REG_AWB_CTRL3,     16#AA#),
      (REG_COM8,          16#FF#),

      (REG_GAM1,          16#0C#),
      (REG_GAM2,          16#16#),
      (REG_GAM3,          16#2A#),
      (REG_GAM4,          16#4E#),
      (REG_GAM5,          16#61#),
      (REG_GAM6,          16#6F#),
      (REG_GAM7,          16#7B#),
      (REG_GAM8,          16#86#),
      (REG_GAM9,          16#8E#),
      (REG_GAM10,         16#97#),
      (REG_GAM11,         16#A4#),
      (REG_GAM12,         16#AF#),
      (REG_GAM13,         16#C5#),
      (REG_GAM14,         16#D7#),
      (REG_GAM15,         16#E8#),

      (REG_SLOP,          16#20#),
      (REG_EDGE1,         16#05#),
      (REG_EDGE2,         16#03#),
      (REG_EDGE3,         16#00#),
      (REG_DNSOFF,        16#01#),

      (REG_MTX1,          16#B0#),
      (REG_MTX2,          16#9D#),
      (REG_MTX3,          16#13#),
      (REG_MTX4,          16#16#),
      (REG_MTX5,          16#7B#),
      (REG_MTX6,          16#91#),
      (REG_MTX_CTRL,      16#1E#),

      (REG_BRIGHTNESS,    16#08#),
      (REG_CONTRAST,      16#20#),
      (REG_UVADJ0,        16#81#),
      (REG_SDE,           (SDE_CONT_BRIGHT_EN or SDE_SATURATION_EN)),

      (REG_DM_LNL,        16#00#),
      (REG_DM_LNH,        16#00#),
      (REG_BDBASE,        16#7F#),
      (REG_DBSTEP,        16#03#),

      (REG_LC_RADI,       16#10#),
      (REG_LC_COEF,       16#10#),
      (REG_LC_COEFB,      16#14#),
      (REG_LC_COEFR,      16#17#),
      (REG_LC_CTR,        16#05#),
      (REG_COM5,          16#D5#));

   function Read (This     : OV7725_Camera;
                  Mem_Addr : UInt8) return UInt8;

   procedure Write (This     : OV7725_Camera;
                    Mem_Addr : UInt8;
                    Data     : UInt8);

   ----------
   -- Read --
   ----------

   function Read (This     : OV7725_Camera;
                  Mem_Addr : UInt8) return UInt8
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 1);
   begin
      This.I2C.Master_Transmit (Addr    => This.Addr,
                                Data    => (1 => Mem_Addr),
                                Status  => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.I2C.Master_Receive (Addr    => This.Addr,
                               Data    => Data,
                               Status  => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      return Data (Data'First);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (This     : OV7725_Camera;
                    Mem_Addr : UInt8;
                    Data     : UInt8)
   is
      Status : I2C_Status;
   begin
      This.I2C.Master_Transmit (Addr    => This.Addr,
                                Data    => (1 => Mem_Addr, 2 => Data),
                                Status  => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out OV7725_Camera;
      Addr : I2C_Address)
   is
   begin
      This.Addr := Addr;
      for Elt of Setup_Commands loop
         Write (This, Elt.Addr, Elt.Data);
      end loop;
   end Initialize;

   ----------------------
   -- Set_Pixel_Format --
   ----------------------

   procedure Set_Pixel_Format
     (This : OV7725_Camera;
      Pix  : Pixel_Format)
   is
      Reg : UInt8 := Read (This, REG_COM7);
   begin
      Reg := Reg and 2#1111_00_11#;
      case Pix is
         when Pix_RGB565 =>
            Reg := Reg or 2#0000_01_00#;
         when Pix_RGB555 =>
            Reg := Reg or 2#0000_10_00#;
         when Pix_RGB444 =>
            Reg := Reg or 2#0000_11_00#;
      end case;
      Write (This, REG_COM7, Reg);

      Reg := Read (This, REG_COM3);
      Write (This, REG_COM3, Reg and 2#0000_1000#); --  SWAP MSB/LSB
   end Set_Pixel_Format;

   --------------------
   -- Set_Frame_Size --
   --------------------

   procedure Set_Frame_Size
     (This : OV7725_Camera;
      Res  : OV2640.Frame_Size)
   is
      use type OV2640.Frame_Size;
      Width  : constant UInt16 := OV2640.Resolutions (Res).Width;
      Height : constant UInt16 := OV2640.Resolutions (Res).Height;
   begin
      Write (This, REG_HOUTSIZE, UInt8 (Shift_Right (Width, 2)));
      Write (This, REG_VOUTSIZE, UInt8 (Shift_Right (Height, 1)));

      Write (This, REG_EXHCH,
             UInt8 (Width and 2#0111#)
             or
               Shift_Left (UInt8 (Height and 2#0001#), 2));

      if Res < OV2640.VGA then
         Write (This, REG_DSPAUTO, 16#FF#);
      else
         Write (This, REG_DSPAUTO, 16#F3#);
         Write (This, REG_SCAL0, 16#00#);
         Write (This, REG_SCAL1, 16#00#);
         Write (This, REG_SCAL2, 16#00#);
      end if;
   end Set_Frame_Size;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (This : OV7725_Camera) return UInt8 is
   begin
      return Read (This, REG_PID);
   end Get_PID;

end OV7725;
