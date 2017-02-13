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

with HAL;     use HAL;
with HAL.I2C; use HAL.I2C;
with OV2640;

package OV7725 is

   OV7725_PID : constant := 16#77#;

   type Pixel_Format is (Pix_RGB565, Pix_RGB555, Pix_RGB444);

   type OV7725_Camera (I2C : not null Any_I2C_Port) is private;

   procedure Initialize (This : in out OV7725_Camera;
                         Addr : I2C_Address);

   procedure Set_Pixel_Format (This : OV7725_Camera;
                               Pix  : Pixel_Format);

   procedure Set_Frame_Size (This : OV7725_Camera;
                             Res  : OV2640.Frame_Size);

   function Get_PID (This : OV7725_Camera) return UInt8;

private

   type OV7725_Camera (I2C  : not null Any_I2C_Port) is record
      Addr : UInt10;
   end record;

   REG_GAIN               : constant := 16#00#;
   REG_BLUE               : constant := 16#01#;
   REG_RED                : constant := 16#02#;
   REG_GREEN              : constant := 16#03#;
   REG_BAVG               : constant := 16#05#;
   REG_GAVG               : constant := 16#06#;
   REG_RAVG               : constant := 16#07#;
   REG_AECH               : constant := 16#08#;
   REG_COM2               : constant := 16#09#;
   REG_PID                : constant := 16#0A#;
   REG_VER                : constant := 16#0B#;
   REG_COM3               : constant := 16#0C#;
   REG_COM4               : constant := 16#0D#;
   REG_COM5               : constant := 16#0E#;
   REG_COM6               : constant := 16#0F#;
   REG_AEC                : constant := 16#10#;
   REG_CLKRC              : constant := 16#11#;
   REG_COM7               : constant := 16#12#;
   REG_COM8               : constant := 16#13#;
   REG_COM9               : constant := 16#14#;
   REG_COM10              : constant := 16#15#;
   REG_REG16              : constant := 16#16#;
   REG_HSTART             : constant := 16#17#;
   REG_HSIZE              : constant := 16#18#;
   REG_VSTART             : constant := 16#19#;
   REG_VSIZE              : constant := 16#1A#;
   REG_PSHFT              : constant := 16#1B#;
   REG_MIDH               : constant := 16#1C#;
   REG_MIDL               : constant := 16#1D#;
   REG_LAEC               : constant := 16#1F#;
   REG_COM11              : constant := 16#20#;
   REG_BDBASE             : constant := 16#22#;
   REG_DBSTEP             : constant := 16#23#;
   REG_AEW                : constant := 16#24#;
   REG_AEB                : constant := 16#25#;
   REG_VPT                : constant := 16#26#;
   REG_REG28              : constant := 16#28#;
   REG_HOUTSIZE           : constant := 16#29#;
   REG_EXHCH              : constant := 16#2A#;
   REG_EXHCL              : constant := 16#2B#;
   REG_VOUTSIZE           : constant := 16#2C#;
   REG_ADVFL              : constant := 16#2D#;
   REG_ADVFH              : constant := 16#2E#;
   REG_YAVE               : constant := 16#2F#;
   REG_LUMHTH             : constant := 16#30#;
   REG_LUMLTH             : constant := 16#31#;
   REG_HREF               : constant := 16#32#;
   REG_DM_LNL             : constant := 16#33#;
   REG_DM_LNH             : constant := 16#34#;
   REG_ADOFF_B            : constant := 16#35#;
   REG_ADOFF_R            : constant := 16#36#;
   REG_ADOFF_GB           : constant := 16#37#;
   REG_ADOFF_GR           : constant := 16#38#;
   REG_OFF_B              : constant := 16#39#;
   REG_OFF_R              : constant := 16#3A#;
   REG_OFF_GB             : constant := 16#3B#;
   REG_OFF_GR             : constant := 16#3C#;
   REG_COM12              : constant := 16#3D#;
   REG_COM13              : constant := 16#3E#;
   REG_COM14              : constant := 16#3F#;
   REG_COM15              : constant := 16#40#;
   REG_COM16              : constant := 16#41#;
   REG_TGT_B              : constant := 16#42#;
   REG_TGT_R              : constant := 16#43#;
   REG_TGT_GB             : constant := 16#44#;
   REG_TGT_GR             : constant := 16#45#;
   REG_LC_CTR             : constant := 16#46#;
   REG_LC_XC              : constant := 16#47#;
   REG_LC_YC              : constant := 16#48#;
   REG_LC_COEF            : constant := 16#49#;
   REG_LC_RADI            : constant := 16#4A#;
   REG_LC_COEFB           : constant := 16#4B#;
   REG_LC_COEFR           : constant := 16#4C#;
   REG_FIXGAIN            : constant := 16#4D#;
   REG_AREF0              : constant := 16#4E#;
   REG_AREF1              : constant := 16#4F#;
   REG_AREF2              : constant := 16#50#;
   REG_AREF3              : constant := 16#51#;
   REG_AREF4              : constant := 16#52#;
   REG_AREF5              : constant := 16#53#;
   REG_AREF6              : constant := 16#54#;
   REG_AREF7              : constant := 16#55#;
   REG_UFIX               : constant := 16#60#;
   REG_VFIX               : constant := 16#61#;
   REG_AWBB_BLK           : constant := 16#62#;
   REG_AWB_CTRL0          : constant := 16#63#;
   REG_DSP_CTRL1          : constant := 16#64#;
   REG_DSP_CTRL2          : constant := 16#65#;
   REG_DSP_CTRL3          : constant := 16#66#;
   REG_DSP_CTRL4          : constant := 16#67#;
   REG_AWB_BIAS           : constant := 16#68#;
   REG_AWB_CTRL1          : constant := 16#69#;
   REG_AWB_CTRL2          : constant := 16#6A#;
   REG_AWB_CTRL3          : constant := 16#6B#;
   REG_AWB_CTRL4          : constant := 16#6C#;
   REG_AWB_CTRL5          : constant := 16#6D#;
   REG_AWB_CTRL6          : constant := 16#6E#;
   REG_AWB_CTRL7          : constant := 16#6F#;
   REG_AWB_CTRL8          : constant := 16#70#;
   REG_AWB_CTRL9          : constant := 16#71#;
   REG_AWB_CTRL10         : constant := 16#72#;
   REG_AWB_CTRL11         : constant := 16#73#;
   REG_AWB_CTRL12         : constant := 16#74#;
   REG_AWB_CTRL13         : constant := 16#75#;
   REG_AWB_CTRL14         : constant := 16#76#;
   REG_AWB_CTRL15         : constant := 16#77#;
   REG_AWB_CTRL16         : constant := 16#78#;
   REG_AWB_CTRL17         : constant := 16#79#;
   REG_AWB_CTRL18         : constant := 16#7A#;
   REG_AWB_CTRL19         : constant := 16#7B#;
   REG_AWB_CTRL20         : constant := 16#7C#;
   REG_AWB_CTRL21         : constant := 16#7D#;
   REG_GAM1               : constant := 16#7E#;
   REG_GAM2               : constant := 16#7F#;
   REG_GAM3               : constant := 16#80#;
   REG_GAM4               : constant := 16#81#;
   REG_GAM5               : constant := 16#82#;
   REG_GAM6               : constant := 16#83#;
   REG_GAM7               : constant := 16#84#;
   REG_GAM8               : constant := 16#85#;
   REG_GAM9               : constant := 16#86#;
   REG_GAM10              : constant := 16#87#;
   REG_GAM11              : constant := 16#88#;
   REG_GAM12              : constant := 16#89#;
   REG_GAM13              : constant := 16#8A#;
   REG_GAM14              : constant := 16#8B#;
   REG_GAM15              : constant := 16#8C#;
   REG_SLOP               : constant := 16#8D#;
   REG_DNSTH              : constant := 16#8E#;
   REG_EDGE0              : constant := 16#8F#;
   REG_EDGE1              : constant := 16#90#;
   REG_DNSOFF             : constant := 16#91#;
   REG_EDGE2              : constant := 16#92#;
   REG_EDGE3              : constant := 16#93#;
   REG_MTX1               : constant := 16#94#;
   REG_MTX2               : constant := 16#95#;
   REG_MTX3               : constant := 16#96#;
   REG_MTX4               : constant := 16#97#;
   REG_MTX5               : constant := 16#98#;
   REG_MTX6               : constant := 16#99#;
   REG_MTX_CTRL           : constant := 16#9A#;
   REG_MTX_CTRL_DBL_EN    : constant := 16#80#;
   REG_BRIGHTNESS         : constant := 16#9B#;
   REG_CONTRAST           : constant := 16#9C#;
   REG_UVADJ0             : constant := 16#9E#;
   REG_UVADJ1             : constant := 16#9F#;
   REG_SCAL0              : constant := 16#A0#;
   REG_SCAL1              : constant := 16#A1#;
   REG_SCAL2              : constant := 16#A2#;
   REG_FIFODLYM           : constant := 16#A3#;
   REG_FIFODLYA           : constant := 16#A4#;
   REG_SDE                : constant := 16#A6#;
   REG_USAT               : constant := 16#A7#;
   REG_VSAT               : constant := 16#A8#;
   REG_HUECOS             : constant := 16#A9#;
   REG_HUESIN             : constant := 16#AA#;
   REG_SIGN_BIT           : constant := 16#AB#;
   REG_DSPAUTO            : constant := 16#AC#;
   REG_SDE_NEGATIVE_EN    : constant := 16#40#;
   REG_SDE_GRAYSCALE_EN   : constant := 16#20#;
   REG_SDE_V_FIXED_EN     : constant := 16#10#;
   REG_SDE_U_FIXED_EN     : constant := 16#08#;
   REG_SDE_CONT_BRIGHT_EN : constant := 16#04#;
   REG_SDE_SATURATION_EN  : constant := 16#02#;
   REG_SDE_HUE_EN         : constant := 16#01#;

   COM3_SWAP_YUV      : constant := 16#10#;
   COM7_RES_VGA       : constant := 16#00#;
   COM7_FMT_RGB565    : constant := 16#04#;
   COM7_FMT_RGB       : constant := 16#02#;
   DSP_CTRL2_VDCW_EN  : constant := 16#08#;
   DSP_CTRL2_HDCW_EN  : constant := 16#04#;
   DSP_CTRL2_VZOOM_EN : constant := 16#02#;
   DSP_CTRL2_HZOOM_EN : constant := 16#01#;
   SDE_CONT_BRIGHT_EN : constant := 16#04#;
   SDE_SATURATION_EN  : constant := 16#02#;

end OV7725;
