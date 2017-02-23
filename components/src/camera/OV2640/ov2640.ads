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

with HAL;     use HAL;
with HAL.I2C; use HAL.I2C;

package OV2640 is

   OV2640_PID : constant := 16#26#;

   type Pixel_Format is (Pix_RGB565, Pix_YUV422, Pix_JPEG);

   type Frame_Size is (QQCIF, QQVGA, QQVGA2, QCIF, HQVGA, QVGA, CIF, VGA,
                       SVGA, SXGA, UXGA);

   type Frame_Rate is (FR_2FPS, FR_8FPS, FR_15FPS, FR_30FPS, FR_60FPS);

   type Resolution is record
     Width, Height : UInt16;
   end record;

   Resolutions : constant array (Frame_Size) of Resolution :=
     ((88,    72),   --  /* QQCIF */
      (160,   120),  --  /* QQVGA */
      (128,   160),  --  /* QQVGA2*/
      (176,   144),  --  /* QCIF  */
      (240,   160),  --  /* HQVGA */
      (320,   240),  --  /* QVGA  */
      (352,   288),  --  /* CIF   */
      (640,   480),  --  /* VGA   */
      (800,   600),  --  /* SVGA  */
      (1280,  1024), --  /* SXGA  */
      (1600,  1200)  --  /* UXGA  */
     );

   type OV2640_Camera (I2C : not null Any_I2C_Port) is private;

   procedure Initialize (This : in out OV2640_Camera;
                         Addr : I2C_Address);

   procedure Set_Pixel_Format (This : OV2640_Camera;
                               Pix  : Pixel_Format);

   procedure Set_Frame_Size (This : OV2640_Camera;
                             Res  : Frame_Size);

   procedure Set_Frame_Rate (This : OV2640_Camera;
                             FR   : Frame_Rate);

   function Get_PID (This : OV2640_Camera) return UInt8;

   procedure Enable_Auto_Gain_Control (This   : OV2640_Camera;
                                       Enable : Boolean := True);
   procedure Enable_Auto_White_Balance (This   : OV2640_Camera;
                                        Enable : Boolean := True);
   procedure Enable_Auto_Exposure_Control (This   : OV2640_Camera;
                                           Enable : Boolean := True);
   procedure Enable_Auto_Band_Filter (This   : OV2640_Camera;
                                      Enable : Boolean := True);
private

   type OV2640_Camera (I2C  : not null Any_I2C_Port) is record
      Addr : UInt10;
   end record;

   REG_BANK_SELECT : constant := 16#FF#;
   SELECT_SENSOR   : constant := 1;
   SELECT_DSP      : constant := 0;

   --  Sensor registers --
   REG_SENSOR_GAIN       : constant := 16#00#;
   REG_SENSOR_COM1       : constant := 16#03#;
   REG_SENSOR_REG04      : constant := 16#04#;
   REG_SENSOR_REG08      : constant := 16#08#;
   REG_SENSOR_COM2       : constant := 16#09#;
   REG_SENSOR_PID        : constant := 16#0A#;
   REG_SENSOR_PIDL       : constant := 16#0B#;
   REG_SENSOR_COM3       : constant := 16#0C#;
   REG_SENSOR_COM4       : constant := 16#0D#;
   REG_SENSOR_AEC        : constant := 16#10#;
   REG_SENSOR_CLKRC      : constant := 16#11#;
   REG_SENSOR_COM7       : constant := 16#12#;
   REG_SENSOR_COM8       : constant := 16#13#;
   REG_SENSOR_COM9       : constant := 16#14#;
   REG_SENSOR_COM10      : constant := 16#15#;
   REG_SENSOR_HREFST     : constant := 16#17#;
   REG_SENSOR_HREFEND    : constant := 16#18#;
   REG_SENSOR_VSTRT      : constant := 16#19#;
   REG_SENSOR_VEND       : constant := 16#1A#;
   REG_SENSOR_MIDH       : constant := 16#1C#;
   REG_SENSOR_MIDL       : constant := 16#1D#;
   REG_SENSOR_AEW        : constant := 16#24#;
   REG_SENSOR_AEB        : constant := 16#25#;
   REG_SENSOR_VV         : constant := 16#26#;
   REG_SENSOR_REG2A      : constant := 16#2A#;
   REG_SENSOR_FRARL      : constant := 16#2B#;
   REG_SENSOR_ADDVSL     : constant := 16#2D#;
   REG_SENSOR_ADDVSH     : constant := 16#2E#;
   REG_SENSOR_YAVG       : constant := 16#2F#;
   REG_SENSOR_HSDY       : constant := 16#30#;
   REG_SENSOR_HEDY       : constant := 16#31#;
   REG_SENSOR_REG32      : constant := 16#32#;
   REG_SENSOR_ARCOM2     : constant := 16#34#;
   REG_SENSOR_REG45      : constant := 16#45#;
   REG_SENSOR_FLL        : constant := 16#46#;
   REG_SENSOR_FLH        : constant := 16#47#;
   REG_SENSOR_COM19      : constant := 16#48#;
   REG_SENSOR_ZOOMS      : constant := 16#49#;
   REG_SENSOR_RSVD       : constant := 16#4A#;
   REG_SENSOR_COM22      : constant := 16#4B#;
   REG_SENSOR_COM25      : constant := 16#4E#;
   REG_SENSOR_BD50       : constant := 16#4F#;
   REG_SENSOR_BD60       : constant := 16#50#;
   REG_SENSOR_REG5D      : constant := 16#5D#;
   REG_SENSOR_REG5E      : constant := 16#5E#;
   REG_SENSOR_REG5F      : constant := 16#5F#;
   REG_SENSOR_REG60      : constant := 16#60#;
   REG_SENSOR_HISTO_LOW  : constant := 16#61#;
   REG_SENSOR_HISTO_HIGH : constant := 16#62#;

   --  DSP registers --

   REG_DSP_BYPASS     : constant := 16#05#;
   REG_DSP_QS         : constant := 16#44#;
   REG_DSP_CTRLI      : constant := 16#50#;
   REG_DSP_HSIZE      : constant := 16#51#;
   REG_DSP_VSIZE      : constant := 16#52#;
   REG_DSP_XOFFL      : constant := 16#53#;
   REG_DSP_YOFFL      : constant := 16#54#;
   REG_DSP_VHYX       : constant := 16#55#;
   REG_DSP_DPRP       : constant := 16#56#;
   REG_DSP_TEST       : constant := 16#57#;
   REG_DSP_ZMOW       : constant := 16#5A#;
   REG_DSP_ZMOH       : constant := 16#5B#;
   REG_DSP_ZMHH       : constant := 16#5C#;
   REG_DSP_BPADDR     : constant := 16#7C#;
   REG_DSP_BPDATA     : constant := 16#7D#;
   REG_DSP_CTRL2      : constant := 16#86#;
   REG_DSP_CTRL3      : constant := 16#87#;
   REG_DSP_SIZEL      : constant := 16#8C#;
   REG_DSP_HSIZE8     : constant := 16#C0#;
   REG_DSP_VSIZE8     : constant := 16#C1#;
   REG_DSP_CTRL0      : constant := 16#C2#;
   REG_DSP_CTRL1      : constant := 16#C3#;
   REG_DSP_R_DVP_SP   : constant := 16#D3#;
   REG_DSP_IMAGE_MODE : constant := 16#DA#;
   REG_DSP_RESET      : constant := 16#E0#;
   REG_DSP_MS_SP      : constant := 16#F0#;
   REG_DSP_SS_ID      : constant := 16#F7#;
   REG_DSP_SS_CTRL    : constant := 16#F8#;
   REG_DSP_MC_BIST    : constant := 16#F9#;
   REG_DSP_MC_AL      : constant := 16#FA#;
   REG_DSP_MC_AH      : constant := 16#FB#;
   REG_DSP_MC_D       : constant := 16#FC#;
   REG_DSP_P_CMD      : constant := 16#FD#;
   REG_DSP_P_STATUS   : constant := 16#FE#;

   COM3_DEFAULT   : constant := 16#38#;
   COM3_BAND_50Hz : constant := 16#04#;
   COM3_BAND_60Hz : constant := 16#00#;
   COM3_BAND_AUTO : constant := 16#02#;

   COM8_DEFAULT : constant := 16#C0#;
   COM8_BNDF_EN : constant := 16#20#; --  Enable Banding filter
   COM8_AGC_EN  : constant := 16#04#; --  AGC Auto/Manual control selection
   COM8_AEC_EN  : constant := 16#01#; --  Auto/Manual Exposure control

   COM9_DEFAULT : constant := 16#08#;

   MC_BIST_RESET           : constant := 16#80#;
   MC_BIST_BOOT_ROM_SEL    : constant := 16#40#;
   MC_BIST_12KB_SEL        : constant := 16#20#;
   MC_BIST_12KB_MASK       : constant := 16#30#;
   MC_BIST_512KB_SEL       : constant := 16#08#;
   MC_BIST_512KB_MASK      : constant := 16#0C#;
   MC_BIST_BUSY_BIT_R      : constant := 16#02#;
   MC_BIST_MC_RES_ONE_SH_W : constant := 16#02#;
   MC_BIST_LAUNCH          : constant := 16#01#;

   RESET_MICROC : constant := 16#40#;
   RESET_SCCB   : constant := 16#20#;
   RESET_JPEG   : constant := 16#10#;
   RESET_DVP    : constant := 16#04#;
   RESET_IPU    : constant := 16#02#;
   RESET_CIF    : constant := 16#01#;

   CTRL3_BPC_EN       : constant := 16#80#;
   CTRL3_WPC_EN       : constant := 16#40#;
   R_DVP_SP           : constant := 16#D3#;
   R_DVP_SP_AUTO_MODE : constant := 16#80#;

   CTRL0_AEC_EN   : constant := 16#80#;
   CTRL0_AEC_SEL  : constant := 16#40#;
   CTRL0_STAT_SEL : constant := 16#20#;
   CTRL0_VFIRST   : constant := 16#10#;
   CTRL0_YUV422   : constant := 16#08#;
   CTRL0_YUV_EN   : constant := 16#04#;
   CTRL0_RGB_EN   : constant := 16#02#;
   CTRL0_RAW_EN   : constant := 16#01#;

end OV2640;
