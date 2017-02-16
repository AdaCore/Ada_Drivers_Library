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

--  Based on ov2640.c from OpenMV
--
--  This file is part of the OpenMV project.
--  Copyright (c) 2013/2014 Ibrahim Abdelkader <i.abdalkader@gmail.com>
--  This work is licensed under the MIT license, see the file LICENSE for
--  details.
--
--  OV2640 driver.
--

with Bit_Fields; use Bit_Fields;

package body OV2640 is

   type Addr_And_Data is record
      Addr, Data : UInt8;
   end record;

   type Command_Array is array (Natural range <>) of Addr_And_Data;

   Setup_Commands : constant Command_Array :=
     ((REG_BANK_SELECT, SELECT_DSP),
      (16#2c#,     16#ff#),
      (16#2e#,     16#df#),
      (REG_BANK_SELECT, SELECT_SENSOR),
      (16#3c#,     16#32#),
      (REG_SENSOR_CLKRC,    16#80#), --  Set PCLK divider */

      --  COM2_OUT_DRIVE_3x
      (REG_SENSOR_COM2,     16#02#), --  Output drive x3 */
      --  #ifdef OPENMV2
      (REG_SENSOR_REG04,    16#F8#), --  Mirror/VFLIP/AEC[1:0] */
      --  #else
      --      (REG04_SET(REG04_HREF_EN)),
      --  #endif
      (REG_SENSOR_COM8,     COM8_DEFAULT or
                            COM8_BNDF_EN or
                            COM8_AGC_EN or
                            COM8_AEC_EN),
      --  COM9_AGC_GAIN_8x
      (REG_SENSOR_COM9,     COM9_DEFAULT or Shift_Left (16#02#, 5)),
      (16#2c#,     16#0c#),
      (16#33#,     16#78#),
      (16#3a#,     16#33#),
      (16#3b#,     16#fb#),
      (16#3e#,     16#00#),
      (16#43#,     16#11#),
      (16#16#,     16#10#),
      (16#39#,     16#02#),
      (16#35#,     16#88#),
      (16#22#,     16#0a#),
      (16#37#,     16#40#),
      (16#23#,     16#00#),
      (REG_SENSOR_ARCOM2,   16#a0#),
      (16#06#,     16#02#),
      (16#06#,     16#88#),
      (16#07#,     16#c0#),
      (16#0d#,     16#b7#),
      (16#0e#,     16#01#),
      (16#4c#,     16#00#),
      (16#4a#,     16#81#),
      (16#21#,     16#99#),
      (REG_SENSOR_AEW,      16#40#),
      (REG_SENSOR_AEB,      16#38#),
      --  AGC/AEC fast mode operating region
      --  VV_AGC_TH_SET(h,l) ((h<<4)|(l&0x0F))
      --  VV_AGC_TH_SET(16#08#, 16#02#)
      (REG_SENSOR_VV,       Shift_Left (16#08#, 4) or 16#02#),
      (REG_SENSOR_COM19,    16#00#), --  Zoom control 2 MSBs */
      (REG_SENSOR_ZOOMS,    16#00#), --  Zoom control 8 MSBs */
      (16#5c#,     16#00#),
      (16#63#,     16#00#),
      (REG_SENSOR_FLL,      16#00#),
      (REG_SENSOR_FLH,      16#00#),

      --  Set banding filter
      (REG_SENSOR_COM3,     COM3_DEFAULT or COM3_BAND_AUTO),
      (REG_SENSOR_REG5D,    16#55#),
      (REG_SENSOR_REG5E,    16#7d#),
      (REG_SENSOR_REG5F,    16#7d#),
      (REG_SENSOR_REG60,    16#55#),
      (REG_SENSOR_HISTO_LOW,   16#70#),
      (REG_SENSOR_HISTO_HIGH,  16#80#),
      (16#7c#,     16#05#),
      (16#20#,     16#80#),
      (16#28#,     16#30#),
      (16#6c#,     16#00#),
      (16#6d#,     16#80#),
      (16#6e#,     16#00#),
      (16#70#,     16#02#),
      (16#71#,     16#94#),
      (16#73#,     16#c1#),
      (16#3d#,     16#34#),
      --  (COM7,   COM7_RES_UXGA | COM7_ZOOM_EN),
      (16#5a#,     16#57#),
      (REG_SENSOR_BD50,     16#bb#),
      (REG_SENSOR_BD60,     16#9c#),

      (REG_BANK_SELECT, SELECT_DSP),
      (16#e5#,     16#7f#),
      (REG_DSP_MC_BIST,  MC_BIST_RESET or MC_BIST_BOOT_ROM_SEL),
      (16#41#,     16#24#),
      (REG_DSP_RESET,    RESET_JPEG or RESET_DVP),
      (16#76#,     16#ff#),
      (16#33#,     16#a0#),
      (16#42#,     16#20#),
      (16#43#,     16#18#),
      (16#4c#,     16#00#),
      (REG_DSP_CTRL3,    CTRL3_BPC_EN or CTRL3_WPC_EN or 16#10#),
      (16#88#,     16#3f#),
      (16#d7#,     16#03#),
      (16#d9#,     16#10#),
      (REG_DSP_R_DVP_SP, R_DVP_SP_AUTO_MODE or 16#2#),
      (16#c8#,     16#08#),
      (16#c9#,     16#80#),
      (REG_DSP_BPADDR,   16#00#),
      (REG_DSP_BPDATA,   16#00#),
      (REG_DSP_BPADDR,   16#03#),
      (REG_DSP_BPDATA,   16#48#),
      (REG_DSP_BPDATA,   16#48#),
      (REG_DSP_BPADDR,   16#08#),
      (REG_DSP_BPDATA,   16#20#),
      (REG_DSP_BPDATA,   16#10#),
      (REG_DSP_BPDATA,   16#0e#),
      (16#90#,     16#00#),
      (16#91#,     16#0e#),
      (16#91#,     16#1a#),
      (16#91#,     16#31#),
      (16#91#,     16#5a#),
      (16#91#,     16#69#),
      (16#91#,     16#75#),
      (16#91#,     16#7e#),
      (16#91#,     16#88#),
      (16#91#,     16#8f#),
      (16#91#,     16#96#),
      (16#91#,     16#a3#),
      (16#91#,     16#af#),
      (16#91#,     16#c4#),
      (16#91#,     16#d7#),
      (16#91#,     16#e8#),
      (16#91#,     16#20#),
      (16#92#,     16#00#),
      (16#93#,     16#06#),
      (16#93#,     16#e3#),
      (16#93#,     16#03#),
      (16#93#,     16#03#),
      (16#93#,     16#00#),
      (16#93#,     16#02#),
      (16#93#,     16#00#),
      (16#93#,     16#00#),
      (16#93#,     16#00#),
      (16#93#,     16#00#),
      (16#93#,     16#00#),
      (16#93#,     16#00#),
      (16#93#,     16#00#),
      (16#96#,     16#00#),
      (16#97#,     16#08#),
      (16#97#,     16#19#),
      (16#97#,     16#02#),
      (16#97#,     16#0c#),
      (16#97#,     16#24#),
      (16#97#,     16#30#),
      (16#97#,     16#28#),
      (16#97#,     16#26#),
      (16#97#,     16#02#),
      (16#97#,     16#98#),
      (16#97#,     16#80#),
      (16#97#,     16#00#),
      (16#97#,     16#00#),
      (16#a4#,     16#00#),
      (16#a8#,     16#00#),
      (16#c5#,     16#11#),
      (16#c6#,     16#51#),
      (16#bf#,     16#80#),
      (16#c7#,     16#10#),
      (16#b6#,     16#66#),
      (16#b8#,     16#A5#),
      (16#b7#,     16#64#),
      (16#b9#,     16#7C#),
      (16#b3#,     16#af#),
      (16#b4#,     16#97#),
      (16#b5#,     16#FF#),
      (16#b0#,     16#C5#),
      (16#b1#,     16#94#),
      (16#b2#,     16#0f#),
      (16#c4#,     16#5c#),
      (16#a6#,     16#00#),
      (16#a7#,     16#20#),
      (16#a7#,     16#d8#),
      (16#a7#,     16#1b#),
      (16#a7#,     16#31#),
      (16#a7#,     16#00#),
      (16#a7#,     16#18#),
      (16#a7#,     16#20#),
      (16#a7#,     16#d8#),
      (16#a7#,     16#19#),
      (16#a7#,     16#31#),
      (16#a7#,     16#00#),
      (16#a7#,     16#18#),
      (16#a7#,     16#20#),
      (16#a7#,     16#d8#),
      (16#a7#,     16#19#),
      (16#a7#,     16#31#),
      (16#a7#,     16#00#),
      (16#a7#,     16#18#),
      (16#7f#,     16#00#),
      (16#e5#,     16#1f#),
      (16#e1#,     16#77#),
      (16#dd#,     16#7f#),
      (REG_DSP_CTRL0,    CTRL0_YUV422 or CTRL0_YUV_EN or CTRL0_RGB_EN),
      (16#00#,     16#00#)
     );

   procedure Write (This : OV2640_Camera; Addr, Data : UInt8);
   function Read (This : OV2640_Camera; Addr : UInt8) return UInt8;
   procedure Select_Sensor_Bank (This : OV2640_Camera);
   procedure Select_DSP_Bank (This : OV2640_Camera);
   procedure Enable_DSP (This : OV2640_Camera; Enable : Boolean);

   -----------
   -- Write --
   -----------

   procedure Write (This : OV2640_Camera; Addr, Data : UInt8) is
      Status : I2C_Status;
   begin
      This.I2C.Mem_Write (Addr          => This.Addr,
                          Mem_Addr      => UInt16 (Addr),
                          Mem_Addr_Size => Memory_Size_8b,
                          Data          => (1 => Data),
                          Status        => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write;

   ----------
   -- Read --
   ----------

   function Read (This : OV2640_Camera; Addr : UInt8) return UInt8 is
      Data : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      This.I2C.Mem_Read (Addr          => This.Addr,
                         Mem_Addr      => UInt16 (Addr),
                         Mem_Addr_Size => Memory_Size_8b,
                         Data          => Data,
                         Status        => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      return Data (Data'First);
   end Read;

   ------------------------
   -- Select_Sensor_Bank --
   ------------------------

   procedure Select_Sensor_Bank (This : OV2640_Camera) is
   begin
      Write (This, REG_BANK_SELECT, 1);
   end Select_Sensor_Bank;

   ---------------------
   -- Select_DSP_Bank --
   ---------------------

   procedure Select_DSP_Bank (This : OV2640_Camera) is
   begin
      Write (This, REG_BANK_SELECT, 0);
   end Select_DSP_Bank;

   ----------------
   -- Enable_DSP --
   ----------------

   procedure Enable_DSP (This : OV2640_Camera; Enable : Boolean) is
   begin
      Select_DSP_Bank (This);
      Write (This, REG_DSP_BYPASS, (if Enable then 0 else 1));
   end Enable_DSP;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out OV2640_Camera;
      Addr : UInt10)
   is
   begin
      This.Addr     := Addr;

      for Elt of Setup_Commands loop
         Write (This, Elt.Addr, Elt.Data);
      end loop;
   end Initialize;

   ----------------------
   -- Set_Pixel_Format --
   ----------------------

   procedure Set_Pixel_Format
     (This : OV2640_Camera;
      Pix : Pixel_Format)
   is
   begin
      Select_DSP_Bank (This);
      Write (This, REG_DSP_RESET, 2#0000_0100#); --  DVP
      case Pix is
         when Pix_RGB565 =>
            Write (This, REG_DSP_IMAGE_MODE, 2#0000_1001#);
         when Pix_YUV422 =>
            Write (This, REG_DSP_IMAGE_MODE, 2#0000_0001#);
         when Pix_JPEG =>
            Write (This, REG_DSP_IMAGE_MODE, 2#0001_1000#);
            Write (This, REG_DSP_QS, 16#0C#);
      end case;
      --  Write 0xD7 := 0x03 (not documented)
      --  Write 0xE1 := 0X77 (not documented)
      Write (This, REG_DSP_RESET, 0);
   end Set_Pixel_Format;

   --------------------
   -- Set_Frame_Size --
   --------------------

   procedure Set_Frame_Size
     (This : OV2640_Camera;
      Res  : Frame_Size)
   is
      H_SIZE, V_SIZE : Bit_Field (0 .. 15);
      Width : constant UInt16 := Resolutions (Res).Width;
      Height : constant UInt16 := Resolutions (Res).Height;
      Is_UXGA : constant Boolean := Res = SXGA or else Res = UXGA;
      CLK_Divider : constant Boolean := Is_UXGA;
   begin

      Enable_DSP (This, False);
      --  DSP bank selected

      Write (This, REG_DSP_ZMOW, UInt8 ((Width / 4) and 16#FF#));
      Write (This, REG_DSP_ZMOH, UInt8 ((Height / 4) and 16#FF#));
      Write (This, REG_DSP_ZMHH,
             UInt8 (Shift_Right (Width, 10) and 16#3#)
             or
             UInt8 (Shift_Right (Height, 8) and 16#4#));

      Select_Sensor_Bank (This);
      Write (This, REG_SENSOR_CLKRC, (if CLK_Divider then 16#81# else 16#80#));

      --  The sensor has only two mode (UXGA and SVGA), the resolution is then
      --  scaled down by ZMOW, ZMOH and ZMHH.

      Select_Sensor_Bank (This);
      Write (This, REG_SENSOR_COM7,    (if Is_UXGA then 16#00# else 16#40#));
      Write (This, REG_SENSOR_COM1,    (if Is_UXGA then 16#0F# else 16#0A#));
      Write (This, REG_SENSOR_REG32,   (if Is_UXGA then 16#36# else 16#09#));
      Write (This, REG_SENSOR_HREFST,  (if Is_UXGA then 16#11# else 16#11#));
      Write (This, REG_SENSOR_HREFEND, (if Is_UXGA then 16#75# else 16#43#));
      Write (This, REG_SENSOR_VSTRT,   (if Is_UXGA then 16#01# else 16#00#));
      Write (This, REG_SENSOR_VEND,    (if Is_UXGA then 16#97# else 16#4B#));

      --  Not documented...
      Write (This, 16#3D#, (if Is_UXGA then 16#34# else 16#38#));
      Write (This, 16#35#, (if Is_UXGA then 16#88# else 16#DA#));
      Write (This, 16#22#, (if Is_UXGA then 16#0A# else 16#1A#));
      Write (This, 16#37#, (if Is_UXGA then 16#40# else 16#C3#));
      Write (This, 16#34#, (if Is_UXGA then 16#A0# else 16#C0#));
      Write (This, 16#06#, (if Is_UXGA then 16#02# else 16#88#));
      Write (This, 16#0D#, (if Is_UXGA then 16#B7# else 16#87#));
      Write (This, 16#0E#, (if Is_UXGA then 16#01# else 16#41#));
      Write (This, 16#42#, (if Is_UXGA then 16#83# else 16#03#));

      Enable_DSP (This, False);
      --  DSP bank selected

      Write (This, REG_DSP_RESET, 2#0000_0100#); --  DVP

      --  HSIZE8, VSIZE8 and SIZEL use the rela values, where HZISE, VSIZE,
      --  VHYX use the value divided by 4 (shifted by 3)...

      if Is_UXGA then
         H_SIZE := To_Bit_Field (Resolutions (UXGA).Width);
         V_SIZE := To_Bit_Field (Resolutions (UXGA).Height);
      else
         H_SIZE := To_Bit_Field (Resolutions (SVGA).Width);
         V_SIZE := To_Bit_Field (Resolutions (SVGA).Height);
      end if;

      --  Real HSIZE[10..3]
      Write (This, REG_DSP_HSIZE8, To_UInt8 (H_SIZE (3 .. 10)));
      --  Real VSIZE[10..3]
      Write (This, REG_DSP_VSIZE8, To_UInt8 (V_SIZE (3 .. 10)));

      --  Real HSIZE[11] real HSIZE[2..0]
      Write (This, REG_DSP_SIZEL,
             To_UInt8 (V_SIZE (0 .. 2) & H_SIZE (0 .. 2) & (H_SIZE (11), 0)));

      H_SIZE := To_Bit_Field (To_UInt16 (H_SIZE) / 4);
      V_SIZE := To_Bit_Field (To_UInt16 (V_SIZE) / 4);

      Write (This, REG_DSP_XOFFL, 0);
      Write (This, REG_DSP_YOFFL, 0);
      Write (This, REG_DSP_HSIZE, To_UInt8 (H_SIZE (0 .. 7)));
      Write (This, REG_DSP_VSIZE, To_UInt8 (V_SIZE (0 .. 7)));

      Write (This, REG_DSP_VHYX,
             To_UInt8 ((0 => 0,
                       1 => 0,
                       2 => 0,
                       3 => H_SIZE (8),
                       4 => 0,
                       5 => 0,
                       6 => 0,
                       7 => V_SIZE (8))));
      Write (This, REG_DSP_TEST,
             To_UInt8 ((0 => 0,
                       1 => 0,
                       2 => 0,
                       3 => 0,
                       4 => 0,
                       5 => 0,
                       6 => 0,
                       7 => H_SIZE (9))));

      Write (This, REG_DSP_CTRL2, 2#0011_1101#);
      Write (This, REG_DSP_CTRLI, 2#1000_0000#); --  LP_DP

      if Is_UXGA then
         Write (This, REG_DSP_R_DVP_SP, 0);  --  AUTO Mode, Div 0
      else
         Write (This, REG_DSP_R_DVP_SP, 4);  --  AUTO Mode, Div 4
      end if;
      Enable_DSP (This, True);
      Write (This, REG_DSP_RESET, 0);
   end Set_Frame_Size;

   --------------------
   -- Set_Frame_Rate --
   --------------------

   procedure Set_Frame_Rate
     (This : OV2640_Camera;
      FR  : Frame_Rate)
   is
   begin
      null;
   end Set_Frame_Rate;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (This : OV2640_Camera) return UInt8 is
   begin
      Select_Sensor_Bank (This);
      return Read (This, REG_SENSOR_PID);
   end Get_PID;

   ------------------------------
   -- Enable_Auto_Gain_Control --
   ------------------------------

   procedure Enable_Auto_Gain_Control (This   : OV2640_Camera;
                                       Enable : Boolean := True)
   is
      COM8 : UInt8;
   begin
      Select_Sensor_Bank (This);
      COM8 := Read (This, REG_SENSOR_COM8);

      if Enable then
         COM8 := COM8 or 2#0000_0100#;
      else
         COM8 := COM8 and 2#1111_1011#;
      end if;
      Write (This, REG_SENSOR_COM8, COM8);
   end Enable_Auto_Gain_Control;

   -------------------------------
   -- Enable_Auto_White_Balance --
   -------------------------------

   procedure Enable_Auto_White_Balance (This   : OV2640_Camera;
                                        Enable : Boolean := True)
   is
      CTRL1 : UInt8;
   begin
      Select_DSP_Bank (This);
      CTRL1 := Read (This, REG_DSP_CTRL1);

      if Enable then
         CTRL1 := CTRL1 or 2#0000_1000#;
      else
         CTRL1 := CTRL1 and 2#1111_0111#;
      end if;
      Write (This, REG_DSP_CTRL1, CTRL1);
   end Enable_Auto_White_Balance;

   ----------------------------------
   -- Enable_Auto_Exposure_Control --
   ----------------------------------

   procedure Enable_Auto_Exposure_Control (This   : OV2640_Camera;
                                           Enable : Boolean := True)
   is
      CTRL0 : UInt8;
   begin
      Select_DSP_Bank (This);
      CTRL0 := Read (This, REG_DSP_CTRL0);

      if Enable then
         CTRL0 := CTRL0 or 2#1000_0000#;
      else
         CTRL0 := CTRL0 and 2#0111_1111#;
      end if;
      Write (This, REG_DSP_CTRL0, CTRL0);
   end Enable_Auto_Exposure_Control;

   -----------------------------
   -- Enable_Auto_Band_Filter --
   -----------------------------

   procedure Enable_Auto_Band_Filter (This   : OV2640_Camera;
                                      Enable : Boolean := True)
   is
      COM8 : UInt8;
   begin
      Select_Sensor_Bank (This);
      COM8 := Read (This, REG_SENSOR_COM8);

      if Enable then
         COM8 := COM8 or 2#0010_0000#;
      else
         COM8 := COM8 and 2#1101_1111#;
      end if;
      Write (This, REG_SENSOR_COM8, COM8);
   end Enable_Auto_Band_Filter;

end OV2640;
