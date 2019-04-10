--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.ISI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype ISI_ISI_CFG1_FRATE_Field is HAL.UInt3;

   --  Threshold Mask
   type ISI_CFG1_THMASK_Field is
     (
      --  Only 4 beats AHB burst allowed
      Beats_4,
      --  Only 4 and 8 beats AHB burst allowed
      Beats_8,
      --  4, 8 and 16 beats AHB burst allowed
      Beats_16)
     with Size => 2;
   for ISI_CFG1_THMASK_Field use
     (Beats_4 => 0,
      Beats_8 => 1,
      Beats_16 => 2);

   subtype ISI_ISI_CFG1_SLD_Field is HAL.UInt8;
   subtype ISI_ISI_CFG1_SFD_Field is HAL.UInt8;

   --  ISI Configuration 1 Register
   type ISI_ISI_CFG1_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Horizontal Synchronization Polarity
      HSYNC_POL      : Boolean := False;
      --  Vertical Synchronization Polarity
      VSYNC_POL      : Boolean := False;
      --  Pixel Clock Polarity
      PIXCLK_POL     : Boolean := False;
      --  Grayscale Little Endian
      GRAYLE         : Boolean := False;
      --  Embedded Synchronization
      EMB_SYNC       : Boolean := False;
      --  Embedded Synchronization Correction
      CRC_SYNC       : Boolean := False;
      --  Frame Rate [0..7]
      FRATE          : ISI_ISI_CFG1_FRATE_Field := 16#0#;
      --  Disable Codec Request
      DISCR          : Boolean := False;
      --  Full Mode is Allowed
      FULL           : Boolean := False;
      --  Threshold Mask
      THMASK         : ISI_CFG1_THMASK_Field := SAM_SVD.ISI.Beats_4;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Start of Line Delay
      SLD            : ISI_ISI_CFG1_SLD_Field := 16#0#;
      --  Start of Frame Delay
      SFD            : ISI_ISI_CFG1_SFD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_CFG1_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      HSYNC_POL      at 0 range 2 .. 2;
      VSYNC_POL      at 0 range 3 .. 3;
      PIXCLK_POL     at 0 range 4 .. 4;
      GRAYLE         at 0 range 5 .. 5;
      EMB_SYNC       at 0 range 6 .. 6;
      CRC_SYNC       at 0 range 7 .. 7;
      FRATE          at 0 range 8 .. 10;
      DISCR          at 0 range 11 .. 11;
      FULL           at 0 range 12 .. 12;
      THMASK         at 0 range 13 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SLD            at 0 range 16 .. 23;
      SFD            at 0 range 24 .. 31;
   end record;

   subtype ISI_ISI_CFG2_IM_VSIZE_Field is HAL.UInt11;
   subtype ISI_ISI_CFG2_IM_HSIZE_Field is HAL.UInt11;

   --  YCrCb Format Swap Mode
   type ISI_CFG2_YCC_SWAP_Field is
     (
      --  Byte 0 Cb(i)Byte 1 Y(i)Byte 2 Cr(i)Byte 3 Y(i+1)
      Default,
      --  Byte 0 Cr(i)Byte 1 Y(i)Byte 2 Cb(i)Byte 3 Y(i+1)
      Mode1,
      --  Byte 0 Y(i)Byte 1 Cb(i)Byte 2 Y(i+1)Byte 3 Cr(i)
      Mode2,
      --  Byte 0 Y(i)Byte 1 Cr(i)Byte 2 Y(i+1)Byte 3 Cb(i)
      Mode3)
     with Size => 2;
   for ISI_CFG2_YCC_SWAP_Field use
     (Default => 0,
      Mode1 => 1,
      Mode2 => 2,
      Mode3 => 3);

   --  RGB Pixel Mapping Configuration
   type ISI_CFG2_RGB_CFG_Field is
     (
      --  Byte 0 R/G(MSB)Byte 1 G(LSB)/BByte 2 R/G(MSB)Byte 3 G(LSB)/B
      Default,
      --  Byte 0 B/G(MSB)Byte 1 G(LSB)/RByte 2 B/G(MSB)Byte 3 G(LSB)/R
      Mode1,
      --  Byte 0 G(LSB)/RByte 1 B/G(MSB)Byte 2 G(LSB)/RByte 3 B/G(MSB)
      Mode2,
      --  Byte 0 G(LSB)/BByte 1 R/G(MSB)Byte 2 G(LSB)/BByte 3 R/G(MSB)
      Mode3)
     with Size => 2;
   for ISI_CFG2_RGB_CFG_Field use
     (Default => 0,
      Mode1 => 1,
      Mode2 => 2,
      Mode3 => 3);

   --  ISI Configuration 2 Register
   type ISI_ISI_CFG2_Register is record
      --  Vertical Size of the Image Sensor [0..2047]
      IM_VSIZE       : ISI_ISI_CFG2_IM_VSIZE_Field := 16#0#;
      --  Grayscale Pixel Format Mode
      GS_MODE        : Boolean := False;
      --  RGB Input Mode
      RGB_MODE       : Boolean := False;
      --  Grayscale Mode Format Enable
      GRAYSCALE      : Boolean := False;
      --  RGB Format Swap Mode
      RGB_SWAP       : Boolean := False;
      --  Color Space for the Image Data
      COL_SPACE      : Boolean := False;
      --  Horizontal Size of the Image Sensor [0..2047]
      IM_HSIZE       : ISI_ISI_CFG2_IM_HSIZE_Field := 16#0#;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  YCrCb Format Swap Mode
      YCC_SWAP       : ISI_CFG2_YCC_SWAP_Field := SAM_SVD.ISI.Default;
      --  RGB Pixel Mapping Configuration
      RGB_CFG        : ISI_CFG2_RGB_CFG_Field := SAM_SVD.ISI.Default;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_CFG2_Register use record
      IM_VSIZE       at 0 range 0 .. 10;
      GS_MODE        at 0 range 11 .. 11;
      RGB_MODE       at 0 range 12 .. 12;
      GRAYSCALE      at 0 range 13 .. 13;
      RGB_SWAP       at 0 range 14 .. 14;
      COL_SPACE      at 0 range 15 .. 15;
      IM_HSIZE       at 0 range 16 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      YCC_SWAP       at 0 range 28 .. 29;
      RGB_CFG        at 0 range 30 .. 31;
   end record;

   subtype ISI_ISI_PSIZE_PREV_VSIZE_Field is HAL.UInt10;
   subtype ISI_ISI_PSIZE_PREV_HSIZE_Field is HAL.UInt10;

   --  ISI Preview Size Register
   type ISI_ISI_PSIZE_Register is record
      --  Vertical Size for the Preview Path
      PREV_VSIZE     : ISI_ISI_PSIZE_PREV_VSIZE_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Horizontal Size for the Preview Path
      PREV_HSIZE     : ISI_ISI_PSIZE_PREV_HSIZE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_PSIZE_Register use record
      PREV_VSIZE     at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      PREV_HSIZE     at 0 range 16 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype ISI_ISI_PDECF_DEC_FACTOR_Field is HAL.UInt8;

   --  ISI Preview Decimation Factor Register
   type ISI_ISI_PDECF_Register is record
      --  Decimation Factor
      DEC_FACTOR    : ISI_ISI_PDECF_DEC_FACTOR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_PDECF_Register use record
      DEC_FACTOR    at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  ISI_ISI_Y2R_SET0_C array element
   subtype ISI_ISI_Y2R_SET0_C_Element is HAL.UInt8;

   --  ISI_ISI_Y2R_SET0_C array
   type ISI_ISI_Y2R_SET0_C_Field_Array is array (0 .. 3)
     of ISI_ISI_Y2R_SET0_C_Element
     with Component_Size => 8, Size => 32;

   --  ISI Color Space Conversion YCrCb To RGB Set 0 Register
   type ISI_ISI_Y2R_SET0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  C as a value
            Val : HAL.UInt32;
         when True =>
            --  C as an array
            Arr : ISI_ISI_Y2R_SET0_C_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_Y2R_SET0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype ISI_ISI_Y2R_SET1_C4_Field is HAL.UInt9;

   --  ISI Color Space Conversion YCrCb To RGB Set 1 Register
   type ISI_ISI_Y2R_SET1_Register is record
      --  Color Space Conversion Matrix Coefficient C4
      C4             : ISI_ISI_Y2R_SET1_C4_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Color Space Conversion Luminance Default Offset
      Yoff           : Boolean := False;
      --  Color Space Conversion Red Chrominance Default Offset
      Croff          : Boolean := False;
      --  Color Space Conversion Blue Chrominance Default Offset
      Cboff          : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_Y2R_SET1_Register use record
      C4             at 0 range 0 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      Yoff           at 0 range 12 .. 12;
      Croff          at 0 range 13 .. 13;
      Cboff          at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype ISI_ISI_R2Y_SET0_C0_Field is HAL.UInt7;
   subtype ISI_ISI_R2Y_SET0_C1_Field is HAL.UInt7;
   subtype ISI_ISI_R2Y_SET0_C2_Field is HAL.UInt7;

   --  ISI Color Space Conversion RGB To YCrCb Set 0 Register
   type ISI_ISI_R2Y_SET0_Register is record
      --  Color Space Conversion Matrix Coefficient C0
      C0             : ISI_ISI_R2Y_SET0_C0_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Color Space Conversion Matrix Coefficient C1
      C1             : ISI_ISI_R2Y_SET0_C1_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Color Space Conversion Matrix Coefficient C2
      C2             : ISI_ISI_R2Y_SET0_C2_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Color Space Conversion Red Component Offset
      Roff           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_R2Y_SET0_Register use record
      C0             at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      C1             at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      C2             at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      Roff           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ISI_ISI_R2Y_SET1_C3_Field is HAL.UInt7;
   subtype ISI_ISI_R2Y_SET1_C4_Field is HAL.UInt7;
   subtype ISI_ISI_R2Y_SET1_C5_Field is HAL.UInt7;

   --  ISI Color Space Conversion RGB To YCrCb Set 1 Register
   type ISI_ISI_R2Y_SET1_Register is record
      --  Color Space Conversion Matrix Coefficient C3
      C3             : ISI_ISI_R2Y_SET1_C3_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Color Space Conversion Matrix Coefficient C4
      C4             : ISI_ISI_R2Y_SET1_C4_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Color Space Conversion Matrix Coefficient C5
      C5             : ISI_ISI_R2Y_SET1_C5_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Color Space Conversion Green Component Offset
      Goff           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_R2Y_SET1_Register use record
      C3             at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      C4             at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      C5             at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      Goff           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ISI_ISI_R2Y_SET2_C6_Field is HAL.UInt7;
   subtype ISI_ISI_R2Y_SET2_C7_Field is HAL.UInt7;
   subtype ISI_ISI_R2Y_SET2_C8_Field is HAL.UInt7;

   --  ISI Color Space Conversion RGB To YCrCb Set 2 Register
   type ISI_ISI_R2Y_SET2_Register is record
      --  Color Space Conversion Matrix Coefficient C6
      C6             : ISI_ISI_R2Y_SET2_C6_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Color Space Conversion Matrix Coefficient C7
      C7             : ISI_ISI_R2Y_SET2_C7_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Color Space Conversion Matrix Coefficient C8
      C8             : ISI_ISI_R2Y_SET2_C8_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Color Space Conversion Blue Component Offset
      Boff           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_R2Y_SET2_Register use record
      C6             at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      C7             at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      C8             at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      Boff           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  ISI Control Register
   type ISI_ISI_CR_Register is record
      --  Write-only. ISI Module Enable Request
      ISI_EN        : Boolean := False;
      --  Write-only. ISI Module Disable Request
      ISI_DIS       : Boolean := False;
      --  Write-only. ISI Software Reset Request
      ISI_SRST      : Boolean := False;
      --  unspecified
      Reserved_3_7  : HAL.UInt5 := 16#0#;
      --  Write-only. ISI Codec Request
      ISI_CDC       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_CR_Register use record
      ISI_EN        at 0 range 0 .. 0;
      ISI_DIS       at 0 range 1 .. 1;
      ISI_SRST      at 0 range 2 .. 2;
      Reserved_3_7  at 0 range 3 .. 7;
      ISI_CDC       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  ISI Status Register
   type ISI_ISI_SR_Register is record
      --  Read-only. Module Enable
      ENABLE         : Boolean;
      --  Read-only. Module Disable Request has Terminated (cleared on read)
      DIS_DONE       : Boolean;
      --  Read-only. Module Software Reset Request has Terminated (cleared on
      --  read)
      SRST           : Boolean;
      --  unspecified
      Reserved_3_7   : HAL.UInt5;
      --  Read-only. Pending Codec Request
      CDC_PND        : Boolean;
      --  unspecified
      Reserved_9_9   : HAL.Bit;
      --  Read-only. Vertical Synchronization (cleared on read)
      VSYNC          : Boolean;
      --  unspecified
      Reserved_11_15 : HAL.UInt5;
      --  Read-only. Preview DMA Transfer has Terminated (cleared on read)
      PXFR_DONE      : Boolean;
      --  Read-only. Codec DMA Transfer has Terminated (cleared on read)
      CXFR_DONE      : Boolean;
      --  unspecified
      Reserved_18_18 : HAL.Bit;
      --  Read-only. Synchronization in Progress
      SIP            : Boolean;
      --  unspecified
      Reserved_20_23 : HAL.UInt4;
      --  Read-only. Preview Datapath Overflow (cleared on read)
      P_OVR          : Boolean;
      --  Read-only. Codec Datapath Overflow (cleared on read)
      C_OVR          : Boolean;
      --  Read-only. CRC Synchronization Error (cleared on read)
      CRC_ERR        : Boolean;
      --  Read-only. Frame Rate Overrun (cleared on read)
      FR_OVR         : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_SR_Register use record
      ENABLE         at 0 range 0 .. 0;
      DIS_DONE       at 0 range 1 .. 1;
      SRST           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      CDC_PND        at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      VSYNC          at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      PXFR_DONE      at 0 range 16 .. 16;
      CXFR_DONE      at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      SIP            at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      P_OVR          at 0 range 24 .. 24;
      C_OVR          at 0 range 25 .. 25;
      CRC_ERR        at 0 range 26 .. 26;
      FR_OVR         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  ISI Interrupt Enable Register
   type ISI_ISI_IER_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write-only. Disable Done Interrupt Enable
      DIS_DONE       : Boolean := False;
      --  Write-only. Software Reset Interrupt Enable
      SRST           : Boolean := False;
      --  unspecified
      Reserved_3_9   : HAL.UInt7 := 16#0#;
      --  Write-only. Vertical Synchronization Interrupt Enable
      VSYNC          : Boolean := False;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Write-only. Preview DMA Transfer Done Interrupt Enable
      PXFR_DONE      : Boolean := False;
      --  Write-only. Codec DMA Transfer Done Interrupt Enable
      CXFR_DONE      : Boolean := False;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Write-only. Preview Datapath Overflow Interrupt Enable
      P_OVR          : Boolean := False;
      --  Write-only. Codec Datapath Overflow Interrupt Enable
      C_OVR          : Boolean := False;
      --  Write-only. Embedded Synchronization CRC Error Interrupt Enable
      CRC_ERR        : Boolean := False;
      --  Write-only. Frame Rate Overflow Interrupt Enable
      FR_OVR         : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_IER_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      DIS_DONE       at 0 range 1 .. 1;
      SRST           at 0 range 2 .. 2;
      Reserved_3_9   at 0 range 3 .. 9;
      VSYNC          at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      PXFR_DONE      at 0 range 16 .. 16;
      CXFR_DONE      at 0 range 17 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      P_OVR          at 0 range 24 .. 24;
      C_OVR          at 0 range 25 .. 25;
      CRC_ERR        at 0 range 26 .. 26;
      FR_OVR         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  ISI Interrupt Disable Register
   type ISI_ISI_IDR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write-only. Disable Done Interrupt Disable
      DIS_DONE       : Boolean := False;
      --  Write-only. Software Reset Interrupt Disable
      SRST           : Boolean := False;
      --  unspecified
      Reserved_3_9   : HAL.UInt7 := 16#0#;
      --  Write-only. Vertical Synchronization Interrupt Disable
      VSYNC          : Boolean := False;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Write-only. Preview DMA Transfer Done Interrupt Disable
      PXFR_DONE      : Boolean := False;
      --  Write-only. Codec DMA Transfer Done Interrupt Disable
      CXFR_DONE      : Boolean := False;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Write-only. Preview Datapath Overflow Interrupt Disable
      P_OVR          : Boolean := False;
      --  Write-only. Codec Datapath Overflow Interrupt Disable
      C_OVR          : Boolean := False;
      --  Write-only. Embedded Synchronization CRC Error Interrupt Disable
      CRC_ERR        : Boolean := False;
      --  Write-only. Frame Rate Overflow Interrupt Disable
      FR_OVR         : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_IDR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      DIS_DONE       at 0 range 1 .. 1;
      SRST           at 0 range 2 .. 2;
      Reserved_3_9   at 0 range 3 .. 9;
      VSYNC          at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      PXFR_DONE      at 0 range 16 .. 16;
      CXFR_DONE      at 0 range 17 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      P_OVR          at 0 range 24 .. 24;
      C_OVR          at 0 range 25 .. 25;
      CRC_ERR        at 0 range 26 .. 26;
      FR_OVR         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  ISI Interrupt Mask Register
   type ISI_ISI_IMR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit;
      --  Read-only. Module Disable Operation Completed
      DIS_DONE       : Boolean;
      --  Read-only. Software Reset Completed
      SRST           : Boolean;
      --  unspecified
      Reserved_3_9   : HAL.UInt7;
      --  Read-only. Vertical Synchronization
      VSYNC          : Boolean;
      --  unspecified
      Reserved_11_15 : HAL.UInt5;
      --  Read-only. Preview DMA Transfer Completed
      PXFR_DONE      : Boolean;
      --  Read-only. Codec DMA Transfer Completed
      CXFR_DONE      : Boolean;
      --  unspecified
      Reserved_18_23 : HAL.UInt6;
      --  Read-only. Preview FIFO Overflow
      P_OVR          : Boolean;
      --  Read-only. Codec FIFO Overflow
      C_OVR          : Boolean;
      --  Read-only. CRC Synchronization Error
      CRC_ERR        : Boolean;
      --  Read-only. Frame Rate Overrun
      FR_OVR         : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_IMR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      DIS_DONE       at 0 range 1 .. 1;
      SRST           at 0 range 2 .. 2;
      Reserved_3_9   at 0 range 3 .. 9;
      VSYNC          at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      PXFR_DONE      at 0 range 16 .. 16;
      CXFR_DONE      at 0 range 17 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      P_OVR          at 0 range 24 .. 24;
      C_OVR          at 0 range 25 .. 25;
      CRC_ERR        at 0 range 26 .. 26;
      FR_OVR         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  DMA Channel Enable Register
   type ISI_ISI_DMA_CHER_Register is record
      --  Write-only. Preview Channel Enable
      P_CH_EN       : Boolean := False;
      --  Write-only. Codec Channel Enable
      C_CH_EN       : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_CHER_Register use record
      P_CH_EN       at 0 range 0 .. 0;
      C_CH_EN       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  DMA Channel Disable Register
   type ISI_ISI_DMA_CHDR_Register is record
      --  Write-only. Preview Channel Disable Request
      P_CH_DIS      : Boolean := False;
      --  Write-only. Codec Channel Disable Request
      C_CH_DIS      : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_CHDR_Register use record
      P_CH_DIS      at 0 range 0 .. 0;
      C_CH_DIS      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  DMA Channel Status Register
   type ISI_ISI_DMA_CHSR_Register is record
      --  Read-only. Preview DMA Channel Status
      P_CH_S        : Boolean;
      --  Read-only. Code DMA Channel Status
      C_CH_S        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_CHSR_Register use record
      P_CH_S        at 0 range 0 .. 0;
      C_CH_S        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype ISI_ISI_DMA_P_ADDR_P_ADDR_Field is HAL.UInt30;

   --  DMA Preview Base Address Register
   type ISI_ISI_DMA_P_ADDR_Register is record
      --  unspecified
      Reserved_0_1 : HAL.UInt2 := 16#0#;
      --  Preview Image Base Address
      P_ADDR       : ISI_ISI_DMA_P_ADDR_P_ADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_P_ADDR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      P_ADDR       at 0 range 2 .. 31;
   end record;

   --  DMA Preview Control Register
   type ISI_ISI_DMA_P_CTRL_Register is record
      --  Descriptor Fetch Control Bit
      P_FETCH       : Boolean := False;
      --  Descriptor Writeback Control Bit
      P_WB          : Boolean := False;
      --  Transfer Done Flag Control
      P_IEN         : Boolean := False;
      --  Preview Transfer Done
      P_DONE        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_P_CTRL_Register use record
      P_FETCH       at 0 range 0 .. 0;
      P_WB          at 0 range 1 .. 1;
      P_IEN         at 0 range 2 .. 2;
      P_DONE        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype ISI_ISI_DMA_P_DSCR_P_DSCR_Field is HAL.UInt30;

   --  DMA Preview Descriptor Address Register
   type ISI_ISI_DMA_P_DSCR_Register is record
      --  unspecified
      Reserved_0_1 : HAL.UInt2 := 16#0#;
      --  Preview Descriptor Base Address
      P_DSCR       : ISI_ISI_DMA_P_DSCR_P_DSCR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_P_DSCR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      P_DSCR       at 0 range 2 .. 31;
   end record;

   subtype ISI_ISI_DMA_C_ADDR_C_ADDR_Field is HAL.UInt30;

   --  DMA Codec Base Address Register
   type ISI_ISI_DMA_C_ADDR_Register is record
      --  unspecified
      Reserved_0_1 : HAL.UInt2 := 16#0#;
      --  Codec Image Base Address
      C_ADDR       : ISI_ISI_DMA_C_ADDR_C_ADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_C_ADDR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      C_ADDR       at 0 range 2 .. 31;
   end record;

   --  DMA Codec Control Register
   type ISI_ISI_DMA_C_CTRL_Register is record
      --  Descriptor Fetch Control Bit
      C_FETCH       : Boolean := False;
      --  Descriptor Writeback Control Bit
      C_WB          : Boolean := False;
      --  Transfer Done Flag Control
      C_IEN         : Boolean := False;
      --  Codec Transfer Done
      C_DONE        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_C_CTRL_Register use record
      C_FETCH       at 0 range 0 .. 0;
      C_WB          at 0 range 1 .. 1;
      C_IEN         at 0 range 2 .. 2;
      C_DONE        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype ISI_ISI_DMA_C_DSCR_C_DSCR_Field is HAL.UInt30;

   --  DMA Codec Descriptor Address Register
   type ISI_ISI_DMA_C_DSCR_Register is record
      --  unspecified
      Reserved_0_1 : HAL.UInt2 := 16#0#;
      --  Codec Descriptor Base Address
      C_DSCR       : ISI_ISI_DMA_C_DSCR_C_DSCR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_DMA_C_DSCR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      C_DSCR       at 0 range 2 .. 31;
   end record;

   --  Write Protection Key Password
   type ISI_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Isi_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit.Always reads as 0.
      Passwd)
     with Size => 24;
   for ISI_WPMR_WPKEY_Field use
     (Isi_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 4805449);

   --  Write Protection Mode Register
   type ISI_ISI_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
      --  Write Protection Key Password
      WPKEY        : ISI_WPMR_WPKEY_Field := Isi_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype ISI_ISI_WPSR_WPVSRC_Field is HAL.UInt16;

   --  Write Protection Status Register
   type ISI_ISI_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : ISI_ISI_WPSR_WPVSRC_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype ISI_ISI_VERSION_VERSION_Field is HAL.UInt12;
   subtype ISI_ISI_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type ISI_ISI_VERSION_Register is record
      --  Read-only. Version of the Hardware Module
      VERSION        : ISI_ISI_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : ISI_ISI_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISI_ISI_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Image Sensor Interface
   type ISI_Peripheral is record
      --  ISI Configuration 1 Register
      ISI_CFG1       : aliased ISI_ISI_CFG1_Register;
      --  ISI Configuration 2 Register
      ISI_CFG2       : aliased ISI_ISI_CFG2_Register;
      --  ISI Preview Size Register
      ISI_PSIZE      : aliased ISI_ISI_PSIZE_Register;
      --  ISI Preview Decimation Factor Register
      ISI_PDECF      : aliased ISI_ISI_PDECF_Register;
      --  ISI Color Space Conversion YCrCb To RGB Set 0 Register
      ISI_Y2R_SET0   : aliased ISI_ISI_Y2R_SET0_Register;
      --  ISI Color Space Conversion YCrCb To RGB Set 1 Register
      ISI_Y2R_SET1   : aliased ISI_ISI_Y2R_SET1_Register;
      --  ISI Color Space Conversion RGB To YCrCb Set 0 Register
      ISI_R2Y_SET0   : aliased ISI_ISI_R2Y_SET0_Register;
      --  ISI Color Space Conversion RGB To YCrCb Set 1 Register
      ISI_R2Y_SET1   : aliased ISI_ISI_R2Y_SET1_Register;
      --  ISI Color Space Conversion RGB To YCrCb Set 2 Register
      ISI_R2Y_SET2   : aliased ISI_ISI_R2Y_SET2_Register;
      --  ISI Control Register
      ISI_CR         : aliased ISI_ISI_CR_Register;
      --  ISI Status Register
      ISI_SR         : aliased ISI_ISI_SR_Register;
      --  ISI Interrupt Enable Register
      ISI_IER        : aliased ISI_ISI_IER_Register;
      --  ISI Interrupt Disable Register
      ISI_IDR        : aliased ISI_ISI_IDR_Register;
      --  ISI Interrupt Mask Register
      ISI_IMR        : aliased ISI_ISI_IMR_Register;
      --  DMA Channel Enable Register
      ISI_DMA_CHER   : aliased ISI_ISI_DMA_CHER_Register;
      --  DMA Channel Disable Register
      ISI_DMA_CHDR   : aliased ISI_ISI_DMA_CHDR_Register;
      --  DMA Channel Status Register
      ISI_DMA_CHSR   : aliased ISI_ISI_DMA_CHSR_Register;
      --  DMA Preview Base Address Register
      ISI_DMA_P_ADDR : aliased ISI_ISI_DMA_P_ADDR_Register;
      --  DMA Preview Control Register
      ISI_DMA_P_CTRL : aliased ISI_ISI_DMA_P_CTRL_Register;
      --  DMA Preview Descriptor Address Register
      ISI_DMA_P_DSCR : aliased ISI_ISI_DMA_P_DSCR_Register;
      --  DMA Codec Base Address Register
      ISI_DMA_C_ADDR : aliased ISI_ISI_DMA_C_ADDR_Register;
      --  DMA Codec Control Register
      ISI_DMA_C_CTRL : aliased ISI_ISI_DMA_C_CTRL_Register;
      --  DMA Codec Descriptor Address Register
      ISI_DMA_C_DSCR : aliased ISI_ISI_DMA_C_DSCR_Register;
      --  Write Protection Mode Register
      ISI_WPMR       : aliased ISI_ISI_WPMR_Register;
      --  Write Protection Status Register
      ISI_WPSR       : aliased ISI_ISI_WPSR_Register;
      --  Version Register
      ISI_VERSION    : aliased ISI_ISI_VERSION_Register;
   end record
     with Volatile;

   for ISI_Peripheral use record
      ISI_CFG1       at 16#0# range 0 .. 31;
      ISI_CFG2       at 16#4# range 0 .. 31;
      ISI_PSIZE      at 16#8# range 0 .. 31;
      ISI_PDECF      at 16#C# range 0 .. 31;
      ISI_Y2R_SET0   at 16#10# range 0 .. 31;
      ISI_Y2R_SET1   at 16#14# range 0 .. 31;
      ISI_R2Y_SET0   at 16#18# range 0 .. 31;
      ISI_R2Y_SET1   at 16#1C# range 0 .. 31;
      ISI_R2Y_SET2   at 16#20# range 0 .. 31;
      ISI_CR         at 16#24# range 0 .. 31;
      ISI_SR         at 16#28# range 0 .. 31;
      ISI_IER        at 16#2C# range 0 .. 31;
      ISI_IDR        at 16#30# range 0 .. 31;
      ISI_IMR        at 16#34# range 0 .. 31;
      ISI_DMA_CHER   at 16#38# range 0 .. 31;
      ISI_DMA_CHDR   at 16#3C# range 0 .. 31;
      ISI_DMA_CHSR   at 16#40# range 0 .. 31;
      ISI_DMA_P_ADDR at 16#44# range 0 .. 31;
      ISI_DMA_P_CTRL at 16#48# range 0 .. 31;
      ISI_DMA_P_DSCR at 16#4C# range 0 .. 31;
      ISI_DMA_C_ADDR at 16#50# range 0 .. 31;
      ISI_DMA_C_CTRL at 16#54# range 0 .. 31;
      ISI_DMA_C_DSCR at 16#58# range 0 .. 31;
      ISI_WPMR       at 16#E4# range 0 .. 31;
      ISI_WPSR       at 16#E8# range 0 .. 31;
      ISI_VERSION    at 16#FC# range 0 .. 31;
   end record;

   --  Image Sensor Interface
   ISI_Periph : aliased ISI_Peripheral
     with Import, Address => System'To_Address (16#4004C000#);

end SAM_SVD.ISI;
