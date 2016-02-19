--  Automatically generated from STM32F40x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.DCMI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_CAPTURE_Field is STM32_SVD.Bit;
   subtype CR_CM_Field is STM32_SVD.Bit;
   subtype CR_CROP_Field is STM32_SVD.Bit;
   subtype CR_JPEG_Field is STM32_SVD.Bit;
   subtype CR_ESS_Field is STM32_SVD.Bit;
   subtype CR_PCKPOL_Field is STM32_SVD.Bit;
   subtype CR_HSPOL_Field is STM32_SVD.Bit;
   subtype CR_VSPOL_Field is STM32_SVD.Bit;
   subtype CR_FCRC_Field is STM32_SVD.UInt2;
   subtype CR_EDM_Field is STM32_SVD.UInt2;
   subtype CR_ENABLE_Field is STM32_SVD.Bit;

   --  control register 1
   type CR_Register is record
      --  Capture enable
      CAPTURE        : CR_CAPTURE_Field := 16#0#;
      --  Capture mode
      CM             : CR_CM_Field := 16#0#;
      --  Crop feature
      CROP           : CR_CROP_Field := 16#0#;
      --  JPEG format
      JPEG           : CR_JPEG_Field := 16#0#;
      --  Embedded synchronization select
      ESS            : CR_ESS_Field := 16#0#;
      --  Pixel clock polarity
      PCKPOL         : CR_PCKPOL_Field := 16#0#;
      --  Horizontal synchronization polarity
      HSPOL          : CR_HSPOL_Field := 16#0#;
      --  Vertical synchronization polarity
      VSPOL          : CR_VSPOL_Field := 16#0#;
      --  Frame capture rate control
      FCRC           : CR_FCRC_Field := 16#0#;
      --  Extended data mode
      EDM            : CR_EDM_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : STM32_SVD.UInt2 := 16#0#;
      --  DCMI enable
      ENABLE         : CR_ENABLE_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      CAPTURE        at 0 range 0 .. 0;
      CM             at 0 range 1 .. 1;
      CROP           at 0 range 2 .. 2;
      JPEG           at 0 range 3 .. 3;
      ESS            at 0 range 4 .. 4;
      PCKPOL         at 0 range 5 .. 5;
      HSPOL          at 0 range 6 .. 6;
      VSPOL          at 0 range 7 .. 7;
      FCRC           at 0 range 8 .. 9;
      EDM            at 0 range 10 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      ENABLE         at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   subtype SR_HSYNC_Field is STM32_SVD.Bit;
   subtype SR_VSYNC_Field is STM32_SVD.Bit;
   subtype SR_FNE_Field is STM32_SVD.Bit;

   --  status register
   type SR_Register is record
      --  HSYNC
      HSYNC         : SR_HSYNC_Field;
      --  VSYNC
      VSYNC         : SR_VSYNC_Field;
      --  FIFO not empty
      FNE           : SR_FNE_Field;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      HSYNC         at 0 range 0 .. 0;
      VSYNC         at 0 range 1 .. 1;
      FNE           at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   ------------------
   -- RIS_Register --
   ------------------

   subtype RIS_FRAME_RIS_Field is STM32_SVD.Bit;
   subtype RIS_OVR_RIS_Field is STM32_SVD.Bit;
   subtype RIS_ERR_RIS_Field is STM32_SVD.Bit;
   subtype RIS_VSYNC_RIS_Field is STM32_SVD.Bit;
   subtype RIS_LINE_RIS_Field is STM32_SVD.Bit;

   --  raw interrupt status register
   type RIS_Register is record
      --  Capture complete raw interrupt status
      FRAME_RIS     : RIS_FRAME_RIS_Field;
      --  Overrun raw interrupt status
      OVR_RIS       : RIS_OVR_RIS_Field;
      --  Synchronization error raw interrupt status
      ERR_RIS       : RIS_ERR_RIS_Field;
      --  VSYNC raw interrupt status
      VSYNC_RIS     : RIS_VSYNC_RIS_Field;
      --  Line raw interrupt status
      LINE_RIS      : RIS_LINE_RIS_Field;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RIS_Register use record
      FRAME_RIS     at 0 range 0 .. 0;
      OVR_RIS       at 0 range 1 .. 1;
      ERR_RIS       at 0 range 2 .. 2;
      VSYNC_RIS     at 0 range 3 .. 3;
      LINE_RIS      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   ------------------
   -- IER_Register --
   ------------------

   subtype IER_FRAME_IE_Field is STM32_SVD.Bit;
   subtype IER_OVR_IE_Field is STM32_SVD.Bit;
   subtype IER_ERR_IE_Field is STM32_SVD.Bit;
   subtype IER_VSYNC_IE_Field is STM32_SVD.Bit;
   subtype IER_LINE_IE_Field is STM32_SVD.Bit;

   --  interrupt enable register
   type IER_Register is record
      --  Capture complete interrupt enable
      FRAME_IE      : IER_FRAME_IE_Field := 16#0#;
      --  Overrun interrupt enable
      OVR_IE        : IER_OVR_IE_Field := 16#0#;
      --  Synchronization error interrupt enable
      ERR_IE        : IER_ERR_IE_Field := 16#0#;
      --  VSYNC interrupt enable
      VSYNC_IE      : IER_VSYNC_IE_Field := 16#0#;
      --  Line interrupt enable
      LINE_IE       : IER_LINE_IE_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      FRAME_IE      at 0 range 0 .. 0;
      OVR_IE        at 0 range 1 .. 1;
      ERR_IE        at 0 range 2 .. 2;
      VSYNC_IE      at 0 range 3 .. 3;
      LINE_IE       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   ------------------
   -- MIS_Register --
   ------------------

   subtype MIS_FRAME_MIS_Field is STM32_SVD.Bit;
   subtype MIS_OVR_MIS_Field is STM32_SVD.Bit;
   subtype MIS_ERR_MIS_Field is STM32_SVD.Bit;
   subtype MIS_VSYNC_MIS_Field is STM32_SVD.Bit;
   subtype MIS_LINE_MIS_Field is STM32_SVD.Bit;

   --  masked interrupt status register
   type MIS_Register is record
      --  Capture complete masked interrupt status
      FRAME_MIS     : MIS_FRAME_MIS_Field;
      --  Overrun masked interrupt status
      OVR_MIS       : MIS_OVR_MIS_Field;
      --  Synchronization error masked interrupt status
      ERR_MIS       : MIS_ERR_MIS_Field;
      --  VSYNC masked interrupt status
      VSYNC_MIS     : MIS_VSYNC_MIS_Field;
      --  Line masked interrupt status
      LINE_MIS      : MIS_LINE_MIS_Field;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MIS_Register use record
      FRAME_MIS     at 0 range 0 .. 0;
      OVR_MIS       at 0 range 1 .. 1;
      ERR_MIS       at 0 range 2 .. 2;
      VSYNC_MIS     at 0 range 3 .. 3;
      LINE_MIS      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   ------------------
   -- ICR_Register --
   ------------------

   subtype ICR_FRAME_ISC_Field is STM32_SVD.Bit;
   subtype ICR_OVR_ISC_Field is STM32_SVD.Bit;
   subtype ICR_ERR_ISC_Field is STM32_SVD.Bit;
   subtype ICR_VSYNC_ISC_Field is STM32_SVD.Bit;
   subtype ICR_LINE_ISC_Field is STM32_SVD.Bit;

   --  interrupt clear register
   type ICR_Register is record
      --  Capture complete interrupt status clear
      FRAME_ISC     : ICR_FRAME_ISC_Field := 16#0#;
      --  Overrun interrupt status clear
      OVR_ISC       : ICR_OVR_ISC_Field := 16#0#;
      --  Synchronization error interrupt status clear
      ERR_ISC       : ICR_ERR_ISC_Field := 16#0#;
      --  Vertical synch interrupt status clear
      VSYNC_ISC     : ICR_VSYNC_ISC_Field := 16#0#;
      --  line interrupt status clear
      LINE_ISC      : ICR_LINE_ISC_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      FRAME_ISC     at 0 range 0 .. 0;
      OVR_ISC       at 0 range 1 .. 1;
      ERR_ISC       at 0 range 2 .. 2;
      VSYNC_ISC     at 0 range 3 .. 3;
      LINE_ISC      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -------------------
   -- ESCR_Register --
   -------------------

   subtype ESCR_FSC_Field is STM32_SVD.Byte;
   subtype ESCR_LSC_Field is STM32_SVD.Byte;
   subtype ESCR_LEC_Field is STM32_SVD.Byte;
   subtype ESCR_FEC_Field is STM32_SVD.Byte;

   --  embedded synchronization code register
   type ESCR_Register is record
      --  Frame start delimiter code
      FSC : ESCR_FSC_Field := 16#0#;
      --  Line start delimiter code
      LSC : ESCR_LSC_Field := 16#0#;
      --  Line end delimiter code
      LEC : ESCR_LEC_Field := 16#0#;
      --  Frame end delimiter code
      FEC : ESCR_FEC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESCR_Register use record
      FSC at 0 range 0 .. 7;
      LSC at 0 range 8 .. 15;
      LEC at 0 range 16 .. 23;
      FEC at 0 range 24 .. 31;
   end record;

   -------------------
   -- ESUR_Register --
   -------------------

   subtype ESUR_FSU_Field is STM32_SVD.Byte;
   subtype ESUR_LSU_Field is STM32_SVD.Byte;
   subtype ESUR_LEU_Field is STM32_SVD.Byte;
   subtype ESUR_FEU_Field is STM32_SVD.Byte;

   --  embedded synchronization unmask register
   type ESUR_Register is record
      --  Frame start delimiter unmask
      FSU : ESUR_FSU_Field := 16#0#;
      --  Line start delimiter unmask
      LSU : ESUR_LSU_Field := 16#0#;
      --  Line end delimiter unmask
      LEU : ESUR_LEU_Field := 16#0#;
      --  Frame end delimiter unmask
      FEU : ESUR_FEU_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESUR_Register use record
      FSU at 0 range 0 .. 7;
      LSU at 0 range 8 .. 15;
      LEU at 0 range 16 .. 23;
      FEU at 0 range 24 .. 31;
   end record;

   ---------------------
   -- CWSTRT_Register --
   ---------------------

   subtype CWSTRT_HOFFCNT_Field is STM32_SVD.UInt14;
   subtype CWSTRT_VST_Field is STM32_SVD.UInt13;

   --  crop window start
   type CWSTRT_Register is record
      --  Horizontal offset count
      HOFFCNT        : CWSTRT_HOFFCNT_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : STM32_SVD.UInt2 := 16#0#;
      --  Vertical start line count
      VST            : CWSTRT_VST_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32_SVD.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CWSTRT_Register use record
      HOFFCNT        at 0 range 0 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      VST            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   ---------------------
   -- CWSIZE_Register --
   ---------------------

   subtype CWSIZE_CAPCNT_Field is STM32_SVD.UInt14;
   subtype CWSIZE_VLINE_Field is STM32_SVD.UInt14;

   --  crop window size
   type CWSIZE_Register is record
      --  Capture count
      CAPCNT         : CWSIZE_CAPCNT_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : STM32_SVD.UInt2 := 16#0#;
      --  Vertical line count
      VLINE          : CWSIZE_VLINE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : STM32_SVD.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CWSIZE_Register use record
      CAPCNT         at 0 range 0 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      VLINE          at 0 range 16 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------
   -- DR_Register --
   -----------------

   --  DR_Byte array element
   subtype DR_Byte_Element is STM32_SVD.Byte;

   --  DR_Byte array
   type DR_Byte_Field_Array is array (0 .. 3) of DR_Byte_Element
     with Component_Size => 8, Size => 32;

   --  data register
   type DR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  Byte as a value
            Val : STM32_SVD.Word;
         when True =>
            --  Byte as an array
            Arr : DR_Byte_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital camera interface
   type DCMI_Peripheral is record
      --  control register 1
      CR     : CR_Register;
      --  status register
      SR     : SR_Register;
      --  raw interrupt status register
      RIS    : RIS_Register;
      --  interrupt enable register
      IER    : IER_Register;
      --  masked interrupt status register
      MIS    : MIS_Register;
      --  interrupt clear register
      ICR    : ICR_Register;
      --  embedded synchronization code register
      ESCR   : ESCR_Register;
      --  embedded synchronization unmask register
      ESUR   : ESUR_Register;
      --  crop window start
      CWSTRT : CWSTRT_Register;
      --  crop window size
      CWSIZE : CWSIZE_Register;
      --  data register
      DR     : DR_Register;
   end record
     with Volatile;

   for DCMI_Peripheral use record
      CR     at 0 range 0 .. 31;
      SR     at 4 range 0 .. 31;
      RIS    at 8 range 0 .. 31;
      IER    at 12 range 0 .. 31;
      MIS    at 16 range 0 .. 31;
      ICR    at 20 range 0 .. 31;
      ESCR   at 24 range 0 .. 31;
      ESUR   at 28 range 0 .. 31;
      CWSTRT at 32 range 0 .. 31;
      CWSIZE at 36 range 0 .. 31;
      DR     at 40 range 0 .. 31;
   end record;

   --  Digital camera interface
   DCMI_Periph : aliased DCMI_Peripheral
     with Import, Address => System'To_Address (16#50050000#);

end STM32_SVD.DCMI;
