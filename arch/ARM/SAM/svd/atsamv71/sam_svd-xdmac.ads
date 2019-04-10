--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.XDMAC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype XDMAC_XDMAC_GTYPE_NB_CH_Field is HAL.UInt5;
   subtype XDMAC_XDMAC_GTYPE_FIFO_SZ_Field is HAL.UInt11;
   subtype XDMAC_XDMAC_GTYPE_NB_REQ_Field is HAL.UInt7;

   --  Global Type Register
   type XDMAC_XDMAC_GTYPE_Register is record
      --  Read-only. Number of Channels Minus One
      NB_CH          : XDMAC_XDMAC_GTYPE_NB_CH_Field;
      --  Read-only. Number of Bytes
      FIFO_SZ        : XDMAC_XDMAC_GTYPE_FIFO_SZ_Field;
      --  Read-only. Number of Peripheral Requests Minus One
      NB_REQ         : XDMAC_XDMAC_GTYPE_NB_REQ_Field;
      --  unspecified
      Reserved_23_31 : HAL.UInt9;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GTYPE_Register use record
      NB_CH          at 0 range 0 .. 4;
      FIFO_SZ        at 0 range 5 .. 15;
      NB_REQ         at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Global Configuration Register
   type XDMAC_XDMAC_GCFG_Register is record
      --  Configuration Registers Clock Gating Disable
      CGDISREG      : Boolean := False;
      --  Pipeline Clock Gating Disable
      CGDISPIPE     : Boolean := False;
      --  FIFO Clock Gating Disable
      CGDISFIFO     : Boolean := False;
      --  Bus Interface Clock Gating Disable
      CGDISIF       : Boolean := False;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  Boundary X Kilobyte Enable
      BXKBEN        : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GCFG_Register use record
      CGDISREG      at 0 range 0 .. 0;
      CGDISPIPE     at 0 range 1 .. 1;
      CGDISFIFO     at 0 range 2 .. 2;
      CGDISIF       at 0 range 3 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      BXKBEN        at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  XDMAC_XDMAC_GWAC_PW array element
   subtype XDMAC_XDMAC_GWAC_PW_Element is HAL.UInt4;

   --  XDMAC_XDMAC_GWAC_PW array
   type XDMAC_XDMAC_GWAC_PW_Field_Array is array (0 .. 3)
     of XDMAC_XDMAC_GWAC_PW_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for XDMAC_XDMAC_GWAC_PW
   type XDMAC_XDMAC_GWAC_PW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PW as a value
            Val : HAL.UInt16;
         when True =>
            --  PW as an array
            Arr : XDMAC_XDMAC_GWAC_PW_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for XDMAC_XDMAC_GWAC_PW_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Global Weighted Arbiter Configuration Register
   type XDMAC_XDMAC_GWAC_Register is record
      --  Pool Weight 0
      PW             : XDMAC_XDMAC_GWAC_PW_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GWAC_Register use record
      PW             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  XDMAC_XDMAC_GIE_IE array
   type XDMAC_XDMAC_GIE_IE_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GIE_IE
   type XDMAC_XDMAC_GIE_IE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IE as a value
            Val : HAL.UInt24;
         when True =>
            --  IE as an array
            Arr : XDMAC_XDMAC_GIE_IE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GIE_IE_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Interrupt Enable Register
   type XDMAC_XDMAC_GIE_Register is record
      --  Write-only. XDMAC Channel 0 Interrupt Enable Bit
      IE             : XDMAC_XDMAC_GIE_IE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GIE_Register use record
      IE             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GID_ID array
   type XDMAC_XDMAC_GID_ID_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GID_ID
   type XDMAC_XDMAC_GID_ID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ID as a value
            Val : HAL.UInt24;
         when True =>
            --  ID as an array
            Arr : XDMAC_XDMAC_GID_ID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GID_ID_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Interrupt Disable Register
   type XDMAC_XDMAC_GID_Register is record
      --  Write-only. XDMAC Channel 0 Interrupt Disable Bit
      ID             : XDMAC_XDMAC_GID_ID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GID_Register use record
      ID             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GIM_IM array
   type XDMAC_XDMAC_GIM_IM_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GIM_IM
   type XDMAC_XDMAC_GIM_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt24;
         when True =>
            --  IM as an array
            Arr : XDMAC_XDMAC_GIM_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GIM_IM_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Interrupt Mask Register
   type XDMAC_XDMAC_GIM_Register is record
      --  Read-only. XDMAC Channel 0 Interrupt Mask Bit
      IM             : XDMAC_XDMAC_GIM_IM_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GIM_Register use record
      IM             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GIS_IS array
   type XDMAC_XDMAC_GIS_IS_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GIS_IS
   type XDMAC_XDMAC_GIS_IS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IS as a value
            Val : HAL.UInt24;
         when True =>
            --  IS as an array
            Arr : XDMAC_XDMAC_GIS_IS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GIS_IS_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Interrupt Status Register
   type XDMAC_XDMAC_GIS_Register is record
      --  Read-only. XDMAC Channel 0 Interrupt Status Bit
      IS_k           : XDMAC_XDMAC_GIS_IS_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GIS_Register use record
      IS_k           at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GE_EN array
   type XDMAC_XDMAC_GE_EN_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GE_EN
   type XDMAC_XDMAC_GE_EN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EN as a value
            Val : HAL.UInt24;
         when True =>
            --  EN as an array
            Arr : XDMAC_XDMAC_GE_EN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GE_EN_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Enable Register
   type XDMAC_XDMAC_GE_Register is record
      --  Write-only. XDMAC Channel 0 Enable Bit
      EN             : XDMAC_XDMAC_GE_EN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GE_Register use record
      EN             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GD_DI array
   type XDMAC_XDMAC_GD_DI_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GD_DI
   type XDMAC_XDMAC_GD_DI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DI as a value
            Val : HAL.UInt24;
         when True =>
            --  DI as an array
            Arr : XDMAC_XDMAC_GD_DI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GD_DI_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Disable Register
   type XDMAC_XDMAC_GD_Register is record
      --  Write-only. XDMAC Channel 0 Disable Bit
      DI             : XDMAC_XDMAC_GD_DI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GD_Register use record
      DI             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GS_ST array
   type XDMAC_XDMAC_GS_ST_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GS_ST
   type XDMAC_XDMAC_GS_ST_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ST as a value
            Val : HAL.UInt24;
         when True =>
            --  ST as an array
            Arr : XDMAC_XDMAC_GS_ST_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GS_ST_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Status Register
   type XDMAC_XDMAC_GS_Register is record
      --  Read-only. XDMAC Channel 0 Status Bit
      ST             : XDMAC_XDMAC_GS_ST_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GS_Register use record
      ST             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GRS_RS array
   type XDMAC_XDMAC_GRS_RS_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GRS_RS
   type XDMAC_XDMAC_GRS_RS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RS as a value
            Val : HAL.UInt24;
         when True =>
            --  RS as an array
            Arr : XDMAC_XDMAC_GRS_RS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GRS_RS_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Read Suspend Register
   type XDMAC_XDMAC_GRS_Register is record
      --  XDMAC Channel 0 Read Suspend Bit
      RS             : XDMAC_XDMAC_GRS_RS_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GRS_Register use record
      RS             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GWS_WS array
   type XDMAC_XDMAC_GWS_WS_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GWS_WS
   type XDMAC_XDMAC_GWS_WS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WS as a value
            Val : HAL.UInt24;
         when True =>
            --  WS as an array
            Arr : XDMAC_XDMAC_GWS_WS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GWS_WS_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Write Suspend Register
   type XDMAC_XDMAC_GWS_Register is record
      --  XDMAC Channel 0 Write Suspend Bit
      WS             : XDMAC_XDMAC_GWS_WS_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GWS_Register use record
      WS             at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GRWS_RWS array
   type XDMAC_XDMAC_GRWS_RWS_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GRWS_RWS
   type XDMAC_XDMAC_GRWS_RWS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RWS as a value
            Val : HAL.UInt24;
         when True =>
            --  RWS as an array
            Arr : XDMAC_XDMAC_GRWS_RWS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GRWS_RWS_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Read Write Suspend Register
   type XDMAC_XDMAC_GRWS_Register is record
      --  Write-only. XDMAC Channel 0 Read Write Suspend Bit
      RWS            : XDMAC_XDMAC_GRWS_RWS_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GRWS_Register use record
      RWS            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GRWR_RWR array
   type XDMAC_XDMAC_GRWR_RWR_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GRWR_RWR
   type XDMAC_XDMAC_GRWR_RWR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RWR as a value
            Val : HAL.UInt24;
         when True =>
            --  RWR as an array
            Arr : XDMAC_XDMAC_GRWR_RWR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GRWR_RWR_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Read Write Resume Register
   type XDMAC_XDMAC_GRWR_Register is record
      --  Write-only. XDMAC Channel 0 Read Write Resume Bit
      RWR            : XDMAC_XDMAC_GRWR_RWR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GRWR_Register use record
      RWR            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GSWR_SWREQ array
   type XDMAC_XDMAC_GSWR_SWREQ_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GSWR_SWREQ
   type XDMAC_XDMAC_GSWR_SWREQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWREQ as a value
            Val : HAL.UInt24;
         when True =>
            --  SWREQ as an array
            Arr : XDMAC_XDMAC_GSWR_SWREQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GSWR_SWREQ_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Software Request Register
   type XDMAC_XDMAC_GSWR_Register is record
      --  Write-only. XDMAC Channel 0 Software Request Bit
      SWREQ          : XDMAC_XDMAC_GSWR_SWREQ_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GSWR_Register use record
      SWREQ          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GSWS_SWRS array
   type XDMAC_XDMAC_GSWS_SWRS_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GSWS_SWRS
   type XDMAC_XDMAC_GSWS_SWRS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWRS as a value
            Val : HAL.UInt24;
         when True =>
            --  SWRS as an array
            Arr : XDMAC_XDMAC_GSWS_SWRS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GSWS_SWRS_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Software Request Status Register
   type XDMAC_XDMAC_GSWS_Register is record
      --  Read-only. XDMAC Channel 0 Software Request Status Bit
      SWRS           : XDMAC_XDMAC_GSWS_SWRS_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GSWS_Register use record
      SWRS           at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  XDMAC_XDMAC_GSWF_SWF array
   type XDMAC_XDMAC_GSWF_SWF_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for XDMAC_XDMAC_GSWF_SWF
   type XDMAC_XDMAC_GSWF_SWF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWF as a value
            Val : HAL.UInt24;
         when True =>
            --  SWF as an array
            Arr : XDMAC_XDMAC_GSWF_SWF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for XDMAC_XDMAC_GSWF_SWF_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Global Channel Software Flush Request Register
   type XDMAC_XDMAC_GSWF_Register is record
      --  Write-only. XDMAC Channel 0 Software Flush Request Bit
      SWF            : XDMAC_XDMAC_GSWF_SWF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_GSWF_Register use record
      SWF            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ------------------------------------------
   -- XDMAC_XDMAC_CHID cluster's Registers --
   ------------------------------------------

   --  Channel Interrupt Enable Register (chid = 0)
   type XDMAC_XDMAC_CIE_XDMAC_XDMAC_CHID_Register is record
      --  Write-only. End of Block Interrupt Enable Bit
      BIE           : Boolean := False;
      --  Write-only. End of Linked List Interrupt Enable Bit
      LIE           : Boolean := False;
      --  Write-only. End of Disable Interrupt Enable Bit
      DIE           : Boolean := False;
      --  Write-only. End of Flush Interrupt Enable Bit
      FIE           : Boolean := False;
      --  Write-only. Read Bus Error Interrupt Enable Bit
      RBIE          : Boolean := False;
      --  Write-only. Write Bus Error Interrupt Enable Bit
      WBIE          : Boolean := False;
      --  Write-only. Request Overflow Error Interrupt Enable Bit
      ROIE          : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CIE_XDMAC_XDMAC_CHID_Register use record
      BIE           at 0 range 0 .. 0;
      LIE           at 0 range 1 .. 1;
      DIE           at 0 range 2 .. 2;
      FIE           at 0 range 3 .. 3;
      RBIE          at 0 range 4 .. 4;
      WBIE          at 0 range 5 .. 5;
      ROIE          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Channel Interrupt Disable Register (chid = 0)
   type XDMAC_XDMAC_CID_XDMAC_XDMAC_CHID_Register is record
      --  Write-only. End of Block Interrupt Disable Bit
      BID           : Boolean := False;
      --  Write-only. End of Linked List Interrupt Disable Bit
      LID           : Boolean := False;
      --  Write-only. End of Disable Interrupt Disable Bit
      DID           : Boolean := False;
      --  Write-only. End of Flush Interrupt Disable Bit
      FID           : Boolean := False;
      --  Write-only. Read Bus Error Interrupt Disable Bit
      RBEID         : Boolean := False;
      --  Write-only. Write Bus Error Interrupt Disable Bit
      WBEID         : Boolean := False;
      --  Write-only. Request Overflow Error Interrupt Disable Bit
      ROID          : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CID_XDMAC_XDMAC_CHID_Register use record
      BID           at 0 range 0 .. 0;
      LID           at 0 range 1 .. 1;
      DID           at 0 range 2 .. 2;
      FID           at 0 range 3 .. 3;
      RBEID         at 0 range 4 .. 4;
      WBEID         at 0 range 5 .. 5;
      ROID          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Channel Interrupt Mask Register (chid = 0)
   type XDMAC_XDMAC_CIM_XDMAC_XDMAC_CHID_Register is record
      --  Read-only. End of Block Interrupt Mask Bit
      BIM           : Boolean;
      --  Read-only. End of Linked List Interrupt Mask Bit
      LIM           : Boolean;
      --  Read-only. End of Disable Interrupt Mask Bit
      DIM           : Boolean;
      --  Read-only. End of Flush Interrupt Mask Bit
      FIM           : Boolean;
      --  Read-only. Read Bus Error Interrupt Mask Bit
      RBEIM         : Boolean;
      --  Read-only. Write Bus Error Interrupt Mask Bit
      WBEIM         : Boolean;
      --  Read-only. Request Overflow Error Interrupt Mask Bit
      ROIM          : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CIM_XDMAC_XDMAC_CHID_Register use record
      BIM           at 0 range 0 .. 0;
      LIM           at 0 range 1 .. 1;
      DIM           at 0 range 2 .. 2;
      FIM           at 0 range 3 .. 3;
      RBEIM         at 0 range 4 .. 4;
      WBEIM         at 0 range 5 .. 5;
      ROIM          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Channel Interrupt Status Register (chid = 0)
   type XDMAC_XDMAC_CIS_XDMAC_XDMAC_CHID_Register is record
      --  Read-only. End of Block Interrupt Status Bit
      BIS           : Boolean;
      --  Read-only. End of Linked List Interrupt Status Bit
      LIS           : Boolean;
      --  Read-only. End of Disable Interrupt Status Bit
      DIS           : Boolean;
      --  Read-only. End of Flush Interrupt Status Bit
      FIS           : Boolean;
      --  Read-only. Read Bus Error Interrupt Status Bit
      RBEIS         : Boolean;
      --  Read-only. Write Bus Error Interrupt Status Bit
      WBEIS         : Boolean;
      --  Read-only. Request Overflow Error Interrupt Status Bit
      ROIS          : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CIS_XDMAC_XDMAC_CHID_Register use record
      BIS           at 0 range 0 .. 0;
      LIS           at 0 range 1 .. 1;
      DIS           at 0 range 2 .. 2;
      FIS           at 0 range 3 .. 3;
      RBEIS         at 0 range 4 .. 4;
      WBEIS         at 0 range 5 .. 5;
      ROIS          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype XDMAC_XDMAC_CNDA_XDMAC_XDMAC_CHID_NDA_Field is HAL.UInt30;

   --  Channel Next Descriptor Address Register (chid = 0)
   type XDMAC_XDMAC_CNDA_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Next Descriptor Interface
      NDAIF        : Boolean := False;
      --  unspecified
      Reserved_1_1 : HAL.Bit := 16#0#;
      --  Channel x Next Descriptor Address
      NDA          : XDMAC_XDMAC_CNDA_XDMAC_XDMAC_CHID_NDA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CNDA_XDMAC_XDMAC_CHID_Register use record
      NDAIF        at 0 range 0 .. 0;
      Reserved_1_1 at 0 range 1 .. 1;
      NDA          at 0 range 2 .. 31;
   end record;

   --  Channel x Next Descriptor Enable
   type XDMAC_CNDC_NDE_Field is
     (
      --  Descriptor fetch is disabled.
      Dscr_Fetch_Dis,
      --  Descriptor fetch is enabled.
      Dscr_Fetch_En)
     with Size => 1;
   for XDMAC_CNDC_NDE_Field use
     (Dscr_Fetch_Dis => 0,
      Dscr_Fetch_En => 1);

   --  Channel x Next Descriptor Source Update
   type XDMAC_CNDC_NDSUP_Field is
     (
      --  Source parameters remain unchanged.
      Src_Params_Unchanged,
      --  Source parameters are updated when the descriptor is retrieved.
      Src_Params_Updated)
     with Size => 1;
   for XDMAC_CNDC_NDSUP_Field use
     (Src_Params_Unchanged => 0,
      Src_Params_Updated => 1);

   --  Channel x Next Descriptor Destination Update
   type XDMAC_CNDC_NDDUP_Field is
     (
      --  Destination parameters remain unchanged.
      Dst_Params_Unchanged,
      --  Destination parameters are updated when the descriptor is retrieved.
      Dst_Params_Updated)
     with Size => 1;
   for XDMAC_CNDC_NDDUP_Field use
     (Dst_Params_Unchanged => 0,
      Dst_Params_Updated => 1);

   --  Channel x Next Descriptor View
   type XDMAC_CNDC_NDVIEW_Field is
     (
      --  Next Descriptor View 0
      Ndv0,
      --  Next Descriptor View 1
      Ndv1,
      --  Next Descriptor View 2
      Ndv2,
      --  Next Descriptor View 3
      Ndv3)
     with Size => 2;
   for XDMAC_CNDC_NDVIEW_Field use
     (Ndv0 => 0,
      Ndv1 => 1,
      Ndv2 => 2,
      Ndv3 => 3);

   --  Channel Next Descriptor Control Register (chid = 0)
   type XDMAC_XDMAC_CNDC_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Next Descriptor Enable
      NDE           : XDMAC_CNDC_NDE_Field := SAM_SVD.XDMAC.Dscr_Fetch_Dis;
      --  Channel x Next Descriptor Source Update
      NDSUP         : XDMAC_CNDC_NDSUP_Field :=
                       SAM_SVD.XDMAC.Src_Params_Unchanged;
      --  Channel x Next Descriptor Destination Update
      NDDUP         : XDMAC_CNDC_NDDUP_Field :=
                       SAM_SVD.XDMAC.Dst_Params_Unchanged;
      --  Channel x Next Descriptor View
      NDVIEW        : XDMAC_CNDC_NDVIEW_Field := SAM_SVD.XDMAC.Ndv0;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CNDC_XDMAC_XDMAC_CHID_Register use record
      NDE           at 0 range 0 .. 0;
      NDSUP         at 0 range 1 .. 1;
      NDDUP         at 0 range 2 .. 2;
      NDVIEW        at 0 range 3 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype XDMAC_XDMAC_CUBC_XDMAC_XDMAC_CHID_UBLEN_Field is HAL.UInt24;

   --  Channel Microblock Control Register (chid = 0)
   type XDMAC_XDMAC_CUBC_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Microblock Length
      UBLEN          : XDMAC_XDMAC_CUBC_XDMAC_XDMAC_CHID_UBLEN_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CUBC_XDMAC_XDMAC_CHID_Register use record
      UBLEN          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype XDMAC_XDMAC_CBC_XDMAC_XDMAC_CHID_BLEN_Field is HAL.UInt12;

   --  Channel Block Control Register (chid = 0)
   type XDMAC_XDMAC_CBC_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Block Length
      BLEN           : XDMAC_XDMAC_CBC_XDMAC_XDMAC_CHID_BLEN_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CBC_XDMAC_XDMAC_CHID_Register use record
      BLEN           at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Channel x Transfer Type
   type XDMAC_CC_TYPE_Field is
     (
      --  Self-triggered mode (memory-to-memory transfer).
      Mem_Tran,
      --  Synchronized mode (peripheral-to-memory or memory-to-peripheral
      --  transfer).
      Per_Tran)
     with Size => 1;
   for XDMAC_CC_TYPE_Field use
     (Mem_Tran => 0,
      Per_Tran => 1);

   --  Channel x Memory Burst Size
   type XDMAC_CC_MBSIZE_Field is
     (
      --  The memory burst size is set to one.
      Single,
      --  The memory burst size is set to four.
      Four,
      --  The memory burst size is set to eight.
      Eight,
      --  The memory burst size is set to sixteen.
      Sixteen)
     with Size => 2;
   for XDMAC_CC_MBSIZE_Field use
     (Single => 0,
      Four => 1,
      Eight => 2,
      Sixteen => 3);

   --  Channel x Synchronization
   type XDMAC_CC_DSYNC_Field is
     (
      --  Peripheral-to-memory transfer.
      Per2Mem,
      --  Memory-to-peripheral transfer.
      Mem2Per)
     with Size => 1;
   for XDMAC_CC_DSYNC_Field use
     (Per2Mem => 0,
      Mem2Per => 1);

   --  Channel x Software Request Trigger
   type XDMAC_CC_SWREQ_Field is
     (
      --  Hardware request line is connected to the peripheral request line.
      Hwr_Connected,
      --  Software request is connected to the peripheral request line.
      Swr_Connected)
     with Size => 1;
   for XDMAC_CC_SWREQ_Field use
     (Hwr_Connected => 0,
      Swr_Connected => 1);

   --  Channel x Fill Block of Memory
   type XDMAC_CC_MEMSET_Field is
     (
      --  Memset is not activated.
      Normal_Mode,
      --  Sets the block of memory pointed by DA field to the specified value.
      --  This operation is performed on 8-, 16- or 32-bit basis.
      Hw_Mode)
     with Size => 1;
   for XDMAC_CC_MEMSET_Field use
     (Normal_Mode => 0,
      Hw_Mode => 1);

   --  Channel x Chunk Size
   type XDMAC_CC_CSIZE_Field is
     (
      --  1 data transferred
      Chk_1,
      --  2 data transferred
      Chk_2,
      --  4 data transferred
      Chk_4,
      --  8 data transferred
      Chk_8,
      --  16 data transferred
      Chk_16)
     with Size => 3;
   for XDMAC_CC_CSIZE_Field use
     (Chk_1 => 0,
      Chk_2 => 1,
      Chk_4 => 2,
      Chk_8 => 3,
      Chk_16 => 4);

   --  Channel x Data Width
   type XDMAC_CC_DWIDTH_Field is
     (
      --  The data size is set to 8 bits
      Byte,
      --  The data size is set to 16 bits
      Halfword,
      --  The data size is set to 32 bits
      Word)
     with Size => 2;
   for XDMAC_CC_DWIDTH_Field use
     (Byte => 0,
      Halfword => 1,
      Word => 2);

   --  Channel x Source Interface Identifier
   type XDMAC_CC_SIF_Field is
     (
      --  The data is read through the system bus interface 0.
      Ahb_If0,
      --  The data is read through the system bus interface 1.
      Ahb_If1)
     with Size => 1;
   for XDMAC_CC_SIF_Field use
     (Ahb_If0 => 0,
      Ahb_If1 => 1);

   --  Channel x Destination Interface Identifier
   type XDMAC_CC_DIF_Field is
     (
      --  The data is written through the system bus interface 0.
      Ahb_If0,
      --  The data is written though the system bus interface 1.
      Ahb_If1)
     with Size => 1;
   for XDMAC_CC_DIF_Field use
     (Ahb_If0 => 0,
      Ahb_If1 => 1);

   --  Channel x Source Addressing Mode
   type XDMAC_CC_SAM_Field is
     (
      --  The address remains unchanged.
      Fixed_Am,
      --  The addressing mode is incremented (the increment size is set to the
      --  data size).
      Incremented_Am,
      --  The microblock stride is added at the microblock boundary.
      Ubs_Am,
      --  The microblock stride is added at the microblock boundary, the data
      --  stride is added at the data boundary.
      Ubs_Ds_Am)
     with Size => 2;
   for XDMAC_CC_SAM_Field use
     (Fixed_Am => 0,
      Incremented_Am => 1,
      Ubs_Am => 2,
      Ubs_Ds_Am => 3);

   --  Channel x Destination Addressing Mode
   type XDMAC_CC_DAM_Field is
     (
      --  The address remains unchanged.
      Fixed_Am,
      --  The addressing mode is incremented (the increment size is set to the
      --  data size).
      Incremented_Am,
      --  The microblock stride is added at the microblock boundary.
      Ubs_Am,
      --  The microblock stride is added at the microblock boundary; the data
      --  stride is added at the data boundary.
      Ubs_Ds_Am)
     with Size => 2;
   for XDMAC_CC_DAM_Field use
     (Fixed_Am => 0,
      Incremented_Am => 1,
      Ubs_Am => 2,
      Ubs_Ds_Am => 3);

   --  Channel Initialization Done (this bit is read-only)
   type XDMAC_CC_INITD_Field is
     (
      --  Channel initialization is in progress.
      In_Progress,
      --  Channel initialization is completed.
      Terminated)
     with Size => 1;
   for XDMAC_CC_INITD_Field use
     (In_Progress => 0,
      Terminated => 1);

   --  Read in Progress (this bit is read-only)
   type XDMAC_CC_RDIP_Field is
     (
      --  No active read transaction on the bus.
      Done,
      --  A read transaction is in progress.
      In_Progress)
     with Size => 1;
   for XDMAC_CC_RDIP_Field use
     (Done => 0,
      In_Progress => 1);

   --  Write in Progress (this bit is read-only)
   type XDMAC_CC_WRIP_Field is
     (
      --  No active write transaction on the bus.
      Done,
      --  A write transaction is in progress.
      In_Progress)
     with Size => 1;
   for XDMAC_CC_WRIP_Field use
     (Done => 0,
      In_Progress => 1);

   subtype XDMAC_XDMAC_CC_XDMAC_XDMAC_CHID_PERID_Field is HAL.UInt7;

   --  Channel Configuration Register (chid = 0)
   type XDMAC_XDMAC_CC_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Transfer Type
      TYPE_k         : XDMAC_CC_TYPE_Field := SAM_SVD.XDMAC.Mem_Tran;
      --  Channel x Memory Burst Size
      MBSIZE         : XDMAC_CC_MBSIZE_Field := SAM_SVD.XDMAC.Single;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Channel x Synchronization
      DSYNC          : XDMAC_CC_DSYNC_Field := SAM_SVD.XDMAC.Per2Mem;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Channel x Software Request Trigger
      SWREQ          : XDMAC_CC_SWREQ_Field := SAM_SVD.XDMAC.Hwr_Connected;
      --  Channel x Fill Block of Memory
      MEMSET         : XDMAC_CC_MEMSET_Field := SAM_SVD.XDMAC.Normal_Mode;
      --  Channel x Chunk Size
      CSIZE          : XDMAC_CC_CSIZE_Field := SAM_SVD.XDMAC.Chk_1;
      --  Channel x Data Width
      DWIDTH         : XDMAC_CC_DWIDTH_Field := SAM_SVD.XDMAC.Byte;
      --  Channel x Source Interface Identifier
      SIF            : XDMAC_CC_SIF_Field := SAM_SVD.XDMAC.Ahb_If0;
      --  Channel x Destination Interface Identifier
      DIF            : XDMAC_CC_DIF_Field := SAM_SVD.XDMAC.Ahb_If0;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Channel x Source Addressing Mode
      SAM            : XDMAC_CC_SAM_Field := SAM_SVD.XDMAC.Fixed_Am;
      --  Channel x Destination Addressing Mode
      DAM            : XDMAC_CC_DAM_Field := SAM_SVD.XDMAC.Fixed_Am;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Channel Initialization Done (this bit is read-only)
      INITD          : XDMAC_CC_INITD_Field := SAM_SVD.XDMAC.In_Progress;
      --  Read in Progress (this bit is read-only)
      RDIP           : XDMAC_CC_RDIP_Field := SAM_SVD.XDMAC.Done;
      --  Write in Progress (this bit is read-only)
      WRIP           : XDMAC_CC_WRIP_Field := SAM_SVD.XDMAC.Done;
      --  Channel x Peripheral Hardware Request Line Identifier
      PERID          : XDMAC_XDMAC_CC_XDMAC_XDMAC_CHID_PERID_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CC_XDMAC_XDMAC_CHID_Register use record
      TYPE_k         at 0 range 0 .. 0;
      MBSIZE         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DSYNC          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SWREQ          at 0 range 6 .. 6;
      MEMSET         at 0 range 7 .. 7;
      CSIZE          at 0 range 8 .. 10;
      DWIDTH         at 0 range 11 .. 12;
      SIF            at 0 range 13 .. 13;
      DIF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SAM            at 0 range 16 .. 17;
      DAM            at 0 range 18 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      INITD          at 0 range 21 .. 21;
      RDIP           at 0 range 22 .. 22;
      WRIP           at 0 range 23 .. 23;
      PERID          at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_SDS_MSP_Field is HAL.UInt16;
   subtype XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_DDS_MSP_Field is HAL.UInt16;

   --  Channel Data Stride Memory Set Pattern (chid = 0)
   type XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Source Data stride or Memory Set Pattern
      SDS_MSP : XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_SDS_MSP_Field := 16#0#;
      --  Channel x Destination Data Stride or Memory Set Pattern
      DDS_MSP : XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_DDS_MSP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_Register use record
      SDS_MSP at 0 range 0 .. 15;
      DDS_MSP at 0 range 16 .. 31;
   end record;

   subtype XDMAC_XDMAC_CSUS_XDMAC_XDMAC_CHID_SUBS_Field is HAL.UInt24;

   --  Channel Source Microblock Stride (chid = 0)
   type XDMAC_XDMAC_CSUS_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Source Microblock Stride
      SUBS           : XDMAC_XDMAC_CSUS_XDMAC_XDMAC_CHID_SUBS_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CSUS_XDMAC_XDMAC_CHID_Register use record
      SUBS           at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype XDMAC_XDMAC_CDUS_XDMAC_XDMAC_CHID_DUBS_Field is HAL.UInt24;

   --  Channel Destination Microblock Stride (chid = 0)
   type XDMAC_XDMAC_CDUS_XDMAC_XDMAC_CHID_Register is record
      --  Channel x Destination Microblock Stride
      DUBS           : XDMAC_XDMAC_CDUS_XDMAC_XDMAC_CHID_DUBS_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_CDUS_XDMAC_XDMAC_CHID_Register use record
      DUBS           at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   type XDMAC_RESERVED_Region is array (1 .. 2) of HAL.UInt32;

   --  Channel Interrupt Enable Register (chid = 0)
   type XDMAC_XDMAC_CHID_Cluster is record
      --  Channel Interrupt Enable Register (chid = 0)
      XDMAC_CIE     : aliased XDMAC_XDMAC_CIE_XDMAC_XDMAC_CHID_Register;
      --  Channel Interrupt Disable Register (chid = 0)
      XDMAC_CID     : aliased XDMAC_XDMAC_CID_XDMAC_XDMAC_CHID_Register;
      --  Channel Interrupt Mask Register (chid = 0)
      XDMAC_CIM     : aliased XDMAC_XDMAC_CIM_XDMAC_XDMAC_CHID_Register;
      --  Channel Interrupt Status Register (chid = 0)
      XDMAC_CIS     : aliased XDMAC_XDMAC_CIS_XDMAC_XDMAC_CHID_Register;
      --  Channel Source Address Register (chid = 0)
      XDMAC_CSA     : aliased HAL.UInt32;
      --  Channel Destination Address Register (chid = 0)
      XDMAC_CDA     : aliased HAL.UInt32;
      --  Channel Next Descriptor Address Register (chid = 0)
      XDMAC_CNDA    : aliased XDMAC_XDMAC_CNDA_XDMAC_XDMAC_CHID_Register;
      --  Channel Next Descriptor Control Register (chid = 0)
      XDMAC_CNDC    : aliased XDMAC_XDMAC_CNDC_XDMAC_XDMAC_CHID_Register;
      --  Channel Microblock Control Register (chid = 0)
      XDMAC_CUBC    : aliased XDMAC_XDMAC_CUBC_XDMAC_XDMAC_CHID_Register;
      --  Channel Block Control Register (chid = 0)
      XDMAC_CBC     : aliased XDMAC_XDMAC_CBC_XDMAC_XDMAC_CHID_Register;
      --  Channel Configuration Register (chid = 0)
      XDMAC_CC      : aliased XDMAC_XDMAC_CC_XDMAC_XDMAC_CHID_Register;
      --  Channel Data Stride Memory Set Pattern (chid = 0)
      XDMAC_CDS_MSP : aliased XDMAC_XDMAC_CDS_MSP_XDMAC_XDMAC_CHID_Register;
      --  Channel Source Microblock Stride (chid = 0)
      XDMAC_CSUS    : aliased XDMAC_XDMAC_CSUS_XDMAC_XDMAC_CHID_Register;
      --  Channel Destination Microblock Stride (chid = 0)
      XDMAC_CDUS    : aliased XDMAC_XDMAC_CDUS_XDMAC_XDMAC_CHID_Register;
      --  Reserved Region
      Reserved_88_8F : aliased XDMAC_RESERVED_Region;
   end record
     with Volatile, Size => 512;

   for XDMAC_XDMAC_CHID_Cluster use record
      XDMAC_CIE     at 16#0# range 0 .. 31;
      XDMAC_CID     at 16#4# range 0 .. 31;
      XDMAC_CIM     at 16#8# range 0 .. 31;
      XDMAC_CIS     at 16#C# range 0 .. 31;
      XDMAC_CSA     at 16#10# range 0 .. 31;
      XDMAC_CDA     at 16#14# range 0 .. 31;
      XDMAC_CNDA    at 16#18# range 0 .. 31;
      XDMAC_CNDC    at 16#1C# range 0 .. 31;
      XDMAC_CUBC    at 16#20# range 0 .. 31;
      XDMAC_CBC     at 16#24# range 0 .. 31;
      XDMAC_CC      at 16#28# range 0 .. 31;
      XDMAC_CDS_MSP at 16#2C# range 0 .. 31;
      XDMAC_CSUS    at 16#30# range 0 .. 31;
      XDMAC_CDUS    at 16#34# range 0 .. 31;
      Reserved_88_8F at 16#38# range 0 .. 63;
   end record;

   --  Channel Interrupt Enable Register (chid = 0)
   type XDMAC_XDMAC_CHID_Clusters is array (0 .. 23)
     of XDMAC_XDMAC_CHID_Cluster;

   subtype XDMAC_XDMAC_VERSION_VERSION_Field is HAL.UInt12;
   subtype XDMAC_XDMAC_VERSION_MFN_Field is HAL.UInt3;

   --  XDMAC Version Register
   type XDMAC_XDMAC_VERSION_Register is record
      --  Version of the Hardware Module
      VERSION        : XDMAC_XDMAC_VERSION_VERSION_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Metal Fix Number
      MFN            : XDMAC_XDMAC_VERSION_MFN_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XDMAC_XDMAC_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Extensible DMA Controller
   type XDMAC_Peripheral is record
      --  Global Type Register
      XDMAC_GTYPE      : aliased XDMAC_XDMAC_GTYPE_Register;
      --  Global Configuration Register
      XDMAC_GCFG       : aliased XDMAC_XDMAC_GCFG_Register;
      --  Global Weighted Arbiter Configuration Register
      XDMAC_GWAC       : aliased XDMAC_XDMAC_GWAC_Register;
      --  Global Interrupt Enable Register
      XDMAC_GIE        : aliased XDMAC_XDMAC_GIE_Register;
      --  Global Interrupt Disable Register
      XDMAC_GID        : aliased XDMAC_XDMAC_GID_Register;
      --  Global Interrupt Mask Register
      XDMAC_GIM        : aliased XDMAC_XDMAC_GIM_Register;
      --  Global Interrupt Status Register
      XDMAC_GIS        : aliased XDMAC_XDMAC_GIS_Register;
      --  Global Channel Enable Register
      XDMAC_GE         : aliased XDMAC_XDMAC_GE_Register;
      --  Global Channel Disable Register
      XDMAC_GD         : aliased XDMAC_XDMAC_GD_Register;
      --  Global Channel Status Register
      XDMAC_GS         : aliased XDMAC_XDMAC_GS_Register;
      --  Global Channel Read Suspend Register
      XDMAC_GRS        : aliased XDMAC_XDMAC_GRS_Register;
      --  Global Channel Write Suspend Register
      XDMAC_GWS        : aliased XDMAC_XDMAC_GWS_Register;
      --  Global Channel Read Write Suspend Register
      XDMAC_GRWS       : aliased XDMAC_XDMAC_GRWS_Register;
      --  Global Channel Read Write Resume Register
      XDMAC_GRWR       : aliased XDMAC_XDMAC_GRWR_Register;
      --  Global Channel Software Request Register
      XDMAC_GSWR       : aliased XDMAC_XDMAC_GSWR_Register;
      --  Global Channel Software Request Status Register
      XDMAC_GSWS       : aliased XDMAC_XDMAC_GSWS_Register;
      --  Global Channel Software Flush Request Register
      XDMAC_GSWF       : aliased XDMAC_XDMAC_GSWF_Register;
      --  Channel Interrupt Enable Register (chid = 0)
      XDMAC_XDMAC_CHID : aliased XDMAC_XDMAC_CHID_Clusters;
      --  XDMAC Version Register
      XDMAC_VERSION    : aliased XDMAC_XDMAC_VERSION_Register;
   end record
     with Volatile;

   for XDMAC_Peripheral use record
      XDMAC_GTYPE      at 16#0# range 0 .. 31;
      XDMAC_GCFG       at 16#4# range 0 .. 31;
      XDMAC_GWAC       at 16#8# range 0 .. 31;
      XDMAC_GIE        at 16#C# range 0 .. 31;
      XDMAC_GID        at 16#10# range 0 .. 31;
      XDMAC_GIM        at 16#14# range 0 .. 31;
      XDMAC_GIS        at 16#18# range 0 .. 31;
      XDMAC_GE         at 16#1C# range 0 .. 31;
      XDMAC_GD         at 16#20# range 0 .. 31;
      XDMAC_GS         at 16#24# range 0 .. 31;
      XDMAC_GRS        at 16#28# range 0 .. 31;
      XDMAC_GWS        at 16#2C# range 0 .. 31;
      XDMAC_GRWS       at 16#30# range 0 .. 31;
      XDMAC_GRWR       at 16#34# range 0 .. 31;
      XDMAC_GSWR       at 16#38# range 0 .. 31;
      XDMAC_GSWS       at 16#3C# range 0 .. 31;
      XDMAC_GSWF       at 16#40# range 0 .. 31;
      XDMAC_XDMAC_CHID at 16#50# range 0 .. 12287;
      XDMAC_VERSION    at 16#FFC# range 0 .. 31;
   end record;

   --  Extensible DMA Controller
   XDMAC_Periph : aliased XDMAC_Peripheral
     with Import, Address => System'To_Address (16#40078000#);

end SAM_SVD.XDMAC;
