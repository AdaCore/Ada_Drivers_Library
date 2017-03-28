--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.CRYP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_ALGOMODE0_Field is HAL.UInt3;
   subtype CR_DATATYPE_Field is HAL.UInt2;
   subtype CR_KEYSIZE_Field is HAL.UInt2;
   subtype CR_GCM_CCMPH_Field is HAL.UInt2;

   --  control register
   type CR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Algorithm direction
      ALGODIR        : Boolean := False;
      --  Algorithm mode
      ALGOMODE0      : CR_ALGOMODE0_Field := 16#0#;
      --  Data type selection
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  Key size selection (AES mode only)
      KEYSIZE        : CR_KEYSIZE_Field := 16#0#;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Write-only. FIFO flush
      FFLUSH         : Boolean := False;
      --  Cryptographic processor enable
      CRYPEN         : Boolean := False;
      --  GCM_CCMPH
      GCM_CCMPH      : CR_GCM_CCMPH_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  ALGOMODE
      ALGOMODE3      : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      ALGODIR        at 0 range 2 .. 2;
      ALGOMODE0      at 0 range 3 .. 5;
      DATATYPE       at 0 range 6 .. 7;
      KEYSIZE        at 0 range 8 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      FFLUSH         at 0 range 14 .. 14;
      CRYPEN         at 0 range 15 .. 15;
      GCM_CCMPH      at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      ALGOMODE3      at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Read-only. Input FIFO empty
      IFEM          : Boolean;
      --  Read-only. Input FIFO not full
      IFNF          : Boolean;
      --  Read-only. Output FIFO not empty
      OFNE          : Boolean;
      --  Read-only. Output FIFO full
      OFFU          : Boolean;
      --  Read-only. Busy bit
      BUSY          : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      IFEM          at 0 range 0 .. 0;
      IFNF          at 0 range 1 .. 1;
      OFNE          at 0 range 2 .. 2;
      OFFU          at 0 range 3 .. 3;
      BUSY          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  DMA control register
   type DMACR_Register is record
      --  DMA input enable
      DIEN          : Boolean := False;
      --  DMA output enable
      DOEN          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMACR_Register use record
      DIEN          at 0 range 0 .. 0;
      DOEN          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  interrupt mask set/clear register
   type IMSCR_Register is record
      --  Input FIFO service interrupt mask
      INIM          : Boolean := False;
      --  Output FIFO service interrupt mask
      OUTIM         : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMSCR_Register use record
      INIM          at 0 range 0 .. 0;
      OUTIM         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  raw interrupt status register
   type RISR_Register is record
      --  Read-only. Input FIFO service raw interrupt status
      INRIS         : Boolean;
      --  Read-only. Output FIFO service raw interrupt status
      OUTRIS        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RISR_Register use record
      INRIS         at 0 range 0 .. 0;
      OUTRIS        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  masked interrupt status register
   type MISR_Register is record
      --  Read-only. Input FIFO service masked interrupt status
      INMIS         : Boolean;
      --  Read-only. Output FIFO service masked interrupt status
      OUTMIS        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
      INMIS         at 0 range 0 .. 0;
      OUTMIS        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  K0LR_b array
   type K0LR_b_Field_Array is array (224 .. 255) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K0LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K0LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K0LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K0RR_b array
   type K0RR_b_Field_Array is array (192 .. 223) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K0RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K0RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K0RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K1LR_b array
   type K1LR_b_Field_Array is array (160 .. 191) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K1LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K1LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K1LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K1RR_b array
   type K1RR_b_Field_Array is array (128 .. 159) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K1RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K1RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K1RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K2LR_b array
   type K2LR_b_Field_Array is array (96 .. 127) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K2LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K2LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K2LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K2RR_b array
   type K2RR_b_Field_Array is array (64 .. 95) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K2RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K2RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K2RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K3LR_b array
   type K3LR_b_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K3LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K3LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K3LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K3RR_b array
   type K3RR_b_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K3RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K3RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K3RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  initialization vector registers
   type IV0LR_Register is record
      --  IV31
      IV31 : Boolean := False;
      --  IV30
      IV30 : Boolean := False;
      --  IV29
      IV29 : Boolean := False;
      --  IV28
      IV28 : Boolean := False;
      --  IV27
      IV27 : Boolean := False;
      --  IV26
      IV26 : Boolean := False;
      --  IV25
      IV25 : Boolean := False;
      --  IV24
      IV24 : Boolean := False;
      --  IV23
      IV23 : Boolean := False;
      --  IV22
      IV22 : Boolean := False;
      --  IV21
      IV21 : Boolean := False;
      --  IV20
      IV20 : Boolean := False;
      --  IV19
      IV19 : Boolean := False;
      --  IV18
      IV18 : Boolean := False;
      --  IV17
      IV17 : Boolean := False;
      --  IV16
      IV16 : Boolean := False;
      --  IV15
      IV15 : Boolean := False;
      --  IV14
      IV14 : Boolean := False;
      --  IV13
      IV13 : Boolean := False;
      --  IV12
      IV12 : Boolean := False;
      --  IV11
      IV11 : Boolean := False;
      --  IV10
      IV10 : Boolean := False;
      --  IV9
      IV9  : Boolean := False;
      --  IV8
      IV8  : Boolean := False;
      --  IV7
      IV7  : Boolean := False;
      --  IV6
      IV6  : Boolean := False;
      --  IV5
      IV5  : Boolean := False;
      --  IV4
      IV4  : Boolean := False;
      --  IV3
      IV3  : Boolean := False;
      --  IV2
      IV2  : Boolean := False;
      --  IV1
      IV1  : Boolean := False;
      --  IV0
      IV0  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV0LR_Register use record
      IV31 at 0 range 0 .. 0;
      IV30 at 0 range 1 .. 1;
      IV29 at 0 range 2 .. 2;
      IV28 at 0 range 3 .. 3;
      IV27 at 0 range 4 .. 4;
      IV26 at 0 range 5 .. 5;
      IV25 at 0 range 6 .. 6;
      IV24 at 0 range 7 .. 7;
      IV23 at 0 range 8 .. 8;
      IV22 at 0 range 9 .. 9;
      IV21 at 0 range 10 .. 10;
      IV20 at 0 range 11 .. 11;
      IV19 at 0 range 12 .. 12;
      IV18 at 0 range 13 .. 13;
      IV17 at 0 range 14 .. 14;
      IV16 at 0 range 15 .. 15;
      IV15 at 0 range 16 .. 16;
      IV14 at 0 range 17 .. 17;
      IV13 at 0 range 18 .. 18;
      IV12 at 0 range 19 .. 19;
      IV11 at 0 range 20 .. 20;
      IV10 at 0 range 21 .. 21;
      IV9  at 0 range 22 .. 22;
      IV8  at 0 range 23 .. 23;
      IV7  at 0 range 24 .. 24;
      IV6  at 0 range 25 .. 25;
      IV5  at 0 range 26 .. 26;
      IV4  at 0 range 27 .. 27;
      IV3  at 0 range 28 .. 28;
      IV2  at 0 range 29 .. 29;
      IV1  at 0 range 30 .. 30;
      IV0  at 0 range 31 .. 31;
   end record;

   --  initialization vector registers
   type IV0RR_Register is record
      --  IV63
      IV63 : Boolean := False;
      --  IV62
      IV62 : Boolean := False;
      --  IV61
      IV61 : Boolean := False;
      --  IV60
      IV60 : Boolean := False;
      --  IV59
      IV59 : Boolean := False;
      --  IV58
      IV58 : Boolean := False;
      --  IV57
      IV57 : Boolean := False;
      --  IV56
      IV56 : Boolean := False;
      --  IV55
      IV55 : Boolean := False;
      --  IV54
      IV54 : Boolean := False;
      --  IV53
      IV53 : Boolean := False;
      --  IV52
      IV52 : Boolean := False;
      --  IV51
      IV51 : Boolean := False;
      --  IV50
      IV50 : Boolean := False;
      --  IV49
      IV49 : Boolean := False;
      --  IV48
      IV48 : Boolean := False;
      --  IV47
      IV47 : Boolean := False;
      --  IV46
      IV46 : Boolean := False;
      --  IV45
      IV45 : Boolean := False;
      --  IV44
      IV44 : Boolean := False;
      --  IV43
      IV43 : Boolean := False;
      --  IV42
      IV42 : Boolean := False;
      --  IV41
      IV41 : Boolean := False;
      --  IV40
      IV40 : Boolean := False;
      --  IV39
      IV39 : Boolean := False;
      --  IV38
      IV38 : Boolean := False;
      --  IV37
      IV37 : Boolean := False;
      --  IV36
      IV36 : Boolean := False;
      --  IV35
      IV35 : Boolean := False;
      --  IV34
      IV34 : Boolean := False;
      --  IV33
      IV33 : Boolean := False;
      --  IV32
      IV32 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV0RR_Register use record
      IV63 at 0 range 0 .. 0;
      IV62 at 0 range 1 .. 1;
      IV61 at 0 range 2 .. 2;
      IV60 at 0 range 3 .. 3;
      IV59 at 0 range 4 .. 4;
      IV58 at 0 range 5 .. 5;
      IV57 at 0 range 6 .. 6;
      IV56 at 0 range 7 .. 7;
      IV55 at 0 range 8 .. 8;
      IV54 at 0 range 9 .. 9;
      IV53 at 0 range 10 .. 10;
      IV52 at 0 range 11 .. 11;
      IV51 at 0 range 12 .. 12;
      IV50 at 0 range 13 .. 13;
      IV49 at 0 range 14 .. 14;
      IV48 at 0 range 15 .. 15;
      IV47 at 0 range 16 .. 16;
      IV46 at 0 range 17 .. 17;
      IV45 at 0 range 18 .. 18;
      IV44 at 0 range 19 .. 19;
      IV43 at 0 range 20 .. 20;
      IV42 at 0 range 21 .. 21;
      IV41 at 0 range 22 .. 22;
      IV40 at 0 range 23 .. 23;
      IV39 at 0 range 24 .. 24;
      IV38 at 0 range 25 .. 25;
      IV37 at 0 range 26 .. 26;
      IV36 at 0 range 27 .. 27;
      IV35 at 0 range 28 .. 28;
      IV34 at 0 range 29 .. 29;
      IV33 at 0 range 30 .. 30;
      IV32 at 0 range 31 .. 31;
   end record;

   --  initialization vector registers
   type IV1LR_Register is record
      --  IV95
      IV95 : Boolean := False;
      --  IV94
      IV94 : Boolean := False;
      --  IV93
      IV93 : Boolean := False;
      --  IV92
      IV92 : Boolean := False;
      --  IV91
      IV91 : Boolean := False;
      --  IV90
      IV90 : Boolean := False;
      --  IV89
      IV89 : Boolean := False;
      --  IV88
      IV88 : Boolean := False;
      --  IV87
      IV87 : Boolean := False;
      --  IV86
      IV86 : Boolean := False;
      --  IV85
      IV85 : Boolean := False;
      --  IV84
      IV84 : Boolean := False;
      --  IV83
      IV83 : Boolean := False;
      --  IV82
      IV82 : Boolean := False;
      --  IV81
      IV81 : Boolean := False;
      --  IV80
      IV80 : Boolean := False;
      --  IV79
      IV79 : Boolean := False;
      --  IV78
      IV78 : Boolean := False;
      --  IV77
      IV77 : Boolean := False;
      --  IV76
      IV76 : Boolean := False;
      --  IV75
      IV75 : Boolean := False;
      --  IV74
      IV74 : Boolean := False;
      --  IV73
      IV73 : Boolean := False;
      --  IV72
      IV72 : Boolean := False;
      --  IV71
      IV71 : Boolean := False;
      --  IV70
      IV70 : Boolean := False;
      --  IV69
      IV69 : Boolean := False;
      --  IV68
      IV68 : Boolean := False;
      --  IV67
      IV67 : Boolean := False;
      --  IV66
      IV66 : Boolean := False;
      --  IV65
      IV65 : Boolean := False;
      --  IV64
      IV64 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV1LR_Register use record
      IV95 at 0 range 0 .. 0;
      IV94 at 0 range 1 .. 1;
      IV93 at 0 range 2 .. 2;
      IV92 at 0 range 3 .. 3;
      IV91 at 0 range 4 .. 4;
      IV90 at 0 range 5 .. 5;
      IV89 at 0 range 6 .. 6;
      IV88 at 0 range 7 .. 7;
      IV87 at 0 range 8 .. 8;
      IV86 at 0 range 9 .. 9;
      IV85 at 0 range 10 .. 10;
      IV84 at 0 range 11 .. 11;
      IV83 at 0 range 12 .. 12;
      IV82 at 0 range 13 .. 13;
      IV81 at 0 range 14 .. 14;
      IV80 at 0 range 15 .. 15;
      IV79 at 0 range 16 .. 16;
      IV78 at 0 range 17 .. 17;
      IV77 at 0 range 18 .. 18;
      IV76 at 0 range 19 .. 19;
      IV75 at 0 range 20 .. 20;
      IV74 at 0 range 21 .. 21;
      IV73 at 0 range 22 .. 22;
      IV72 at 0 range 23 .. 23;
      IV71 at 0 range 24 .. 24;
      IV70 at 0 range 25 .. 25;
      IV69 at 0 range 26 .. 26;
      IV68 at 0 range 27 .. 27;
      IV67 at 0 range 28 .. 28;
      IV66 at 0 range 29 .. 29;
      IV65 at 0 range 30 .. 30;
      IV64 at 0 range 31 .. 31;
   end record;

   --  initialization vector registers
   type IV1RR_Register is record
      --  IV127
      IV127 : Boolean := False;
      --  IV126
      IV126 : Boolean := False;
      --  IV125
      IV125 : Boolean := False;
      --  IV124
      IV124 : Boolean := False;
      --  IV123
      IV123 : Boolean := False;
      --  IV122
      IV122 : Boolean := False;
      --  IV121
      IV121 : Boolean := False;
      --  IV120
      IV120 : Boolean := False;
      --  IV119
      IV119 : Boolean := False;
      --  IV118
      IV118 : Boolean := False;
      --  IV117
      IV117 : Boolean := False;
      --  IV116
      IV116 : Boolean := False;
      --  IV115
      IV115 : Boolean := False;
      --  IV114
      IV114 : Boolean := False;
      --  IV113
      IV113 : Boolean := False;
      --  IV112
      IV112 : Boolean := False;
      --  IV111
      IV111 : Boolean := False;
      --  IV110
      IV110 : Boolean := False;
      --  IV109
      IV109 : Boolean := False;
      --  IV108
      IV108 : Boolean := False;
      --  IV107
      IV107 : Boolean := False;
      --  IV106
      IV106 : Boolean := False;
      --  IV105
      IV105 : Boolean := False;
      --  IV104
      IV104 : Boolean := False;
      --  IV103
      IV103 : Boolean := False;
      --  IV102
      IV102 : Boolean := False;
      --  IV101
      IV101 : Boolean := False;
      --  IV100
      IV100 : Boolean := False;
      --  IV99
      IV99  : Boolean := False;
      --  IV98
      IV98  : Boolean := False;
      --  IV97
      IV97  : Boolean := False;
      --  IV96
      IV96  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV1RR_Register use record
      IV127 at 0 range 0 .. 0;
      IV126 at 0 range 1 .. 1;
      IV125 at 0 range 2 .. 2;
      IV124 at 0 range 3 .. 3;
      IV123 at 0 range 4 .. 4;
      IV122 at 0 range 5 .. 5;
      IV121 at 0 range 6 .. 6;
      IV120 at 0 range 7 .. 7;
      IV119 at 0 range 8 .. 8;
      IV118 at 0 range 9 .. 9;
      IV117 at 0 range 10 .. 10;
      IV116 at 0 range 11 .. 11;
      IV115 at 0 range 12 .. 12;
      IV114 at 0 range 13 .. 13;
      IV113 at 0 range 14 .. 14;
      IV112 at 0 range 15 .. 15;
      IV111 at 0 range 16 .. 16;
      IV110 at 0 range 17 .. 17;
      IV109 at 0 range 18 .. 18;
      IV108 at 0 range 19 .. 19;
      IV107 at 0 range 20 .. 20;
      IV106 at 0 range 21 .. 21;
      IV105 at 0 range 22 .. 22;
      IV104 at 0 range 23 .. 23;
      IV103 at 0 range 24 .. 24;
      IV102 at 0 range 25 .. 25;
      IV101 at 0 range 26 .. 26;
      IV100 at 0 range 27 .. 27;
      IV99  at 0 range 28 .. 28;
      IV98  at 0 range 29 .. 29;
      IV97  at 0 range 30 .. 30;
      IV96  at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Cryptographic processor
   type CRYP_Peripheral is record
      --  control register
      CR         : aliased CR_Register;
      --  status register
      SR         : aliased SR_Register;
      --  data input register
      DIN        : aliased HAL.UInt32;
      --  data output register
      DOUT       : aliased HAL.UInt32;
      --  DMA control register
      DMACR      : aliased DMACR_Register;
      --  interrupt mask set/clear register
      IMSCR      : aliased IMSCR_Register;
      --  raw interrupt status register
      RISR       : aliased RISR_Register;
      --  masked interrupt status register
      MISR       : aliased MISR_Register;
      --  key registers
      K0LR       : aliased K0LR_Register;
      --  key registers
      K0RR       : aliased K0RR_Register;
      --  key registers
      K1LR       : aliased K1LR_Register;
      --  key registers
      K1RR       : aliased K1RR_Register;
      --  key registers
      K2LR       : aliased K2LR_Register;
      --  key registers
      K2RR       : aliased K2RR_Register;
      --  key registers
      K3LR       : aliased K3LR_Register;
      --  key registers
      K3RR       : aliased K3RR_Register;
      --  initialization vector registers
      IV0LR      : aliased IV0LR_Register;
      --  initialization vector registers
      IV0RR      : aliased IV0RR_Register;
      --  initialization vector registers
      IV1LR      : aliased IV1LR_Register;
      --  initialization vector registers
      IV1RR      : aliased IV1RR_Register;
      --  context swap register
      CSGCMCCM0R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM1R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM2R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM3R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM4R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM5R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM6R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM7R : aliased HAL.UInt32;
      --  context swap register
      CSGCM0R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM1R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM2R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM3R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM4R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM5R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM6R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM7R    : aliased HAL.UInt32;
   end record
     with Volatile;

   for CRYP_Peripheral use record
      CR         at 16#0# range 0 .. 31;
      SR         at 16#4# range 0 .. 31;
      DIN        at 16#8# range 0 .. 31;
      DOUT       at 16#C# range 0 .. 31;
      DMACR      at 16#10# range 0 .. 31;
      IMSCR      at 16#14# range 0 .. 31;
      RISR       at 16#18# range 0 .. 31;
      MISR       at 16#1C# range 0 .. 31;
      K0LR       at 16#20# range 0 .. 31;
      K0RR       at 16#24# range 0 .. 31;
      K1LR       at 16#28# range 0 .. 31;
      K1RR       at 16#2C# range 0 .. 31;
      K2LR       at 16#30# range 0 .. 31;
      K2RR       at 16#34# range 0 .. 31;
      K3LR       at 16#38# range 0 .. 31;
      K3RR       at 16#3C# range 0 .. 31;
      IV0LR      at 16#40# range 0 .. 31;
      IV0RR      at 16#44# range 0 .. 31;
      IV1LR      at 16#48# range 0 .. 31;
      IV1RR      at 16#4C# range 0 .. 31;
      CSGCMCCM0R at 16#50# range 0 .. 31;
      CSGCMCCM1R at 16#54# range 0 .. 31;
      CSGCMCCM2R at 16#58# range 0 .. 31;
      CSGCMCCM3R at 16#5C# range 0 .. 31;
      CSGCMCCM4R at 16#60# range 0 .. 31;
      CSGCMCCM5R at 16#64# range 0 .. 31;
      CSGCMCCM6R at 16#68# range 0 .. 31;
      CSGCMCCM7R at 16#6C# range 0 .. 31;
      CSGCM0R    at 16#70# range 0 .. 31;
      CSGCM1R    at 16#74# range 0 .. 31;
      CSGCM2R    at 16#78# range 0 .. 31;
      CSGCM3R    at 16#7C# range 0 .. 31;
      CSGCM4R    at 16#80# range 0 .. 31;
      CSGCM5R    at 16#84# range 0 .. 31;
      CSGCM6R    at 16#88# range 0 .. 31;
      CSGCM7R    at 16#8C# range 0 .. 31;
   end record;

   --  Cryptographic processor
   CRYP_Periph : aliased CRYP_Peripheral
     with Import, Address => System'To_Address (16#50060000#);

end STM32_SVD.CRYP;
