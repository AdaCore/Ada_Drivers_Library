--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

--  with HAL;
with System;

package SAM_SVD.LOCKBIT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Lock Bits Word 0
   type LOCKBIT_LOCKBIT_WORD0_Register is record
      --  Lock Region 0
      LOCK_REGION_0  : Boolean := False;
      --  Lock Region 1
      LOCK_REGION_1  : Boolean := False;
      --  Lock Region 2
      LOCK_REGION_2  : Boolean := False;
      --  Lock Region 3
      LOCK_REGION_3  : Boolean := False;
      --  Lock Region 4
      LOCK_REGION_4  : Boolean := False;
      --  Lock Region 5
      LOCK_REGION_5  : Boolean := False;
      --  Lock Region 6
      LOCK_REGION_6  : Boolean := False;
      --  Lock Region 7
      LOCK_REGION_7  : Boolean := False;
      --  Lock Region 8
      LOCK_REGION_8  : Boolean := False;
      --  Lock Region 9
      LOCK_REGION_9  : Boolean := False;
      --  Lock Region 10
      LOCK_REGION_10 : Boolean := False;
      --  Lock Region 11
      LOCK_REGION_11 : Boolean := False;
      --  Lock Region 12
      LOCK_REGION_12 : Boolean := False;
      --  Lock Region 13
      LOCK_REGION_13 : Boolean := False;
      --  Lock Region 14
      LOCK_REGION_14 : Boolean := False;
      --  Lock Region 15
      LOCK_REGION_15 : Boolean := False;
      --  Lock Region 16
      LOCK_REGION_16 : Boolean := False;
      --  Lock Region 17
      LOCK_REGION_17 : Boolean := False;
      --  Lock Region 18
      LOCK_REGION_18 : Boolean := False;
      --  Lock Region 19
      LOCK_REGION_19 : Boolean := False;
      --  Lock Region 20
      LOCK_REGION_20 : Boolean := False;
      --  Lock Region 21
      LOCK_REGION_21 : Boolean := False;
      --  Lock Region 22
      LOCK_REGION_22 : Boolean := False;
      --  Lock Region 23
      LOCK_REGION_23 : Boolean := False;
      --  Lock Region 24
      LOCK_REGION_24 : Boolean := False;
      --  Lock Region 25
      LOCK_REGION_25 : Boolean := False;
      --  Lock Region 26
      LOCK_REGION_26 : Boolean := False;
      --  Lock Region 27
      LOCK_REGION_27 : Boolean := False;
      --  Lock Region 28
      LOCK_REGION_28 : Boolean := False;
      --  Lock Region 29
      LOCK_REGION_29 : Boolean := False;
      --  Lock Region 30
      LOCK_REGION_30 : Boolean := False;
      --  Lock Region 31
      LOCK_REGION_31 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCKBIT_LOCKBIT_WORD0_Register use record
      LOCK_REGION_0  at 0 range 0 .. 0;
      LOCK_REGION_1  at 0 range 1 .. 1;
      LOCK_REGION_2  at 0 range 2 .. 2;
      LOCK_REGION_3  at 0 range 3 .. 3;
      LOCK_REGION_4  at 0 range 4 .. 4;
      LOCK_REGION_5  at 0 range 5 .. 5;
      LOCK_REGION_6  at 0 range 6 .. 6;
      LOCK_REGION_7  at 0 range 7 .. 7;
      LOCK_REGION_8  at 0 range 8 .. 8;
      LOCK_REGION_9  at 0 range 9 .. 9;
      LOCK_REGION_10 at 0 range 10 .. 10;
      LOCK_REGION_11 at 0 range 11 .. 11;
      LOCK_REGION_12 at 0 range 12 .. 12;
      LOCK_REGION_13 at 0 range 13 .. 13;
      LOCK_REGION_14 at 0 range 14 .. 14;
      LOCK_REGION_15 at 0 range 15 .. 15;
      LOCK_REGION_16 at 0 range 16 .. 16;
      LOCK_REGION_17 at 0 range 17 .. 17;
      LOCK_REGION_18 at 0 range 18 .. 18;
      LOCK_REGION_19 at 0 range 19 .. 19;
      LOCK_REGION_20 at 0 range 20 .. 20;
      LOCK_REGION_21 at 0 range 21 .. 21;
      LOCK_REGION_22 at 0 range 22 .. 22;
      LOCK_REGION_23 at 0 range 23 .. 23;
      LOCK_REGION_24 at 0 range 24 .. 24;
      LOCK_REGION_25 at 0 range 25 .. 25;
      LOCK_REGION_26 at 0 range 26 .. 26;
      LOCK_REGION_27 at 0 range 27 .. 27;
      LOCK_REGION_28 at 0 range 28 .. 28;
      LOCK_REGION_29 at 0 range 29 .. 29;
      LOCK_REGION_30 at 0 range 30 .. 30;
      LOCK_REGION_31 at 0 range 31 .. 31;
   end record;

   --  Lock Bits Word 1
   type LOCKBIT_LOCKBIT_WORD1_Register is record
      --  Lock Region 32
      LOCK_REGION_32 : Boolean := False;
      --  Lock Region 33
      LOCK_REGION_33 : Boolean := False;
      --  Lock Region 34
      LOCK_REGION_34 : Boolean := False;
      --  Lock Region 35
      LOCK_REGION_35 : Boolean := False;
      --  Lock Region 36
      LOCK_REGION_36 : Boolean := False;
      --  Lock Region 37
      LOCK_REGION_37 : Boolean := False;
      --  Lock Region 38
      LOCK_REGION_38 : Boolean := False;
      --  Lock Region 39
      LOCK_REGION_39 : Boolean := False;
      --  Lock Region 40
      LOCK_REGION_40 : Boolean := False;
      --  Lock Region 41
      LOCK_REGION_41 : Boolean := False;
      --  Lock Region 42
      LOCK_REGION_42 : Boolean := False;
      --  Lock Region 43
      LOCK_REGION_43 : Boolean := False;
      --  Lock Region 44
      LOCK_REGION_44 : Boolean := False;
      --  Lock Region 45
      LOCK_REGION_45 : Boolean := False;
      --  Lock Region 46
      LOCK_REGION_46 : Boolean := False;
      --  Lock Region 47
      LOCK_REGION_47 : Boolean := False;
      --  Lock Region 48
      LOCK_REGION_48 : Boolean := False;
      --  Lock Region 49
      LOCK_REGION_49 : Boolean := False;
      --  Lock Region 50
      LOCK_REGION_50 : Boolean := False;
      --  Lock Region 51
      LOCK_REGION_51 : Boolean := False;
      --  Lock Region 52
      LOCK_REGION_52 : Boolean := False;
      --  Lock Region 53
      LOCK_REGION_53 : Boolean := False;
      --  Lock Region 54
      LOCK_REGION_54 : Boolean := False;
      --  Lock Region 55
      LOCK_REGION_55 : Boolean := False;
      --  Lock Region 56
      LOCK_REGION_56 : Boolean := False;
      --  Lock Region 57
      LOCK_REGION_57 : Boolean := False;
      --  Lock Region 58
      LOCK_REGION_58 : Boolean := False;
      --  Lock Region 59
      LOCK_REGION_59 : Boolean := False;
      --  Lock Region 60
      LOCK_REGION_60 : Boolean := False;
      --  Lock Region 61
      LOCK_REGION_61 : Boolean := False;
      --  Lock Region 62
      LOCK_REGION_62 : Boolean := False;
      --  Lock Region 63
      LOCK_REGION_63 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCKBIT_LOCKBIT_WORD1_Register use record
      LOCK_REGION_32 at 0 range 0 .. 0;
      LOCK_REGION_33 at 0 range 1 .. 1;
      LOCK_REGION_34 at 0 range 2 .. 2;
      LOCK_REGION_35 at 0 range 3 .. 3;
      LOCK_REGION_36 at 0 range 4 .. 4;
      LOCK_REGION_37 at 0 range 5 .. 5;
      LOCK_REGION_38 at 0 range 6 .. 6;
      LOCK_REGION_39 at 0 range 7 .. 7;
      LOCK_REGION_40 at 0 range 8 .. 8;
      LOCK_REGION_41 at 0 range 9 .. 9;
      LOCK_REGION_42 at 0 range 10 .. 10;
      LOCK_REGION_43 at 0 range 11 .. 11;
      LOCK_REGION_44 at 0 range 12 .. 12;
      LOCK_REGION_45 at 0 range 13 .. 13;
      LOCK_REGION_46 at 0 range 14 .. 14;
      LOCK_REGION_47 at 0 range 15 .. 15;
      LOCK_REGION_48 at 0 range 16 .. 16;
      LOCK_REGION_49 at 0 range 17 .. 17;
      LOCK_REGION_50 at 0 range 18 .. 18;
      LOCK_REGION_51 at 0 range 19 .. 19;
      LOCK_REGION_52 at 0 range 20 .. 20;
      LOCK_REGION_53 at 0 range 21 .. 21;
      LOCK_REGION_54 at 0 range 22 .. 22;
      LOCK_REGION_55 at 0 range 23 .. 23;
      LOCK_REGION_56 at 0 range 24 .. 24;
      LOCK_REGION_57 at 0 range 25 .. 25;
      LOCK_REGION_58 at 0 range 26 .. 26;
      LOCK_REGION_59 at 0 range 27 .. 27;
      LOCK_REGION_60 at 0 range 28 .. 28;
      LOCK_REGION_61 at 0 range 29 .. 29;
      LOCK_REGION_62 at 0 range 30 .. 30;
      LOCK_REGION_63 at 0 range 31 .. 31;
   end record;

   --  Lock Bits Word 2
   type LOCKBIT_LOCKBIT_WORD2_Register is record
      --  Lock Region 64
      LOCK_REGION_64 : Boolean := False;
      --  Lock Region 65
      LOCK_REGION_65 : Boolean := False;
      --  Lock Region 66
      LOCK_REGION_66 : Boolean := False;
      --  Lock Region 67
      LOCK_REGION_67 : Boolean := False;
      --  Lock Region 68
      LOCK_REGION_68 : Boolean := False;
      --  Lock Region 69
      LOCK_REGION_69 : Boolean := False;
      --  Lock Region 70
      LOCK_REGION_70 : Boolean := False;
      --  Lock Region 71
      LOCK_REGION_71 : Boolean := False;
      --  Lock Region 72
      LOCK_REGION_72 : Boolean := False;
      --  Lock Region 73
      LOCK_REGION_73 : Boolean := False;
      --  Lock Region 74
      LOCK_REGION_74 : Boolean := False;
      --  Lock Region 75
      LOCK_REGION_75 : Boolean := False;
      --  Lock Region 76
      LOCK_REGION_76 : Boolean := False;
      --  Lock Region 77
      LOCK_REGION_77 : Boolean := False;
      --  Lock Region 78
      LOCK_REGION_78 : Boolean := False;
      --  Lock Region 79
      LOCK_REGION_79 : Boolean := False;
      --  Lock Region 80
      LOCK_REGION_80 : Boolean := False;
      --  Lock Region 81
      LOCK_REGION_81 : Boolean := False;
      --  Lock Region 82
      LOCK_REGION_82 : Boolean := False;
      --  Lock Region 83
      LOCK_REGION_83 : Boolean := False;
      --  Lock Region 84
      LOCK_REGION_84 : Boolean := False;
      --  Lock Region 85
      LOCK_REGION_85 : Boolean := False;
      --  Lock Region 86
      LOCK_REGION_86 : Boolean := False;
      --  Lock Region 87
      LOCK_REGION_87 : Boolean := False;
      --  Lock Region 88
      LOCK_REGION_88 : Boolean := False;
      --  Lock Region 89
      LOCK_REGION_89 : Boolean := False;
      --  Lock Region 90
      LOCK_REGION_90 : Boolean := False;
      --  Lock Region 91
      LOCK_REGION_91 : Boolean := False;
      --  Lock Region 92
      LOCK_REGION_92 : Boolean := False;
      --  Lock Region 93
      LOCK_REGION_93 : Boolean := False;
      --  Lock Region 94
      LOCK_REGION_94 : Boolean := False;
      --  Lock Region 95
      LOCK_REGION_95 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCKBIT_LOCKBIT_WORD2_Register use record
      LOCK_REGION_64 at 0 range 0 .. 0;
      LOCK_REGION_65 at 0 range 1 .. 1;
      LOCK_REGION_66 at 0 range 2 .. 2;
      LOCK_REGION_67 at 0 range 3 .. 3;
      LOCK_REGION_68 at 0 range 4 .. 4;
      LOCK_REGION_69 at 0 range 5 .. 5;
      LOCK_REGION_70 at 0 range 6 .. 6;
      LOCK_REGION_71 at 0 range 7 .. 7;
      LOCK_REGION_72 at 0 range 8 .. 8;
      LOCK_REGION_73 at 0 range 9 .. 9;
      LOCK_REGION_74 at 0 range 10 .. 10;
      LOCK_REGION_75 at 0 range 11 .. 11;
      LOCK_REGION_76 at 0 range 12 .. 12;
      LOCK_REGION_77 at 0 range 13 .. 13;
      LOCK_REGION_78 at 0 range 14 .. 14;
      LOCK_REGION_79 at 0 range 15 .. 15;
      LOCK_REGION_80 at 0 range 16 .. 16;
      LOCK_REGION_81 at 0 range 17 .. 17;
      LOCK_REGION_82 at 0 range 18 .. 18;
      LOCK_REGION_83 at 0 range 19 .. 19;
      LOCK_REGION_84 at 0 range 20 .. 20;
      LOCK_REGION_85 at 0 range 21 .. 21;
      LOCK_REGION_86 at 0 range 22 .. 22;
      LOCK_REGION_87 at 0 range 23 .. 23;
      LOCK_REGION_88 at 0 range 24 .. 24;
      LOCK_REGION_89 at 0 range 25 .. 25;
      LOCK_REGION_90 at 0 range 26 .. 26;
      LOCK_REGION_91 at 0 range 27 .. 27;
      LOCK_REGION_92 at 0 range 28 .. 28;
      LOCK_REGION_93 at 0 range 29 .. 29;
      LOCK_REGION_94 at 0 range 30 .. 30;
      LOCK_REGION_95 at 0 range 31 .. 31;
   end record;

   --  Lock Bits Word 3
   type LOCKBIT_LOCKBIT_WORD3_Register is record
      --  Lock Region 96
      LOCK_REGION_96  : Boolean := False;
      --  Lock Region 97
      LOCK_REGION_97  : Boolean := False;
      --  Lock Region 98
      LOCK_REGION_98  : Boolean := False;
      --  Lock Region 99
      LOCK_REGION_99  : Boolean := False;
      --  Lock Region 100
      LOCK_REGION_100 : Boolean := False;
      --  Lock Region 101
      LOCK_REGION_101 : Boolean := False;
      --  Lock Region 102
      LOCK_REGION_102 : Boolean := False;
      --  Lock Region 103
      LOCK_REGION_103 : Boolean := False;
      --  Lock Region 104
      LOCK_REGION_104 : Boolean := False;
      --  Lock Region 105
      LOCK_REGION_105 : Boolean := False;
      --  Lock Region 106
      LOCK_REGION_106 : Boolean := False;
      --  Lock Region 107
      LOCK_REGION_107 : Boolean := False;
      --  Lock Region 108
      LOCK_REGION_108 : Boolean := False;
      --  Lock Region 109
      LOCK_REGION_109 : Boolean := False;
      --  Lock Region 110
      LOCK_REGION_110 : Boolean := False;
      --  Lock Region 111
      LOCK_REGION_111 : Boolean := False;
      --  Lock Region 112
      LOCK_REGION_112 : Boolean := False;
      --  Lock Region 113
      LOCK_REGION_113 : Boolean := False;
      --  Lock Region 114
      LOCK_REGION_114 : Boolean := False;
      --  Lock Region 115
      LOCK_REGION_115 : Boolean := False;
      --  Lock Region 116
      LOCK_REGION_116 : Boolean := False;
      --  Lock Region 117
      LOCK_REGION_117 : Boolean := False;
      --  Lock Region 118
      LOCK_REGION_118 : Boolean := False;
      --  Lock Region 119
      LOCK_REGION_119 : Boolean := False;
      --  Lock Region 120
      LOCK_REGION_120 : Boolean := False;
      --  Lock Region 121
      LOCK_REGION_121 : Boolean := False;
      --  Lock Region 122
      LOCK_REGION_122 : Boolean := False;
      --  Lock Region 123
      LOCK_REGION_123 : Boolean := False;
      --  Lock Region 124
      LOCK_REGION_124 : Boolean := False;
      --  Lock Region 125
      LOCK_REGION_125 : Boolean := False;
      --  Lock Region 126
      LOCK_REGION_126 : Boolean := False;
      --  Lock Region 127
      LOCK_REGION_127 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCKBIT_LOCKBIT_WORD3_Register use record
      LOCK_REGION_96  at 0 range 0 .. 0;
      LOCK_REGION_97  at 0 range 1 .. 1;
      LOCK_REGION_98  at 0 range 2 .. 2;
      LOCK_REGION_99  at 0 range 3 .. 3;
      LOCK_REGION_100 at 0 range 4 .. 4;
      LOCK_REGION_101 at 0 range 5 .. 5;
      LOCK_REGION_102 at 0 range 6 .. 6;
      LOCK_REGION_103 at 0 range 7 .. 7;
      LOCK_REGION_104 at 0 range 8 .. 8;
      LOCK_REGION_105 at 0 range 9 .. 9;
      LOCK_REGION_106 at 0 range 10 .. 10;
      LOCK_REGION_107 at 0 range 11 .. 11;
      LOCK_REGION_108 at 0 range 12 .. 12;
      LOCK_REGION_109 at 0 range 13 .. 13;
      LOCK_REGION_110 at 0 range 14 .. 14;
      LOCK_REGION_111 at 0 range 15 .. 15;
      LOCK_REGION_112 at 0 range 16 .. 16;
      LOCK_REGION_113 at 0 range 17 .. 17;
      LOCK_REGION_114 at 0 range 18 .. 18;
      LOCK_REGION_115 at 0 range 19 .. 19;
      LOCK_REGION_116 at 0 range 20 .. 20;
      LOCK_REGION_117 at 0 range 21 .. 21;
      LOCK_REGION_118 at 0 range 22 .. 22;
      LOCK_REGION_119 at 0 range 23 .. 23;
      LOCK_REGION_120 at 0 range 24 .. 24;
      LOCK_REGION_121 at 0 range 25 .. 25;
      LOCK_REGION_122 at 0 range 26 .. 26;
      LOCK_REGION_123 at 0 range 27 .. 27;
      LOCK_REGION_124 at 0 range 28 .. 28;
      LOCK_REGION_125 at 0 range 29 .. 29;
      LOCK_REGION_126 at 0 range 30 .. 30;
      LOCK_REGION_127 at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type LOCKBIT_Peripheral is record
      --  Lock Bits Word 0
      LOCKBIT_WORD0 : aliased LOCKBIT_LOCKBIT_WORD0_Register;
      --  Lock Bits Word 1
      LOCKBIT_WORD1 : aliased LOCKBIT_LOCKBIT_WORD1_Register;
      --  Lock Bits Word 2
      LOCKBIT_WORD2 : aliased LOCKBIT_LOCKBIT_WORD2_Register;
      --  Lock Bits Word 3
      LOCKBIT_WORD3 : aliased LOCKBIT_LOCKBIT_WORD3_Register;
   end record
     with Volatile;

   for LOCKBIT_Peripheral use record
      LOCKBIT_WORD0 at 16#0# range 0 .. 31;
      LOCKBIT_WORD1 at 16#4# range 0 .. 31;
      LOCKBIT_WORD2 at 16#8# range 0 .. 31;
      LOCKBIT_WORD3 at 16#C# range 0 .. 31;
   end record;

   LOCKBIT_Periph : aliased LOCKBIT_Peripheral
     with Import, Address => System'To_Address (16#0#);

end SAM_SVD.LOCKBIT;
