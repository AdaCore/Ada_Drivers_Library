------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

with System; use System;

package STM32.Eth is
   procedure Initialize_RMII;
   --  Initialize the driver using RMII configuration.

   procedure Read_MMI (Reg : UInt5; Val : out UInt16);
   --  Read an MMI register.

   procedure Init_Mac;
   --  Initialize MAC layer.

   procedure Start_Rx;

   procedure Wait_Packet;

   type TDES0_Type is record
      Own        : Bit;
      Ic         : Bit;
      Ls         : Bit;
      Fs         : Bit;
      Dc         : Bit;
      Dp         : Bit;
      Ttse       : Bit;
      Reserved_1 : Bit;
      Cic        : UInt2;
      Ter        : Bit;
      Tch        : Bit;
      Reserved_2 : UInt2;
      Ttss       : Bit;
      Ihe        : Bit;
      Es         : Bit;
      Jt         : Bit;
      Ff         : Bit;
      Ipe        : Bit;
      Lca        : Bit;
      Nc         : Bit;
      Lco        : Bit;
      Ec         : Bit;
      Vf         : Bit;
      Cc         : UInt4;
      Ed         : Bit;
      Uf         : Bit;
      Db         : Bit;
   end record;

   for TDES0_Type use record
      Own at 0 range 31 .. 31;
      Ic at 0 range 30 .. 30;
      Ls at 0 range 29 .. 29;
      Fs at 0 range 28 .. 28;
      Dc at 0 range 27 .. 27;
      Dp at 0 range 26 .. 26;
      Ttse at 0 range 25 .. 25;
      Reserved_1 at 0 range 24 .. 24;
      Cic at 0 range 22 .. 23;
      Ter at 0 range 21 .. 21;
      Tch at 0 range 20 .. 20;
      Reserved_2 at 0 range 18 .. 19;
      Ttss at 0 range 17 .. 17;
      Ihe at 0 range 16 .. 16;
      Es at 0 range 15 .. 15;
      Jt at 0 range 14 .. 14;
      Ff at 0 range 13 .. 13;
      Ipe at 0 range 12 .. 12;
      Lca at 0 range 11 .. 11;
      Nc at 0 range 10 .. 10;
      Lco at 0 range 9 .. 9;
      Ec at 0 range 8 .. 8;
      Vf at 0 range 7 .. 7;
      Cc at 0 range 3 .. 6;
      Ed at 0 range 2 .. 2;
      Uf at 0 range 1 .. 1;
      Db at 0 range 0 .. 0;
   end record;

   type TDES1_Type is record
      Tbs1           : UInt13;
      Reserved_13_15 : UInt3;
      Tbs2           : UInt13;
      Reserved_29_31 : UInt3;
   end record;

   for TDES1_Type use record
      Tbs1 at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      Tbs2 at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   type Tx_Desc_Type is record
      Tdes0 : TDES0_Type;
      Tdes1 : TDES1_Type;
      Tdes2 : Address;
      Tdes3 : Address;
   end record;

   for Tx_Desc_Type use record
      Tdes0 at 0 range 0 .. 31;
      Tdes1 at 4 range 0 .. 31;
      Tdes2 at 8 range 0 .. 31;
      Tdes3 at 12 range 0 .. 31;
   end record;

   type Rdes0_Type is record
      Pce_Esa : Bit;
      Ce      : Bit;
      Dre      : Bit;
      Re      : Bit;
      Rwt     : Bit;
      Ft      : Bit;
      Lco     : Bit;
      Iphce   : Bit;
      Ls      : Bit;
      Fs      : Bit;
      Vlan    : Bit;
      Oe      : Bit;
      Le      : Bit;
      Saf     : Bit;
      De      : Bit;
      Es      : Bit;
      Fl      : UInt14;
      Afm     : Bit;
      Own     : Bit;
   end record;

   for Rdes0_Type use record
      Pce_Esa at 0 range 0 .. 0;
      Ce at 0 range 1 .. 1;
      Dre at 0 range 2 .. 2;
      Re at 0 range 3 .. 3;
      Rwt at 0 range 4 .. 4;
      Ft at 0 range 5 .. 5;
      Lco at 0 range 6 .. 6;
      Iphce at 0 range 7 .. 7;
      Ls at 0 range 8 .. 8;
      Fs at 0 range 9 .. 9;
      Vlan at 0 range 10 .. 10;
      Oe at 0 range 11 .. 11;
      Le at 0 range 12 .. 12;
      Saf at 0 range 13 .. 13;
      De at 0 range 14 .. 14;
      Es at 0 range 15 .. 15;
      Fl at 0 range 16 .. 29;
      Afm at 0 range 30 .. 30;
      Own at 0 range 31 .. 31;
   end record;

   type Rdes1_Type is record
      Rbs            : UInt13;
      Reserved_13    : Bit;
      Rch            : Bit;
      Rer            : Bit;
      Rbs2           : UInt13;
      Reserved_29_30 : UInt2;
      Dic            : Bit;
   end record;

   for Rdes1_Type use record
      Rbs at 0 range 0 .. 12;
      Reserved_13 at 0 range 13 .. 13;
      Rch at 0 range 14 .. 14;
      Rer at 0 range 15 .. 15;
      Rbs2 at 0 range 16 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      Dic at 0 range 31 .. 31;
   end record;

   type Rx_Desc_Type is record
      Rdes0 : Rdes0_Type;
      Rdes1 : Rdes1_Type;
      Rdes2 : UInt32;
      Rdes3 : UInt32;
   end record;

   for Rx_Desc_Type use record
      Rdes0 at 0 range 0 .. 31;
      Rdes1 at 4 range 0 .. 31;
      Rdes2 at 8 range 0 .. 31;
      Rdes3 at 12 range 0 .. 31;
   end record;

end STM32.Eth;
