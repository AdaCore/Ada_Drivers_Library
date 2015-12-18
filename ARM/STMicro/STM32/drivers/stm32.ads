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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

--  This file provides type definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

with Interfaces;              use Interfaces;
with System.Storage_Elements; use System;

with STM32_SVD;               use STM32_SVD;

package STM32 is
   use type System.Storage_Elements.Storage_Offset;

   subtype Word      is Interfaces.Unsigned_32;
   subtype Half_Word is Interfaces.Unsigned_16;
   subtype Byte      is Interfaces.Unsigned_8;

   subtype Bits_1  is STM32_SVD.Bit;
   subtype Bits_2  is STM32_SVD.UInt2;
   subtype Bits_3  is STM32_SVD.UInt3;
   subtype Bits_4  is STM32_SVD.UInt4;
   subtype Bits_5  is STM32_SVD.UInt5;
   subtype Bits_6  is STM32_SVD.UInt6;
   subtype Bits_7  is STM32_SVD.UInt7;
   subtype Bits_9  is STM32_SVD.UInt9;
   subtype Bits_10 is STM32_SVD.UInt10;
   subtype Bits_11 is STM32_SVD.UInt11;
   subtype Bits_12 is STM32_SVD.UInt12;
   subtype Bits_13 is STM32_SVD.UInt13;
   subtype Bits_14 is STM32_SVD.UInt14;
   subtype Bits_15 is STM32_SVD.UInt15;
   subtype Bits_17 is STM32_SVD.UInt17;
   subtype Bits_18 is STM32_SVD.UInt18;
   subtype Bits_19 is STM32_SVD.UInt19;
   subtype Bits_20 is STM32_SVD.UInt20;
   subtype Bits_21 is STM32_SVD.UInt21;
   subtype Bits_22 is STM32_SVD.UInt22;
   subtype Bits_23 is STM32_SVD.UInt23;
   subtype Bits_24 is STM32_SVD.UInt24;
   subtype Bits_25 is STM32_SVD.UInt25;
   subtype Bits_26 is STM32_SVD.UInt26;
   subtype Bits_27 is STM32_SVD.UInt27;
   subtype Bits_28 is STM32_SVD.UInt28;
   subtype Bits_29 is STM32_SVD.UInt29;
   subtype Bits_30 is STM32_SVD.UInt30;

   type Bits_32x1 is array (0 .. 31) of Bits_1 with Pack, Size => 32;
   type Bits_16x2 is array (0 .. 15) of Bits_2 with Pack, Size => 32;
   type Bits_8x4  is array (0 ..  7) of Bits_4 with Pack, Size => 32;

end STM32;
