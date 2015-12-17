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

with System.Storage_Elements;
use System;
use type System.Storage_Elements.Storage_Offset;

with STM32_SVD;

package STM32 is

   subtype Word      is STM32_SVD.Word;
   subtype Half_Word is STM32_SVD.Short;
   subtype Byte      is STM32_SVD.Byte;   -- for shift/rotate

   type Bits_1  is mod 2**1  with Size => 1;
   type Bits_2  is mod 2**2  with Size => 2;
   type Bits_3  is mod 2**3  with Size => 3;
   type Bits_4  is mod 2**4  with Size => 4;
   type Bits_5  is mod 2**5  with Size => 5;
   type Bits_6  is mod 2**6  with Size => 6;
   type Bits_7  is mod 2**7  with Size => 7;
   type Bits_8  is mod 2**8  with Size => 8;
   type Bits_9  is mod 2**9  with Size => 9;
   type Bits_10 is mod 2**10 with Size => 10;
   type Bits_11 is mod 2**11 with Size => 11;
   type Bits_12 is mod 2**12 with Size => 12;
   type Bits_13 is mod 2**13 with Size => 13;
   type Bits_14 is mod 2**14 with Size => 14;
   type Bits_15 is mod 2**15 with Size => 15;
   type Bits_16 is mod 2**16 with Size => 16;
   type Bits_17 is mod 2**17 with Size => 17;
   type Bits_18 is mod 2**18 with Size => 18;
   type Bits_19 is mod 2**19 with Size => 19;
   type Bits_20 is mod 2**20 with Size => 20;
   type Bits_21 is mod 2**21 with Size => 21;
   type Bits_22 is mod 2**22 with Size => 22;
   type Bits_23 is mod 2**23 with Size => 23;
   type Bits_24 is mod 2**24 with Size => 24;
   type Bits_25 is mod 2**25 with Size => 25;
   type Bits_26 is mod 2**26 with Size => 26;
   type Bits_27 is mod 2**27 with Size => 27;
   type Bits_28 is mod 2**28 with Size => 28;
   type Bits_29 is mod 2**29 with Size => 29;
   type Bits_30 is mod 2**30 with Size => 30;

   type Bits_32x1 is array (0 .. 31) of Bits_1 with Pack, Size => 32;
   type Bits_16x2 is array (0 .. 15) of Bits_2 with Pack, Size => 32;
   type Bits_8x4  is array (0 ..  7) of Bits_4 with Pack, Size => 32;

end STM32;
