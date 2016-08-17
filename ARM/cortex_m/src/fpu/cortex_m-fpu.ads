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

--  The Floating-point unit (FPU) provides IEEE754-compliant operations on
--  single precision, 32-bit, floating-point values.

--  Of particular interest may be the Sqrt function, implemented as a single
--  machine code instruction.

--  See ST Micro Application Note AN4044: "Using floating-point unit (FPU) with
--  STM32F405/07xx and STM32F415/417xx microcontrollers" March 2012, Doc ID
--  022737 Rev 1, file "DM00047230.pdf"

--  See ST Micro Programming manual PM0214:
--  "STM32F3 and STM32F4 Series Cortex-M4 programming manual"
--  May 2014 DocID022708 Rev 4, file "DM00046982.pdf"
--  especially section 4.6.4

with HAL; use HAL;

package Cortex_M.FPU is

   function Sqrt (X : Float) return Float;

   type Rounding_Mode is
     (To_Nearest,
      To_Plus_Infinity,
      To_Minus_Infinity,
      To_Zero);

   type Bits_1  is mod 2**1 with Size => 1;
   type Bits_2  is mod 2**2 with Size => 2;
   type Bits_6  is mod 2**6 with Size => 6;

   type Status_Control_Register is record
      --  negative condition flag
      N         : Boolean;
      --  zero condition flag
      Z         : Boolean;
      --  carry condition flag
      C         : Boolean;
      --  overflow condition flag
      V         : Boolean;
      Reserved1 : Bits_1;
      --  Alternative Half-Precision format
      AHP       : Boolean;
      --  Default NaN mode
      DN        : Boolean;
      --  Flush-to-zero mode
      FZ        : Boolean;
      --  rounding mode
      RM        : Rounding_Mode;
      Reserved2 : Bits_6;
      Reserved3 : Byte;
      --  flush to zero
      IDC       : Boolean;
      Reserved4 : Bits_2;
      --  inexact result: the rounded (and returned) value is different from
      --  the mathematically exact result of the operation
      IXC       : Boolean;
      --  underflow, result is a denorm
      UFC       : Boolean;
      --  overflow, result depends on rounding mode
      OFC       : Boolean;
      --  division by zero
      DZC       : Boolean;
      --  invalid operation, result is a NaN
      IOC       : Boolean;
   end record
     with Atomic, Size => 32;

   for Status_Control_Register use record
      N         at 0 range 31 .. 31;
      Z         at 0 range 30 .. 30;
      C         at 0 range 29 .. 29;
      V         at 0 range 28 .. 28;
      Reserved1 at 0 range 27 .. 27;
      AHP       at 0 range 26 .. 26;
      DN        at 0 range 25 .. 25;
      FZ        at 0 range 24 .. 24;
      RM        at 0 range 22 .. 23;
      Reserved2 at 0 range 16 .. 21;
      Reserved3 at 0 range  8 .. 15;
      IDC       at 0 range  7 .. 7;
      Reserved4 at 0 range  5 .. 6;
      IXC       at 0 range  4 .. 4;
      UFC       at 0 range  3 .. 3;
      OFC       at 0 range  2 .. 2;
      DZC       at 0 range  1 .. 1;
      IOC       at 0 range  0 .. 0;
   end record;

   function FPSCR return Status_Control_Register;

end Cortex_M.FPU;
