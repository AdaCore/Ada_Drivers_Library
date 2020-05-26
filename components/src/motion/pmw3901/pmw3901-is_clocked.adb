------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
--           Copyright (C) 2020, Simon Wright (simon@pushface.org)          --
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
--     3. Neither the name of Pixart Imaging nor the names of its           --
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

with STM32_SVD.RCC;
with System;

separate (PMW3901)
function Is_Clocked (This : STM32.SPI.SPI_Port'Class) return Boolean is
   use STM32_SVD;
   use STM32_SVD.RCC;
   use type System.Address;
begin
   if This.Periph.all'Address = SPI1_Base then
      return RCC_Periph.APB2ENR.SPI1EN;
   elsif This.Periph.all'Address = SPI2_Base then
      return RCC_Periph.APB1ENR.SPI2EN;
   elsif This.Periph.all'Address = SPI3_Base then
      return RCC_Periph.APB1ENR.SPI3EN;
   else
      return raise STM32.Device.Unknown_Device;
   end if;
end Is_Clocked;
