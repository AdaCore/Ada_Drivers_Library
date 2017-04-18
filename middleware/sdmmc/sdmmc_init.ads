------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with HAL.SDMMC;  use HAL, HAL.SDMMC;

--  This package implements the SD and MMC card identification protocol.
package SDMMC_Init is

   procedure Card_Identification_Process (This   : in out SDMMC_Driver'Class;
                                          Info   : out Card_Information;
                                          Status : out SD_Error);
   --  Generic card identification process procedure.

   procedure Read_SCR (This   : in out SDMMC_Driver'Class;
                       Info   : Card_Information;
                       SCR    : out SDCard_Configuration_Register;
                       Status : out SD_Error);
   --  Retrieve the current SDCard configuration

   --  OCR bits
   SD_OCR_Power_Up             : constant := 16#8000_0000#;
   SD_OCR_High_Capacity        : constant := 16#4000_0000#;
   SD_OCR_Std_Capacity         : constant := 16#0000_0000#;

   SD_MAX_VOLT_TRIAL           : constant := 16#0000_FFFF#;

end SDMMC_Init;
