------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--  This file is based on:                                                  --
--   @file    stm32f469i_discovery_audio.h                                  --
--   @author  MCD Application Team                                          --
------------------------------------------------------------------------------

with HAL.Audio;      use HAL.Audio;
with HAL.I2C;        use HAL.I2C;
with Ravenscar_Time;

private with CS43L22;

package Audio is

   type CS43L22_Audio_Device (Port : not null Any_I2C_Port) is
     tagged limited private;

   procedure Initialize_Audio_Out
     (This      : in out CS43L22_Audio_Device;
      Volume    : Audio_Volume;
      Frequency : Audio_Frequency);

   procedure Play
     (This   : in out CS43L22_Audio_Device;
      Buffer : Audio_Buffer);

   procedure Pause
     (This : in out CS43L22_Audio_Device);

   procedure Resume
     (This : in out CS43L22_Audio_Device);

   procedure Stop
     (This : in out CS43L22_Audio_Device);

   procedure Set_Volume
     (This   : in out CS43L22_Audio_Device;
      Volume : Audio_Volume);

   procedure Set_Frequency
     (This      : in out CS43L22_Audio_Device;
      Frequency : Audio_Frequency);

private

   type CS43L22_Audio_Device (Port : not null Any_I2C_Port) is
     tagged limited record
      Device : CS43L22.CS43L22_Device (Port, Ravenscar_Time.Delays);
   end record;

end Audio;
