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
--   @file    stm32f746g_discovery_audio.h                                  --
--   @author  MCD Application Team                                          --
------------------------------------------------------------------------------

--  with HAL.Audio; use HAL.Audio;
with HAL; use HAL;
with HAL.I2C;   use HAL.I2C;
with Ravenscar_Time;

with WM8994;

package Audio is

   type WM8994_Audio_Device (Port : not null Any_I2C_Port) is
     tagged limited private;

   type Audio_Output_Device is new WM8994.Output_Device
     range WM8994.Speaker .. WM8994.Auto;
   --  Only No_Output is not included

   type Audio_Frequency is new WM8994.Audio_Frequency;
   --  TODO: use HAL.Audio package's type, with WM8994 additions to that type

   type Audio_Volume is range 0 .. 100; -- a percentage
   --  TODO: use HAL.Audio package's type

   type Audio_Buffer is array (Natural range <>) of UInt16
     with Component_Size => 16, Alignment => 2;
   --  TODO: change to signed 16-bit, since that's apparently what should be used for PCM samples,
   --  so revert the change to HAL.Audio.Audio_Buffer (so it is Integer_16 again) and use that.

   procedure Initialize
     (This      : in out WM8994_Audio_Device;
      Volume    : Audio_Volume;
      Frequency : Audio_Frequency;
      Sink      : Audio_Output_Device);
   --  This routine initializes the hardware and configures the volume,
   --  sampling frequency, and output device (the sink). This routine must be
   --  called, before any others. The routines for setting the volume and
   --  the output frequency are optional.

   procedure Start_Playing
     (This   : in out WM8994_Audio_Device;
      Buffer : Audio_Buffer);
   --  Start playing, for the first time, content from the audio file/stream.
   --  This routine must be called, perhaps just once but more than once if the
   --  other routines below are called. The effect is to tell the underlying
   --  WM8994 codec where the buffer to be played is located, and cause the
   --  codec to start playing that buffer. Playing continues after the call
   --  returns. An additional mechanism, outside this package, updates the
   --  content of the buffer while the codec is playing it. That update/play
   --  process continues until either there is no more music to be played, or
   --  Stop or Pause is called.

   procedure Pause
     (This : in out WM8994_Audio_Device);
   --  After calling Pause, only Resume should be called for resuming play (do
   --  not call Start_Playing again).

   procedure Resume
     (This : in out WM8994_Audio_Device);
   --  Procedure Resume should be called only when the audio is playing or
   --  paused (not stopped).

   procedure Stop
     (This : in out WM8994_Audio_Device);
   --  Stops the hardware and update/play process. Once called, you must call
   --  Start_Playing again if you want to restart the output.

   procedure Set_Volume
     (This   : in out WM8994_Audio_Device;
      Volume : Audio_Volume);

   procedure Set_Frequency
     (This      : in out WM8994_Audio_Device;
      Frequency : Audio_Frequency);

private

   Audio_I2C_Addr  : constant I2C_Address := 16#34#;

   type WM8994_Audio_Device
     (Port : not null Any_I2C_Port)
   is tagged limited record
      Device : WM8994.WM8994_Device (Port, Audio_I2C_Addr, Ravenscar_Time.Delays);
      Sink   : Audio_Output_Device := No_Output;
   end record;

end Audio;
