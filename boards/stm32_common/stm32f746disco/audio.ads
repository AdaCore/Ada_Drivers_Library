------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2016-2026, AdaCore                        --
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

with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;
with Interfaces; use Interfaces;
with WM8994;

private with Ravenscar_Time;

package Audio is

   type WM8994_Audio_CODEC (Port : not null Any_I2C_Port) is
     tagged limited private;

   type Audio_Outputs is new WM8994.Analog_Outputs with
     Static_Predicate => Audio_Outputs in
       Headphone | Speaker | Both;

   type Audio_Frequency is new WM8994.Audio_Frequency;

   type Audio_Volume is range 0 .. 100; -- a percentage

   type Audio_Buffer is array (Natural range <>) of Integer_16
     with Component_Size => 16, Alignment => 2;

   procedure Initialize
     (This      : in out WM8994_Audio_CODEC;
      Volume    : Audio_Volume;
      Frequency : Audio_Frequency;
      Sink      : Audio_Outputs);
   --  This routine initializes the hardware and configures the volume,
   --  sampling frequency, and output device (the sink). This routine must be
   --  called, before any others. The routines for setting the volume and
   --  the output frequency are optional.

   procedure Play
     (This   : in out WM8994_Audio_CODEC;
      Buffer : Audio_Buffer);
   --  Start playing content from the specified buffer. The effect is to tell
   --  the underlying WM8994 CODEC where the buffer to be played is located,
   --  and cause the CODEC to start playing the contents.
   --
   --  NB: playing continues after the call returns. An additional mechanism,
   --  outside this package, updates the content of the buffer while the CODEC
   --  is playing it. That update/play process continues until either there is
   --  no more music to be played, or Stop or Pause is called.

   procedure Pause
     (This : in out WM8994_Audio_CODEC);
   --  After calling Pause, only Resume should be called for resuming play (do
   --  not call Start_Playing again).

   procedure Resume
     (This : in out WM8994_Audio_CODEC);
   --  Procedure Resume should be called only when the audio is playing or
   --  paused (not stopped).

   procedure Stop
     (This : in out WM8994_Audio_CODEC);
   --  Stops the hardware and update/play process. Once called, you must call
   --  Start_Playing again if you want to restart the output.

   procedure Set_Volume
     (This   : in out WM8994_Audio_CODEC;
      Volume : Audio_Volume);

   procedure Set_Frequency
     (This      : in out WM8994_Audio_CODEC;
      Frequency : Audio_Frequency);

private

   Audio_I2C_Addr  : constant I2C_Address := 16#34#;

   type WM8994_Audio_CODEC
     (Port : not null Any_I2C_Port)
   is tagged limited record
      Device : WM8994.Audio_CODEC (Port, Audio_I2C_Addr, Ravenscar_Time.Delays);
      Sink   : WM8994.Analog_Outputs := WM8994.No_Output;
      --  The initial value of Sink is overwritten by Initialize. The value
      --  No_Output will trigger a C_E if ever referenced, so it is used as a
      --  check that Initialize has been called. We need the component Sink
      --  itself for the sake of a clean parameter profile for Set_Frequency,
      --  otherwise clients would have to pass another parameter to specify
      --  the output device selection again (after having done so when calling
      --  Initialize). That's because Set_Frequency needs to do enough hardware
      --  re-initialization to accommodate the new frequency, but doing so
      --  requires the output device selection so that the re-init can select
      --  the active slots. Just activating all slots (as is done in the STM
      --  C code) doesn't work (at least in the current Ada code).
   end record;

end Audio;
