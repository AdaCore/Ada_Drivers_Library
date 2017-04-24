------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with System;

--  Mailboxes facilitate communication between the ARM and the VideoCore.
--  The mechanism is generic to the BCM SoC, but the actual messages depend
--  on the actual firmware loaded on the GPU.
package RPi.Mailbox is

   type Mailbox_Channel is range 0 .. 9;
   Power_Management_Channel : constant Mailbox_Channel := 0;
   Framebuffer_Channel      : constant Mailbox_Channel := 1;
   Virtual_UART_Channel     : constant Mailbox_Channel := 2;
   VCHIQ_Channel            : constant Mailbox_Channel := 3;
   LEDs_Channel             : constant Mailbox_Channel := 4;
   Buttons_Channel          : constant Mailbox_Channel := 5;
   Touch_Screen_Channel     : constant Mailbox_Channel := 6;
   Property_Tags_ARM_To_VC  : constant Mailbox_Channel := 8;
   Property_Tags_VC_To_ARM  : constant Mailbox_Channel := 9;

   procedure Mailbox_Write (Val : UInt32; Channel : Mailbox_Channel);
   procedure Mailbox_Write (Addr : System.Address; Channel : Mailbox_Channel)
     with Inline_Always;

   function Mailbox_Read (Channel : Mailbox_Channel) return UInt32;

end RPi.Mailbox;
