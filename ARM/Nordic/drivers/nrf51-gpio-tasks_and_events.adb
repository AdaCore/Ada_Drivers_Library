------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with NRF51_SVD.GPIOTE; use NRF51_SVD.GPIOTE;
with HAL;              use HAL;

package body nRF51.GPIO.Tasks_And_Events is

   -------------
   -- Disable --
   -------------

   procedure Disable (Chan : GPIOTE_Channel) is
   begin
      GPIOTE_Periph.CONFIG (Integer (Chan)).MODE := Disabled;
   end Disable;

   ------------------
   -- Enable_Event --
   ------------------

   procedure Enable_Event
     (Chan     : GPIOTE_Channel;
      GPIO_Pin : GPIO_Pin_Index;
      Polarity : Event_Polarity)
   is
      CONFIG : CONFIG_Register renames GPIOTE_Periph.CONFIG (Integer (Chan));
   begin
      CONFIG.PSEL := UInt5 (GPIO_Pin);
      CONFIG.POLARITY := (case Polarity is
                          when Rising_Edge  => Lotohi,
                          when Falling_Edge => Hitolo,
                          when Any_Change   => Toggle);
      CONFIG.MODE := Event;
   end Enable_Event;

   -----------------
   -- Enable_Task --
   -----------------

   procedure Enable_Task
     (Chan          : GPIOTE_Channel;
      GPIO_Pin      : GPIO_Pin_Index;
      Action        : Task_Action;
      Initial_Value : Init_Value)
   is
      CONFIG : CONFIG_Register renames GPIOTE_Periph.CONFIG (Integer (Chan));
   begin
      CONFIG.PSEL := UInt5 (GPIO_Pin);
      CONFIG.POLARITY := (case Action is
                          when Set_Pin    => Lotohi,
                          when Clear_Pin  => Hitolo,
                             when Toggle_Pin => Toggle);
      CONFIG.OUTINIT := (case Initial_Value is
                            when Init_Set => High,
                            when Init_Clear => Low);
      CONFIG.MODE := Task_k;
   end Enable_Task;

end nRF51.GPIO.Tasks_And_Events;
