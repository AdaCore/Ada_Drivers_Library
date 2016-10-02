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

package nRF51.GPIO.Tasks_And_Events is

   type GPIOTE_Channel is range 0 .. 3;

   procedure Disable (Chan : GPIOTE_Channel);

   type Event_Polarity is (Rising_Edge, Falling_Edge, Any_Change);

   procedure Enable_Event (Chan     : GPIOTE_Channel;
                           GPIO_Pin : GPIO_Pin_Index;
                           Polarity : Event_Polarity);
   --  When GPIO_Pin value changes the event associated with Chan is raised

   type Task_Action is (Set_Pin, Clear_Pin, Toggle_Pin);
   type Init_Value is (Init_Set, Init_Clear);

   procedure Enable_Task (Chan          : GPIOTE_Channel;
                          GPIO_Pin      : GPIO_Pin_Index;
                          Action        : Task_Action;
                          Initial_Value : Init_Value);
   --  When the tasks associated with Chan is triggered, Action (Set, Clear,
   --  Toggle) is applied to GPIO_Pin.

end nRF51.GPIO.Tasks_And_Events;
