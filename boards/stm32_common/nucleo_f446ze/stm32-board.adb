------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2019, AdaCore                           --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4_discovery.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief  This file provides set of firmware functions to manage Leds    --
--           and push-button available on STM32F42-Discovery Kit from       --
--           STMicroelectronics.                                            --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

package body STM32.Board is

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Clear (All_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Set (All_LEDs);
   end All_LEDs_On;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
   begin
      Enable_Clock (All_LEDs);

      Configure_IO
        (All_LEDs,
         (Mode_Out,
          Resistors   => Floating,
          Output_Type => Push_Pull,
          Speed       => Speed_100MHz));
   end Initialize_LEDs;

   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------

   procedure Configure_User_Button_GPIO is
   begin
      Enable_Clock (User_Button_Point);
      Configure_IO (User_Button_Point, (Mode_In, Resistors => Floating));
   end Configure_User_Button_GPIO;

end STM32.Board;
