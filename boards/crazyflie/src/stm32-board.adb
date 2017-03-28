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

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (This : in out User_LED)
   is
   begin
      if This = LED_Blue_L then
         Set (This);
      else
         Clear (This);
      end if;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (This : in out User_LED)
   is
   begin
      if This = LED_Blue_L then
         Clear (This);
      else
         Set (This);
      end if;
   end Turn_Off;

   -----------
   -- Is_On --
   -----------

   function Is_On (This : User_LED) return Boolean
   is
   begin
      if This = LED_Blue_L then
         return Set (This);
      else
         return not Set (This);
      end if;
   end Is_On;

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Set (RG_LEDs);
      Clear (LED_Blue_L);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Clear (RG_LEDs);
      Set (LED_Blue_L);
   end All_LEDs_On;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (All_LEDs,
                    Config => Configuration);
      All_LEDs_Off;
   end Initialize_LEDs;

   -------------------------
   -- Initialize_I2C_GPIO --
   -------------------------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
   is
      Id     : constant I2C_Port_Id := As_Port_Id (Port);
      Points : constant GPIO_Points (1 .. 2) :=
                 (if Id = I2C_Id_1 then (PB6, PB7)
                  else (PA8, PC9));

   begin
      if Id = I2C_Id_2 then
         raise Unknown_Device with
           "This I2C_Port cannot be used on this board";
      end if;

      Enable_Clock (Points);
      Enable_Clock (Port);
      Reset (Port);

      Configure_Alternate_Function (Points, GPIO_AF_I2C2_4);
      Configure_IO (Points,
                    (Speed       => Speed_25MHz,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      Lock (Points);
   end Initialize_I2C_GPIO;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure Configure_I2C (Port : in out I2C_Port)
   is
   begin
      if not STM32.I2C.Port_Enabled (Port) then
         Configure
           (This => Port,
            Conf =>
              (Clock_Speed     => 400_000,
               Mode            => I2C_Mode,
               Duty_Cycle      => DutyCycle_16_9,
               Addressing_Mode => Addressing_Mode_7bit,
               Own_Address     => 0,
               others          => <>));
      end if;
   end Configure_I2C;

end STM32.Board;
