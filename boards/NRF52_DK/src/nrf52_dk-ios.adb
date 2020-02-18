------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2020, AdaCore                      --
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

with HAL;                         use HAL;

with nRF.ADC;                   use nRF.ADC;
with nRF.PPI;                   use nRF.PPI;
with nRF.Timers;                use nRF.Timers;
with nRF.GPIO.Tasks_And_Events; use nRF.GPIO.Tasks_And_Events;
with nRF.Events;                use nRF.Events;
with nRF.Interrupts;            use nRF.Interrupts;

package body NRF52_DK.IOs is

   --  The analog out feature is implemented as PWM signal. To generate the PWM
   --  signals we use a timer with the configuration described bellow.
   --
   --  Because of the limited number of timer comparators and GPIOTE channels,
   --  we can only have 3 PWMs on the system at the same time. However there
   --  are 5 pins allowed to use PWM, so we need to dynamicaly allocate the
   --  PWM based on user requests.
   --
   --  Timer configuration:
   --
   --  Comparator 0, 1, 2 are used to control the pulse width of the 3 PWMs.
   --  Each of those comparator is associated with a PWM and a pin. When the
   --  timer counter reaches the value of a comparator, the associated pin
   --  toggles.
   --
   --  Comparator 3 is use to control the period. When the timer counter reaches
   --  its value, all pins toggle.
   --
   --  Comparator 3 also trigger an interrupt. In the handler for this
   --  interrupt, we update all the comparator values and start the timer again.
   --
   --
   --  Int handler and timer start    Cmp 0  Cmp 1  Cmp 2     Cmp3, Timer stop and interrupt
   --  v                              v      v      v         v
   --  _______________________________                         ____
   --                                 |_______________________|
   --  ______________________________________                  ____
   --                                        |________________|
   --  _____________________________________________           ____
   --                                               |_________|
   --
   --  ^------------------ Timer loop sequence -------------------^
   --
   --  Since all the timer events trigger a toggle of the pin, we have to make
   --  sure that the pin is at a good state (high) when starting the timer,
   --  otherwise the waveform could be inverted. This is why the GPIO channels
   --  are always configured when the timer is reconfigured.
   --
   --  PPI and GPIOTE:
   --
   --  To trigger a pin toggle from the timer compare events we use the
   --  following configuation.
   --
   --  Two PPI channels are used for each PWM pin. For a PWM X, one PPI channel
   --  is used to trigger a GPIOTE task on comparator X event, a second PPI
   --  channel is used to trigger a GPIOTE event on comparator 3 event. So
   --  the comparator 3 event is used by all PWMs.
   --
   --  For a PWM X, GPIOTE channel X is configure to do a pin toggle when its
   --  task is activated by one of the two PPI channels described above.

   --  We keep track of the current mode of the pin to be able to detect when a
   --  change of configuration is needed.
   type Pin_Mode is (None, Digital_In, Digital_Out, Analog_In, Analog_Out);
   Current_Mode : array (Pin_Id) of Pin_Mode := (others => None);

   -- PWM --

   Number_Of_PWMs : constant := 3;

   type PWM_Allocated is range 0 .. Number_Of_PWMs;
   subtype PWM_Id is PWM_Allocated range 0 .. Number_Of_PWMs - 1;

   No_PWM : constant PWM_Allocated := Number_Of_PWMs;

   PWM_Alloc : array (Pin_Id) of PWM_Allocated := (others => No_PWM);

   PWM_Timer          : Timer renames Timer_0;
   PWM_Interrupt      : constant Interrupt_Name := TIMER0_Interrupt;
   PWM_Global_Compare : constant Timer_Channel := 3;
   PWM_Precision      : constant := 4;
   PWM_Period         : UInt32 := 2_000 / PWM_Precision;

   type PWM_Status is record
      Taken       : Boolean := False;
      Pulse_Width : Analog_Value;
      Cmp         : UInt32 := 10;
      Pin         : Pin_Id;
   end record;

   PWMs : array (PWM_Id) of PWM_Status;

   function Has_PWM (Pin : Pin_Id) return Boolean
   is (PWM_Alloc (Pin) /= No_PWM);
   procedure Allocate_PWM (Pin     : Pin_Id;
                           Success : out Boolean)
     with Pre => not Has_PWM (Pin);
   procedure Deallocate_PWM (Pin : Pin_Id)
     with Pre  => Has_PWM (Pin),
          Post => not Has_PWM (Pin);
   procedure Configure_PPI (Id : PWM_Id);
   procedure Configure_GPIOTE (Id : PWM_Id);
   procedure Init_PWM_Timer;
   function To_Compare_Value (V : Analog_Value) return UInt32;
   procedure PWM_Timer_Handler;

   ----------------------
   -- To_Compare_Value --
   ----------------------

   function To_Compare_Value (V : Analog_Value) return UInt32
   is
      Cmp : constant UInt32 :=
        UInt32 (Float (PWM_Period) * (Float (V) / Float (Analog_Value'Last)));
   begin

      if Cmp = 0 then
         return 1;
      elsif Cmp >= PWM_Period then
         return PWM_Period - 1;
      else
         return Cmp;
      end if;
   end To_Compare_Value;

   ------------------
   -- Allocate_PWM --
   ------------------

   procedure Allocate_PWM (Pin     : Pin_Id;
                           Success : out Boolean)
   is
   begin
      for Id in PWM_Id loop
         if not PWMs (Id).Taken then
            PWMs (Id).Taken := True;
            PWMs (Id).Pin := Pin;
            PWM_Alloc (Pin) := Id;

            Configure_PPI (Id);

            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Allocate_PWM;

   --------------------
   -- Deallocate_PWM --
   --------------------

   procedure Deallocate_PWM (Pin : Pin_Id) is
   begin
      if PWM_Alloc (Pin) /= No_PWM then
         nRF.GPIO.Tasks_And_Events.Disable (GPIOTE_Channel (PWM_Alloc (Pin)));
         PWMs (PWM_Alloc (Pin)).Taken := False;
         PWM_Alloc (Pin) := No_PWM;
      end if;
   end Deallocate_PWM;

   -------------------
   -- Configure_PPI --
   -------------------

   procedure Configure_PPI (Id : PWM_Id) is
      Chan1 : constant Channel_ID := Channel_ID (Id) * 2;
      Chan2 : constant Channel_ID := Chan1 + 1;
   begin

      --  Use one PPI channel to triggerd GPTIOTE OUT task on the compare event
      --  associated with this PWM_Id;
      nRF.PPI.Configure
        (Chan    => Chan1,
         Evt_EP  => PWM_Timer.Compare_Event (Timer_Channel (Id)),
         Task_EP => Out_Task (GPIOTE_Channel (Id)));

      --  Use another PPI channel to triggerd GPTIOTE OUT task on compare 3 event
      nRF.PPI.Configure
        (Chan    => Chan2,
         Evt_EP  => PWM_Timer.Compare_Event (PWM_Global_Compare),
         Task_EP => Out_Task (GPIOTE_Channel (Id)));

      nRF.PPI.Enable_Channel (Chan1);
      nRF.PPI.Enable_Channel (Chan2);
   end Configure_PPI;

   ----------------------
   -- Configure_GPIOTE --
   ----------------------

   procedure Configure_GPIOTE (Id : PWM_Id) is
   begin
      --  Configure the GPIOTE OUT task to toggle the pin
      nRF.GPIO.Tasks_And_Events.Enable_Task
        (Chan          => GPIOTE_Channel (Id),
         GPIO_Pin      => Points (PWMs (Id).Pin).Pin,
         Action        => Toggle_Pin,
         Initial_Value => Init_Set);
   end Configure_GPIOTE;

   -----------------------
   -- PWM_Timer_Handler --
   -----------------------

   procedure PWM_Timer_Handler is
   begin
      Clear (PWM_Timer.Compare_Event (PWM_Global_Compare));

      PWM_Timer.Set_Compare (PWM_Global_Compare, PWM_Period);

      PWM_Timer.Set_Compare (0, PWMs (0).Cmp);
      PWM_Timer.Set_Compare (1, PWMs (1).Cmp);
      PWM_Timer.Set_Compare (2, PWMs (2).Cmp);

      PWM_Timer.Start;
   end PWM_Timer_Handler;

   --------------------
   -- Init_PWM_Timer --
   --------------------

   procedure Init_PWM_Timer is
   begin
      PWM_Timer.Set_Mode (Mode_Timer);
      PWM_Timer.Set_Prescaler (6);
      PWM_Timer.Set_Bitmode (Bitmode_32bit);

      --  Clear counter internal register and stop when timer reaches compare
      --  value 3.
      PWM_Timer.Compare_Shortcut (Chan  => PWM_Global_Compare,
                                  Stop  => True,
                                  Clear => True);

      PWM_Timer.Set_Compare (PWM_Global_Compare, PWM_Period);

      for Id in PWM_Id loop         PWM_Timer.Set_Compare (Timer_Channel (Id),
                                To_Compare_Value (PWMs (Id).Pulse_Width));
         if PWMs (Id).Taken then
            Configure_GPIOTE (Id);
         end if;

      end loop;

      Enable_Interrupt (PWM_Timer.Compare_Event (PWM_Global_Compare));

      nRF.Interrupts.Register (PWM_Interrupt,
                                 PWM_Timer_Handler'Access);

      nRF.Interrupts.Enable (PWM_Interrupt);
   end Init_PWM_Timer;

   ---------
   -- Set --
   ---------

   procedure Set
     (Pin : Pin_Id;
      Value : Boolean)
   is
      Pt   : GPIO_Point renames Points (Pin);
      Conf : GPIO_Configuration;
   begin
      if Current_Mode (Pin) /= Digital_Out then
         if Has_PWM (Pin) then
            Deallocate_PWM (Pin);
         end if;

         Conf.Mode         := Mode_Out;
         Conf.Resistors    := No_Pull;
         Conf.Input_Buffer := Input_Buffer_Connect;
         Conf.Sense        := Sense_Disabled;

         Pt.Configure_IO (Conf);
         Current_Mode (Pin) := Digital_Out;
      end if;

      if Value then
         Pt.Set;
      else
         Pt.Clear;
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   function Set
     (Pin : Pin_Id)
      return Boolean
   is
      Pt   : GPIO_Point renames Points (Pin);
      Conf : GPIO_Configuration;
   begin
      if Current_Mode (Pin) /= Digital_In then
         if Has_PWM (Pin) then
            Deallocate_PWM (Pin);
         end if;

         Conf.Mode         := Mode_In;
         Conf.Resistors    := No_Pull;
         Conf.Input_Buffer := Input_Buffer_Connect;
         Conf.Sense        := Sense_Disabled;

         Pt.Configure_IO (Conf);

         Current_Mode (Pin) := Digital_In;
      end if;

      return Pt.Set;
   end Set;

   --------------------------
   -- Set_Analog_Period_Us --
   --------------------------

   procedure Set_Analog_Period_Us (Period : Natural) is
   begin
      PWM_Period := UInt32 (Period) / PWM_Precision;

      --  Update the comparator values for ech PWM
      for PWM of PWMs loop
         PWM.Cmp := To_Compare_Value (PWM.Pulse_Width);
      end loop;
   end Set_Analog_Period_Us;

   -----------
   -- Write --
   -----------

   procedure Write
     (Pin : Pin_Id;
      Value : Analog_Value)
   is
      Success : Boolean;

      Pt   : GPIO_Point renames Points (Pin);
      Conf : GPIO_Configuration;
   begin

      if not Has_PWM (Pin) then

         --  Stop the timer while we configure a new pin

         PWM_Timer.Stop;
         PWM_Timer.Clear;

         Allocate_PWM (Pin, Success);
         if not Success then
            raise Program_Error with "No PWM available";
         end if;

         --  Set the pin as output
         Conf.Mode         := Mode_Out;
         Conf.Resistors    := No_Pull;
         Conf.Input_Buffer := Input_Buffer_Connect;
         Conf.Sense        := Sense_Disabled;

         Pt.Configure_IO (Conf);
         Pt.Clear;

         Current_Mode (Pin) := Analog_Out;

         Init_PWM_Timer;

         PWM_Timer.Start;
      end if;

      PWMs (PWM_Alloc (Pin)).Pulse_Width := Value;
      PWMs (PWM_Alloc (Pin)).Cmp := To_Compare_Value (Value);
   end Write;

   ------------
   -- Analog --
   ------------

   function Analog
     (Pin : Pin_Id)
      return Analog_Value
   is
      Result : UInt16;
   begin
      if Current_Mode (Pin) /= Analog_In then
         if Has_PWM (Pin) then
            Deallocate_PWM (Pin);
         end if;
         Current_Mode (Pin) := Analog_In;
      end if;

      Result := Do_Pin_Conversion (Pin   => (case Pin is
                                         when 0      => 4,
                                         when 1      => 3,
                                         when 2      => 2,
                                         when 3      => 5,
                                         when 4      => 6,
                                         when 10     => 7,
                                         when others => 0),
                            Input => Pin_One_Forth,
                            Ref   => VDD_One_Forth,
                            Res   => Res_10bit);
      return Analog_Value (Result);
   end Analog;

end NRF52_DK.IOs;
