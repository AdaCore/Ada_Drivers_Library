with SAM.Device;  use SAM.Device;
with SAM.GPIO;    use SAM.GPIO;

--  with HAL.GPIO; use HAL.GPIO;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time; use Ada.Real_Time;

procedure Demo_GPIO is

   LED0 : GPIO_Point renames PA23;
   LED1 : GPIO_Point renames PC9;

   procedure Initialize_LEDs is
   begin
      Enable_Clock (GPIO_A);
      Enable_Clock (GPIO_B);

      LED0.Configure_IO
        (Config => GPIO_Port_Configuration'(Mode           => Mode_Out,
                                            Resistors      => <>,
                                            Output_Type    => Push_Pull));
      LED1.Configure_IO
        (Config => GPIO_Port_Configuration'(Mode           => Mode_Out,
                                            Resistors      => <>,
                                            Output_Type    => Push_Pull));
   end Initialize_LEDs;

   procedure Wait (Period : Time_Span)
   is
   begin
      delay until (Period + Clock);
   end Wait;

   procedure Flip_Coin
   is
      type Coin is (Heads, Tails);

      package Random_Coin is new Ada.Numerics.Discrete_Random (Coin);
      use Random_Coin;
      G : Generator;
   begin
      Reset (G);
      loop
         --  simulate flipping
         for I in 1 .. 10 loop
            LED0.Set;
            LED1.Clear;
            Wait (Period => Milliseconds (50));
            LED0.Clear;
            LED1.Set;
            Wait (Period => Milliseconds (50));
         end loop;

         --  Clear LEDS and delay
         LED0.Clear;
         LED1.Clear;
         Wait (Period => Milliseconds (300));

         --  check result
         case Random (G) is
            when Heads =>
               LED0.Set;
               LED1.Clear;
            when Tails =>
               LED0.Clear;
               LED1.Set;
         end case;

         -- delay and repeat
         Wait (Period => Milliseconds (2000));
      end loop;
   end Flip_Coin;

begin
   Initialize_LEDs;
   Flip_Coin;
end Demo_GPIO;
