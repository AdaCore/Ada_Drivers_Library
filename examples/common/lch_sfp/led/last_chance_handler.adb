
--  This version of the LCH only toggles the Red LED.

--  Note this version is for use with the ravenscar-sfp runtime.

with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

package body Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);
   begin
      Initialize_LEDs;

      All_LEDs_Off;

      --  No-return procedure...
      loop
         Toggle (LCH_LED);
         delay until Clock + Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
