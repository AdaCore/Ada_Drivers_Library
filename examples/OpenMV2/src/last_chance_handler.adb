with OpenMV; use OpenMV;

package body Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);
      procedure Busy_loop;

      ---------------
      -- Busy_loop --
      ---------------

      procedure Busy_loop is
      begin
         for Cnt in 1 .. 10_000_000 loop
            null;
         end loop;
      end Busy_loop;
   begin

      Initialize_LEDs;

      --  No-return procedure...
      loop
         Set_RGB_LED (Red);
         Busy_loop;
         Set_RGB_LED (Off);
         Busy_loop;
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
