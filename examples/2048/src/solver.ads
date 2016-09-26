
pragma Restrictions (No_Streams);

package Solver is

   Solver_Enabled : Boolean := False;

   type Move_Type is (Up, Down, Left, Right, None);
   subtype Valid_Move_Type is Move_Type range Up .. Right;

   procedure Init_Solver;

   function Next_Move return Move_Type;

end Solver;
