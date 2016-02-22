with LCD_Std_Out;

with STM32_SVD.SAI; use STM32_SVD.SAI;

package body Support is
   use STM32_SVD;

   Msg : String (1 .. 17) := (others => ' ');
   New_Msg : Boolean := False;

   ---------------
   -- Check_Msg --
   ---------------

   procedure Check_Msg is
   begin
      if New_Msg then
         LCD_Std_Out.Put_Line (Msg);
         Msg := (others => ' ');
         New_Msg := False;
      end if;
   end Check_Msg;

   -------------
   -- Set_Msg --
   -------------

   procedure Set_Msg (S : String) is
   begin
      if not New_Msg then
         Msg (1 .. S'Length) := S;
         New_Msg := True;
      end if;
   end Set_Msg;

   ----------------------
   -- On_Half_Complete --
   ----------------------

   procedure On_Half_Complete
   is
   begin
      Set_Msg ("Half complete");
   end On_Half_Complete;

   -----------------
   -- On_Complete --
   -----------------

   procedure On_Complete
   is
   begin
      Set_Msg ("Complete");
   end On_Complete;

   procedure On_Error (Err : DMA_Error)
   is
   begin
      case Err is
         when FIFO_Error =>
            Set_Msg ("FIFO Error");
         when Direct_Mode_Error =>
            Set_Msg ("Direct mode Error");
         when Transfer_Error =>
            Set_Msg ("Transfer Error");
      end case;
   end On_Error;

end Support;
