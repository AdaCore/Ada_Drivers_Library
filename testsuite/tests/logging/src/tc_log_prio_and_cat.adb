with Ada.Text_IO;
with Logging;

procedure TC_Log_Prio_And_Cat is

   type Categories is (Debug, Warning, Error);

   Maximum_Message_Length : constant := 64;

   package Log is new Logging
     (Categories                    => Categories,
      Priorities                    => Natural,
      Default_Category              => Debug,
      Default_Priority              => 0,
      Categories_Enabled_By_Default => True,
      Prefix_Enabled_By_Default     => True,
      Maximum_Message_Length        => Maximum_Message_Length,
      Maximum_Number_Of_Messages    => 6);

   procedure Pop_And_Print;

   -------------------
   -- Pop_And_Print --
   -------------------

   procedure Pop_And_Print is
      Str    : String (1 .. Maximum_Message_Length);
      Length : Natural;
      Prio   : Natural;
   begin
      Log.Pop (Str, Length, Prio);
      if Length /= 0 then
         Ada.Text_IO.Put_Line ("Prio:" & Prio'Img & " -> " &
                                 Str (Str'First .. Str'First + Length - 1));
      else
         Ada.Text_IO.Put_Line ("Pop : The queue is empty");
      end if;
   end Pop_And_Print;

begin
   Ada.Text_IO.Put_Line ("--- Log test begin ---");

   --  The priority and category features are already tested separatly so this
   --  test is just a simple check to see if the two work together.

   Log.Log_Line (Debug, "Debug, prio 0");
   Log.Disable (Debug);
   Log.Log_Line (Debug, "Debug, should not print");
   Log.Enable (Debug);
   Log.Set_Priority (Debug, 1);
   Log.Log_Line (Debug, "Debug, prio 1");

   Log.Log_Line (Warning, "Warning, prio 0");
   Log.Disable (Warning);
   Log.Log_Line (Warning, "Warning, should not print");
   Log.Enable (Warning);
   Log.Set_Priority (Warning, 2);
   Log.Log_Line (Warning, "Warning, prio 2");

   Log.Log_Line (Error, "Error, prio 0");
   Log.Disable (Error);
   Log.Log_Line (Error, "Error, should not print");
   Log.Enable (Error);
   Log.Set_Priority (Error, 3);
   Log.Log_Line (Error, "Error, prio 3");

   if not Log.Full then
      Ada.Text_IO.Put_Line ("The queue should be full");
   end if;

   for Cnt in 1 .. 7 loop
      Pop_And_Print;
   end loop;

   if not Log.Empty then
      Ada.Text_IO.Put_Line ("The queue should be empty");
   end if;

   Ada.Text_IO.Put_Line ("--- Log test end ---");
end TC_Log_Prio_And_Cat;
