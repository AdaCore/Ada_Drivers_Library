with Ada.Text_IO;
with Logging_With_Categories;

procedure TC_Log_Filtering is
   type Categories is (Debug, Warning, Error);

   procedure Print (Str : String; Prio : Natural);

   package Log is new Logging_With_Categories
     (Categories                    => Categories,
      Priorities                    => Natural,
      Default_Category              => Error,
      Default_Priority              => 0,
      Categories_Enabled_By_Default => False,
      Prefix_Enabled_By_Default     => False,
      Log_Line_Backend              => Print);

   procedure Print (Str : String; Prio : Natural) is
   begin
      Ada.Text_IO.Put_Line ("Prio:" & Prio'Img & " -> " & Str);
   end Print;

begin
   Ada.Text_IO.Put_Line ("--- Log test begin ---");

   --  All categories should be disabled so no message is expected here
   Log.Log_Line (Debug,   "Don't print this");
   Log.Log_Line (Warning, "Don't print this");
   Log.Log_Line (Error,   "Don't print this");

   --  Enabled categories one by one and print something
   Log.Enable (Debug);
   Log.Log_Line (Debug,   "This Debug should print");
   Log.Log_Line (Warning, "Don't print this");
   Log.Log_Line (Error,   "Don't print this");

   Log.Enable (Warning);
   Log.Log_Line (Warning, "This Warning should print");
   Log.Log_Line (Error,   "Don't print this");

   Log.Enable (Error);
   Log.Log_Line (Error,   "This Error should print");

   --  Change priorities
   Log.Set_Priority (Debug, 1);
   Log.Log_Line (Debug,   "This Debug should print prio 1");
   Log.Set_Priority (Warning, 2);
   Log.Log_Line (Warning, "This Warning should print prio 2");
   Log.Set_Priority (Error, 3);
   Log.Log_Line (Error,   "This Error should print prio 3");

   --  Enabled prefixes one by one and print something
   Log.Enable_Prefix (Debug);
   Log.Log_Line (Debug,   "This Debug should print with prefix");
   Log.Log_Line (Warning, "This Warning should print without prefix");
   Log.Log_Line (Error,   "This Error should print without prefix");

   Log.Enable_Prefix (Warning);
   Log.Log_Line (Warning, "This Warning should print with prefix");
   Log.Log_Line (Error,   "This Error should print without prefix");

   Log.Enable_Prefix (Error);
   Log.Log_Line (Error,   "This Error should print with prefix");

   Log.Log_Line ("This should print in the Error category");

   Ada.Text_IO.Put_Line ("--- Log test end ---");
end TC_Log_Filtering;
