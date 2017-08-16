with Ada.Text_IO;
with Logging_With_Priority;

procedure TC_Log_Priorities is

   Maximum_Message_Length : constant := 64;

   package Log is new Logging_With_Priority
     (Priorities                 => Natural,
      Maximum_Message_Length     => Maximum_Message_Length,
      Maximum_Number_Of_Messages => 6);

   procedure Pop_And_Print;
   procedure Fill_Queue;
   procedure Empty_Queue;

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

   ----------------
   -- Fill_Queue --
   ----------------

   procedure Fill_Queue is
   begin
      Log.Log_Line ("Prio 1 - 1", 1);
      Log.Log_Line ("Prio 2 - 1", 2);
      Log.Log_Line ("Prio 5 - 1", 5);
      Log.Log_Line ("Prio 1 - 2", 1);
      Log.Log_Line ("Prio 5 - 2", 5);
      Log.Log_Line ("Prio 8 - 1", 8);

      if not Log.Full then
         raise Program_Error with "The queue should be full";
      end if;

   end Fill_Queue;

   -----------------
   -- Empty_Queue --
   -----------------

   procedure Empty_Queue is
   begin
      --  Empty the queue
      for Cnt in 1 .. 6 loop
         Pop_And_Print;
      end loop;

      if not Log.Empty then
         raise Program_Error with "The queue should be empty";
      end if;
   end Empty_Queue;


begin
   Ada.Text_IO.Put_Line ("--- Log test begin ---");

   declare
   begin
      Ada.Text_IO.Put_Line ("--- Test priorities ---");

      --  Try to print but there should be nothing in the queue
      Pop_And_Print;

      --  Insert a few messages with various priorities to check that the messages
      --  will be properly sorted.
      Fill_Queue;
      Empty_Queue;

      --  Try to print but there should be nothing in the queue
      Pop_And_Print;
   end;

   declare
   begin
      Ada.Text_IO.Put_Line ("--- Test insert lower prio in full queue ---");
      --  Insert a message with a prio below all the other priorities so it
      --  should be rejected.
      Fill_Queue;
      Log.Log_Line ("Prio 0 - This message should be discarded", 0);

      Empty_Queue;

      --  Try to print but there should be nothing in the queue
      Pop_And_Print;
   end;

   declare
   begin
      Ada.Text_IO.Put_Line ("--- Test insert low prio in full queue ---");

      --  Insert a message with a prio equal to the lowest but it should be
      --  rejected because there's no more room.
      Fill_Queue;
      Log.Log_Line ("Prio 1 - This message should be discarded", 1);

      Empty_Queue;

      --  Try to print but there should be nothing in the queue
      Pop_And_Print;
   end;

   declare
   begin
      Ada.Text_IO.Put_Line ("--- Test insert high prio in full queue ---");

      --  Insert a message with a prio above all the other priorities so it
      --  should be accepted in the queue.
      Fill_Queue;
      Log.Log_Line ("Prio 9 - This message should be accepted", 9);

      Empty_Queue;

      --  Try to print but there should be nothing in the queue
      Pop_And_Print;
   end;

   declare
      Str : constant String (1 .. Maximum_Message_Length + 10) := (others => 'a');
   begin
      Ada.Text_IO.Put_Line ("--- Test insert too long message ---");

      --  Insert a message with a length over the limit so it should be
      --  rejected.
      Fill_Queue;
      Log.Log_Line (Str, 9);

      Empty_Queue;

      --  Try to print but there should be nothing in the queue
      Pop_And_Print;
   end;

   Ada.Text_IO.Put_Line ("--- Log test end ---");
end TC_Log_Priorities;
