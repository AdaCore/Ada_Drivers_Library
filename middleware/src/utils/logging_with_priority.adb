------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body Logging_With_Priority is

   subtype Message_Length is Natural range 1 .. Maximum_Message_Length;

   type Message is record
      Data : String (Message_Length);
      Len  : Message_Length;
      Free : Boolean := True;
      Prio : Priorities;
   end record;

   subtype Message_Id is Natural range 0 .. Maximum_Number_Of_Messages;
   subtype Valid_Message_Id is Message_Id range 1 .. Maximum_Number_Of_Messages;

   Invalid_Id : constant Message_Id := 0;

   Messages : array (Valid_Message_Id) of Message;
   --  The messages are statically allocated in the this array

   subtype Queue_Index is Valid_Message_Id;
   Queue : array (Queue_Index) of Message_Id := (others => Invalid_Id);
   --  This queue contain indexes to the allocated message array

   function Allocate (Prio : Priorities) return Message_Id;
   --  Allocate a message and return its ID.
   --  If there's no free message, this sub-program will try to discard a lower
   --  priority message to make room.
   --
   --  Return Invalid_Id if the allocation is not possible, i.e. all messages
   --  are allocated with higher or equal priority.

   procedure Deallocate (Id : in out Message_Id);

   procedure Insert_Fifo_Within_Priorities (Id   : Valid_Message_Id;
                                            Prio : Priorities)
     with Pre => not Full;

   --------------
   -- Allocate --
   --------------

   function Allocate (Prio : Priorities) return Message_Id is
      Ret_Id : Message_Id;
   begin
      --  Naive implementation...

      for Id in Valid_Message_Id loop
         if Messages (Id).Free then
            Messages (Id).Free := False;
            return Id;
         end if;
      end loop;

      --  Since we cannot find a free message it means the queue is full. We
      --  will try to remove a message with lower priority.
      --  The last message of the queue should be of the lower priority.

      if Messages (Queue (Queue'Last)).Prio < Prio then

         Ret_Id := Queue (Queue'Last);

         --  Discard the last message in the queue
         Deallocate (Queue (Queue'Last));

         Messages (Ret_Id).Free := False;
         return Ret_Id;
      end if;
      return Invalid_Id;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Id : in out Message_Id) is
   begin
      if Id /= Invalid_Id then
         Messages (Id).Free := True;
         Id := Invalid_Id;
      end if;
   end Deallocate;

   -----------------------------------
   -- Insert_Fifo_Within_Priorities --
   -----------------------------------

   procedure Insert_Fifo_Within_Priorities (Id   : Valid_Message_Id;
                                            Prio : Priorities)
   is
      Index     : Queue_Index;
      Insert_At : Queue_Index;
   begin

      --  Let's Find the first null or message with a lower priority

      Index := Queue'First;

      loop
         exit when Queue (Index) = Invalid_Id
           or else
             Messages (Queue (Index)).Prio < Prio
           or else
             Index = Queue'Last;

         Index := Index + 1;
      end loop;

      Insert_At := Index;

      if Insert_At /= Queue'Last then
         --  Shift the lower prio messages
         Index := Queue'Last;
         loop
            Queue (Index) := Queue (Index - 1);
            Index := Index - 1;
            exit when Index = Insert_At;
         end loop;
      end if;

      Queue (Insert_At) := Id;
   end Insert_Fifo_Within_Priorities;

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line (Str : String; Prio : Priorities) is
      Id : Message_Id;
   begin
      if Str'Length > Maximum_Message_Length then
         return;
      end if;

      Id := Allocate (Prio);

      if Id = Invalid_Id then
         --  No more room available, this message is discarded
         return;
      else
         declare
            Msg : Message renames Messages (Id);
         begin
            Msg.Len := Str'Length;
            Msg.Data (Msg.Data'First .. Msg.Data'First + Msg.Len - 1) :=
              Str (Str'First .. Str'First + Msg.Len - 1);
            Msg.Prio := Prio;
            Insert_Fifo_Within_Priorities (Id, Prio);
         end;
      end if;
   end Log_Line;

   ---------
   -- Pop --
   ---------

   procedure Pop (Str    : out String;
                  Length : out Natural;
                  Prio   : out Priorities)
   is
      Top_Id : Message_Id := Queue (Queue'First);
   begin
      if Top_Id /= Invalid_Id then
         declare
            Msg : Message renames Messages (Top_Id);
         begin
            --  Copy the string
            Str (Str'First .. Str'First + Msg.Len - 1) :=
              Msg.Data (Msg.Data'First .. Msg.Data'First + Msg.Len - 1);

            Length := Msg.Len;

            Prio := Msg.Prio;

            Deallocate (Top_Id);

            --  Shift all message IDs
            for Index in Queue'First .. Queue'Last - 1 loop
               Queue (Index) := Queue (Index + 1);
            end loop;
            Queue (Queue'Last) := Invalid_Id;
         end;
      else
         Length := 0;
      end if;
   end Pop;

   ----------
   -- Full --
   ----------

   function Full return Boolean is
   begin
      --  If the last element is not an invalid message ID, the queue is not
      --  full.
      return Queue (Queue'Last) /= Invalid_Id;
   end Full;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      --  If the first element is an invalid message ID, the queue is empty
      return Queue (Queue'First) = Invalid_Id;
   end Empty;

end Logging_With_Priority;
