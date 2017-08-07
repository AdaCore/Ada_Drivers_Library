------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

package body Command_Line is

   Echo_Instance : aliased Echo_Cmd := Echo_Cmd'(Next => null);
   Help_Instance : aliased Help_Cmd := Help_Cmd'(Next => Echo_Instance'Access);
   Command_List : Any_Command := Help_Instance'Access;

   Empty_Delim_Array : constant String_Delimiters_Array (1 .. 0) :=
     (others => (0, 0));

   function Make_Delims (Input : String) return String_Delimiters_Array;

   -----------------
   -- Make_Delims --
   -----------------

   function Make_Delims (Input : String) return String_Delimiters_Array is
      Delim : String_Delimiters;
      Index : Natural := Input'First;
   begin
      --  Skip all whitespaces
      while Index in Input'Range and then Input (Index) = ' ' loop
         Index := Index + 1;
      end loop;

      if Index not in Input'Range then
         return Empty_Delim_Array;
      end if;

      Delim.From := Index;

      --  Eat all the non whitespaces characters
      while Index in Input'Range and then Input (Index) /= ' ' loop
         Index := Index + 1;
      end loop;

      Delim.To := Index - 1;

      if Delim.To < Delim.From then
         return Empty_Delim_Array;
      end if;

      return Delim & Make_Delims (Input ((Index) .. Input'Last));
   end Make_Delims;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (This     : in out Echo_Cmd;
                      Args     : in out Arguments'Class;
                      Put      : Put_Procedure;
                      Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (This);
      First    : Boolean := True;
      New_Line : Boolean := True;
   begin
      loop
         declare
            Arg : constant String := Args.Next;
         begin
            exit when Arg'Length = 0;
            if First and then Arg = "-n" then
               New_Line := False;
            else
               if First then
                  Put (Arg);
                  First := False;
               else
                  Put (" " & Arg);
               end if;
            end if;
         end;
      end loop;
      if New_Line then
         Put_Line ("");
      end if;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding
   procedure Help (This     : in out Echo_Cmd;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (This, Put);
   begin
      Put_Line ("Usage: echo [-n] [STRING]...");
      Put_Line ("Display text.");
      Put_Line (" -n : Do no output the trailing newline");
   end Help;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (This     : in out Help_Cmd;
                      Args     : in out Arguments'Class;
                      Put      : Put_Procedure;
                      Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (This);
      Cmd_Driver : Any_Command := Command_List;
   begin
      if Args.Remaining = 0 then

         --  No arguments, display the list of commands

         Put_Line ("Here is the list of commands available:");
         while Cmd_Driver /= null loop
            Put_Line (" - " & Cmd_Driver.Name);
            Cmd_Driver := Cmd_Driver.Next;
         end loop;
         Put_Line ("Use 'help <command name>' for more information.");
      else
         loop
            declare
               Arg : constant String := Args.Next;
            begin
               exit when Arg'Length = 0;
               Cmd_Driver := Command_List;

               while Cmd_Driver /= null and then Cmd_Driver.Name /= Arg loop
                  Cmd_Driver := Cmd_Driver.Next;
               end loop;

               if Cmd_Driver /= null then
                  Cmd_Driver.Help (Put, Put_Line);
               else
                  Put_Line ("Unknown command: '" & Arg & "'");
               end if;
            end;
         end loop;
      end if;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding
   procedure Help (This     : in out Help_Cmd;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure)
   is
      Empty : aliased constant String := "";
      Args : Arguments := (Number_Of_Args => 0,
                           Delimiters     => Empty_Delim_Array,
                           Current        => 0,
                           Str            => Empty'Unchecked_Access);
   begin
      This.Execute (Args, Put, Put_Line);
   end Help;

   ----------
   -- Next --
   ----------

   function Next (This : in out Arguments) return String is
      Index : Natural;
   begin
      if This.Current in This.Delimiters'Range then
         Index := This.Current;
         This.Current := This.Current + 1;
         return This.Str (This.Delimiters (Index).From .. This.Delimiters (Index).To);
      else
         return "";
      end if;
   end Next;

   ---------------
   -- Remaining --
   ---------------

   function Remaining (This : Arguments) return Natural is
   begin
      if This.Current in This.Delimiters'Range then
         return This.Delimiters'Last - This.Current + 1;
      else
         return 0;
      end if;
   end Remaining;

   ------------
   -- Create --
   ------------

   function Create (Line : not null String_Access) return Arguments is
      Arr : constant String_Delimiters_Array := Make_Delims (Line.all);
      Ret : Arguments := Arguments'(Number_Of_Args => Arr'Length,
                                    Delimiters     => Arr,
                                    Str            => Line,
                                    Current        => Arr'First);
   begin
      Ret.Current := Ret.Delimiters'First;
      return Ret;
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (Line     : aliased String;
      Put      : Put_Procedure;
      Put_Line : Put_Line_Procedure)
   is
      Args       : Arguments := Create (Line'Unchecked_Access);
      Cmd        : constant String := Args.Next;
      Cmd_Driver : Any_Command := Command_List;
   begin
      if Cmd'Length = 0 then
         return;
      end if;

      while Cmd_Driver /= null loop
         if Cmd_Driver.Name = Cmd then
            exit;
         else
            Cmd_Driver := Cmd_Driver.Next;
         end if;
      end loop;

      if Cmd_Driver = null then
         Put_Line ("Unknown command: '" & Cmd & "'");
      end if;

      if Cmd_Driver = null then
         return;
      end if;

      Cmd_Driver.Execute (Args, Put, Put_Line);
   end Run;

   --------------
   -- Register --
   --------------

   procedure Register (Cmd : Any_Command) is
   begin
      Cmd.Next := Command_List;
      Command_List := Cmd;
   end Register;

end Command_Line;
