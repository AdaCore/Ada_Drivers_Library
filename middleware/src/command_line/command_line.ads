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

package Command_Line is

   type Put_Procedure is not null access procedure (Str : String);
   type Put_Line_Procedure is not null access procedure (Str : String);

   ----------------------------
   -- Command line arguments --
   ----------------------------

   type Arguments (Number_Of_Args : Natural) is tagged private;

   function Next (This : in out Arguments) return String;
   function Remaining (This : Arguments) return Natural;

   type String_Access is access constant String;

   function Create (Line : not null String_Access) return Arguments;

   -------------
   -- Command --
   -------------

   type Command is abstract tagged limited private;

   type Any_Command is access all Command'Class;

   function Name (This : Command)
                  return String is abstract;

   procedure Execute (This     : in out Command;
                      Args     : in out Arguments'Class;
                      Put      : Put_Procedure;
                      Put_Line : Put_Line_Procedure) is abstract;

   procedure Help (This     : in out Command;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure) is abstract;

   ---------
   -- Run --
   ---------

   procedure Run (Line     : aliased String;
                  Put      : Put_Procedure;
                  Put_Line : Put_Line_Procedure);

   procedure Register (Cmd : Any_Command);

private

   ----------------------------
   -- Command line arguments --
   ----------------------------

   type String_Delimiters is record
      From, To : Natural;
   end record;

   type String_Delimiters_Array is array (Natural range <>) of String_Delimiters;

   type Arguments (Number_Of_Args : Natural) is tagged record
      Delimiters : String_Delimiters_Array (1 .. Number_Of_Args);
      Str        : not null String_Access;
      Current    : Natural;
   end record;

   -------------
   -- Command --
   -------------

   type Command is abstract tagged limited record
      Next : Any_Command := null;
   end record;

   --------------
   -- Builtins --
   --------------

   -- Echo --

   type Echo_Cmd is new Command with null record;

   overriding
   function Name (This : Echo_Cmd) return String
   is ("echo");

   overriding
   procedure Execute (This     : in out Echo_Cmd;
                      Args     : in out Arguments'Class;
                      Put      : Put_Procedure;
                      Put_Line : Put_Line_Procedure);
   overriding
   procedure Help (This     : in out Echo_Cmd;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure);

   -- Help --

   type Help_Cmd is new Command with null record;

   overriding
   function Name (This : Help_Cmd) return String
   is ("help");

   overriding
   procedure Execute (This     : in out Help_Cmd;
                      Args     : in out Arguments'Class;
                      Put      : Put_Procedure;
                      Put_Line : Put_Line_Procedure);
   overriding
   procedure Help (This     : in out Help_Cmd;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure);

end Command_Line;
