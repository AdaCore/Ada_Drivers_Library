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

generic
   type Categories is (<>);
   --  Logging categories
   --  Typicaly an enum type, example:
   --     type My_Log_Categories is (Debug, Warning, Error);

   type Priorities is (<>);
   --  Logging priority type

   Default_Category : Categories;
   --  Category used when the category is not specified as parameter

   Default_Priority : Priorities := Priorities'First;
   --  Priority value set at the initialization

   Categories_Enabled_By_Default : Boolean := True;
   --  If this value is true, all logging categories are enabled at
   --  initialization. Otherwise, they are all disabled.

   Prefix_Enabled_By_Default : Boolean := True;
   --  If this value is true, prefix is enabled for all categories at
   --  initialization. Otherwise, it is diabled for all.

   with procedure Log_Line_Backend (Str : String; Prio : Priorities);
   --  Backend procedure that will actually log the messages.
   --
   --  In case the throughput is limited - for instance on a serial port - this
   --  procedure is responsible for printing the higer priority messages first
   --  and potentially discard the lower priority message.
package Logging_With_Categories is

   procedure Log_Line (Cat : Categories; Str : String);

   procedure Log_Line (Str : String);
   --  Log under the Default_Category

   ----------------------
   -- Category Control --
   ----------------------

   function Enabled (Cat : Categories) return Boolean;
   --  Is logging enabled for the given category?

   procedure Enable (Cat : Categories)
     with Post => Enabled (Cat);
   --  Enable logging for the given category.
   --  Messages for this category will printed using the Log_Line_Backend
   --  procedure.

   procedure Disable (Cat : Categories)
     with Post => not Enabled (Cat);
   --  Disable logging for the given category.
   --  Messages for this category will be ignored.

   --------------------
   -- Prefix Control --
   --------------------

   --  Insert the name of the category in front of the log messages.
   --  The format is: Catergory'Image (Cat) & ": " & Str
   --  This feature works best when the Categories type is an enum and when
   --  'Image is available for enum type (native or ravenscar-full run-times).

   procedure Enable_Prefix (Cat : Categories);
   --  Enable the prefix for the given category

   procedure Disable_Prefix (Cat : Categories);
   --  Disable the prefix for the given category

   ----------------------
   -- Priority Control --
   ----------------------

   function Priority (Cat : Categories) return Priorities;
   --  Return the proirity associated with the given category

   procedure Set_Priority (Cat : Categories; Prio : Priorities)
     with Post => Priority (Cat) = Prio;
   --  Set the proirity associated with the given category.
   --  The value is just passed to the Log_Line procedure, there is no
   --  filtering or sorting in this implementation.

end Logging_With_Categories;
