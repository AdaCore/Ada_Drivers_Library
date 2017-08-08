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

package body Logging_With_Categories is

   Is_Enabled : array (Categories) of Boolean :=
     (others => Categories_Enabled_By_Default);

   Cat_Priorities : array (Categories) of Priorities :=
     (others => Default_Priority);

   Prefix_Enabled : array (Categories) of Boolean :=
     (others => Prefix_Enabled_By_Default);

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line (Cat : Categories; Str : String) is
   begin
      if Is_Enabled (Cat) then
         if Prefix_Enabled (Cat) then
            Log_Line_Backend (Cat'Img & ": " & Str, Cat_Priorities (Cat));
         else
            Log_Line_Backend (Str, Cat_Priorities (Cat));
         end if;
      end if;
   end Log_Line;

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line (Str : String) is
   begin
      Log_Line (Default_Category, Str);
   end Log_Line;

   -------------
   -- Enabled --
   -------------

   function Enabled (Cat : Categories) return Boolean
   is (Is_Enabled (Cat));

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Cat : Categories)
   is
   begin
      Is_Enabled (Cat) := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (Cat : Categories)
   is
   begin
      Is_Enabled (Cat) := False;
   end Disable;

   -------------------
   -- Enable_Prefix --
   -------------------

   procedure Enable_Prefix (Cat : Categories) is
   begin
      Prefix_Enabled (Cat) := True;
   end Enable_Prefix;

   --------------------
   -- Disable_Prefix --
   --------------------

   procedure Disable_Prefix (Cat : Categories) is
   begin
      Prefix_Enabled (Cat) := False;
   end Disable_Prefix;

   --------------
   -- Priority --
   --------------

   function Priority (Cat : Categories) return Priorities
   is (Cat_Priorities (Cat));

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Cat : Categories;
      Prio : Priorities)
   is
   begin
      Cat_Priorities (Cat) := Prio;
   end Set_Priority;

end Logging_With_Categories;
