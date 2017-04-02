------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body Pathname_Manipulation is

   -----------
   -- Parse --
   -----------

   function Parse (Path : Pathname) return New_Path is
      From, To : Integer;
   begin
      if Path'Length = 0 then
         return Empty_Path;
      end if;

      --  Skip the first delimiter, if any
      if Path (Path'First) = '/' then
         From := Path'First + 1;
      else
         From := Path'First;
      end if;
      To := From - 1;

      for Index in From .. Path'Last loop

         --  Look for next delimiter or end of string

         if Path (Index) = '/' then
            To := Index - 1;
            exit;
         elsif Index = Path'Last then
            To := Index;
            exit;
         end if;
      end loop;

      declare
         Next : constant New_Path := Parse (Path (To + 1 .. Path'Last));
      begin
         if To < From then
            return Next;
         else
            declare
               Ret : New_Path := (String_Length => To - From + 1,
                                  Delimiters_Length => 1,
                                  Str => Path (From .. To),
                                  Delim => (others => (0, 0)));
            begin
               Ret.Delim (Ret.Delim'First) := (Ret.Str'First, Ret.Str'Last);
               return Ret & Next;
            end;
         end if;
      end;
   end Parse;

   ---------
   -- "&" --
   ---------

   function "&"(Left : New_Path; Right : New_Path) return New_Path is
   begin
      if Left.Delimiters_Length = 0 then
         return Right;
      elsif Right.Delimiters_Length = 0 then
         return Left;
      end if;

      declare
         Path : New_Path :=
           (String_Length     => Left.String_Length + Right.String_Length,
            Delimiters_Length => Left.Delimiters_Length + Right.Delimiters_Length,
            Str               => Left.Str & Right.Str,
            Delim             => Left.Delim & Right.Delim);
         L_Diff : constant Integer := Path.Str'First - Left.Str'First;
         R_Diff : constant Integer := Path.Str'First - Right.Str'First + Left.Str'Length;
      begin
                  --  Adjust indexes
         for Index in Path.Delim'First .. Path.Delim'First + Left.Delim'Length - 1 loop
            Path.Delim (Index).From := Path.Delim (Index).From + L_Diff;
            Path.Delim (Index).To := Path.Delim (Index).To + L_Diff;
         end loop;

         for Index in Path.Delim'First + Left.Delim'Length .. Path.Delim'Last loop
            Path.Delim (Index).From := Path.Delim (Index).From + R_Diff;
            Path.Delim (Index).To := Path.Delim (Index).To + R_Diff;
         end loop;
         return Path;
      end;
   end "&";

   -----------
   -- Slice --
   -----------

   function Slice (Path       : New_Path;
                   Index      : Integer)
                   return Pathname
   is
   begin
      if Index in Path.Delim'Range then
         return Path.Str (Path.Delim (Index).From .. Path.Delim (Index).To);
      else
         return "";
      end if;
   end Slice;

   -----------
   -- Parse --
   -----------

   function Parse (Path : Pathname) return Path_Delimiters is
      Empty : constant Path_Delimiters (1 .. 0) :=
        (others => Delimiter'(0, 0));
      From, To : Integer;
   begin
      if Path'Length = 0 then
         return Empty;
      end if;

      --  Skip the first delimiter, if any
      if Path (Path'First) = '/' then
         From := Path'First + 1;
      else
         From := Path'First;
      end if;
      To := From - 1;

      for Index in From .. Path'Last loop

         --  Look for next delimiter or end of string

         if Path (Index) = '/' then
            To := Index - 1;
            exit;
         elsif Index = Path'Last then
            To := Index;
            exit;
         end if;
      end loop;

      return (if To < From then Empty else (1 => Delimiter'(From, To))) &
        Parse (Path (To + 1 .. Path'Last));
   end Parse;

   --------------------
   -- Sub_Delimiters --
   --------------------

   function Sub_Delimiters (Delimiters : Path_Delimiters)
                            return Path_Delimiters
   is
   begin
      return Delimiters (Delimiters'First + 1 .. Delimiters'Last);
   end Sub_Delimiters;

   --------------
   -- Sub_Path --
   --------------

   function Sub_Path (Path       : Pathname;
                      Delimiters : Path_Delimiters)
                      return Pathname
   is
      From : Integer;
   begin

      if Delimiters'Length <= 1 then
         return "/";
      else
         From := Delimiters (Delimiters'First + 1).From;
         return Path (From .. Path'Last);
      end if;
   end Sub_Path;

   -----------
   -- Slice --
   -----------

   function Slice (Path       : Pathname;
                   Delimiters : Path_Delimiters;
                   Index      : Integer)
                   return Pathname
   is
   begin
      if Index in Delimiters'Range then
         return Path (Delimiters (Index).From .. Delimiters (Index).To);
      else
         return "";
      end if;
   end Slice;

   ----------
   -- Root --
   ----------

   function Root (Path       : Pathname;
                  Delimiters : Path_Delimiters)
                  return Pathname
   is (Slice (Path, Delimiters, Delimiters'First));


end Pathname_Manipulation;
