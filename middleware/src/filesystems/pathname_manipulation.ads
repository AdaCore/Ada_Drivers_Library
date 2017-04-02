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

with HAL.Filesystem; use HAL.Filesystem;

package Pathname_Manipulation is

   type New_Path (String_Length     : Natural;
                  Delimiters_Length : Natural)
   is record
      Str   : String (1 .. String_Length);
      Delim : Path_Delimiters (1 .. Delimiters_Length);
   end record;

   Empty_Path : constant New_Path := (String_Length => 0,
                                      Delimiters_Length => 0,
                                      Str => "",
                                      Delim => (others => (0, 0)));

   function Valid (Path : New_Path) return Boolean
   is (
       --  From is always less than or equal to To
       (for all E of Path.Delim =>
           E.From <= E.To)
       and then
       --  All indexes are within the range of path String
         (for all E of Path.Delim =>
             E.From in Path.Str'Range and then E.To in Path.Str'Range)
       and then
       --  Indexes are strictly incresing
         (for all Index in Path.Delim'First .. Path.Delim'Last - 1 =>
             Path.Delim (Index).To < Path.Delim (Index + 1).From))
   with Ghost;

   function Parse (Path : Pathname) return New_Path
     with Post => Valid (Parse'Result);

   function "&"(Left : New_Path; Right : New_Path) return New_Path
     with Post => Valid ("&"'Result);

   function Slice (Path       : New_Path;
                   Index      : Integer)
                   return Pathname;

   function Valid (Path : Pathname; Delimiters : Path_Delimiters) return Boolean
   is (
       --  From is always less than or equal to To
       (for all E of Delimiters =>
           E.From <= E.To)
       and then
       --  All indexes are within the range of path String
         (for all E of Delimiters =>
             E.From in Path'Range and then E.To in Path'Range)
       and then
       --  Indexes are strictly incresing
         (for all Index in Delimiters'First .. Delimiters'Last - 1 =>
             Delimiters (Index).To < Delimiters (Index + 1).From))
   with Ghost;

   function Parse (Path : Pathname) return Path_Delimiters
     with Post => Valid (Path, Parse'Result);

   function Sub_Delimiters (Delimiters : Path_Delimiters)
                            return Path_Delimiters;

   function Sub_Path (Path       : Pathname;
                      Delimiters : Path_Delimiters)
                      return Pathname;

   function Slice (Path       : Pathname;
                   Delimiters : Path_Delimiters;
                   Index      : Integer)
                   return Pathname;

   function Root (Path       : Pathname;
                  Delimiters : Path_Delimiters)
                  return Pathname;

end Pathname_Manipulation;
