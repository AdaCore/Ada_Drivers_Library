--  Institution: Technische Universitaet Muenchen
--  Department:  Real-Time Computer Systems (RCS)
--  Project:     StratoX
--  Authors:     Martin Becker (becker@rcs.ei.tum.de)

--  @summary String functions
package body MyStrings with SPARK_Mode is

   procedure StrCpySpace (outstring : out String; instring : String) is
   begin
      if instring'Length >= outstring'Length then
         --  trim
         outstring := instring (instring'First .. instring'First - 1 + outstring'Length);
      else
         --  pad
         declare
            lastidx : constant Natural := outstring'First + instring'Length - 1;
         begin
            outstring := (others => ' ');
            outstring (outstring'First .. lastidx) := instring;
         end;
      end if;
   end StrCpySpace;

   function RTrim (S : String) return String is
   begin
      for J in reverse S'Range loop
         if S (J) /= ' ' then
            return S (S'First .. J);
         end if;
      end loop;

      return "";
   end RTrim;

   function LTrim (S : String) return String is
   begin
      for J in S'Range loop
         if S (J) /= ' ' then
            return S (J .. S'Last);
         end if;
      end loop;

      return "";
   end LTrim;

   function Trim (S : String) return String is
   begin
      return LTrim (RTrim (S));
   end Trim;

   function StrChr (S : String; C : Character) return Integer is
   begin
      for idx in S'Range loop
         if S (idx) = C then
            return idx;
         end if;
      end loop;
      return S'Last + 1;
   end StrChr;

   function Is_AlNum (c : Character) return Boolean is
   begin
      return (c in 'a' .. 'z') or (c in 'A' .. 'Z') or (c in '0' .. '9');
   end Is_AlNum;

   function Strip_Non_Alphanum (s : String) return String with SPARK_Mode => Off is
      tmp : String (1 .. s'Length) := s;
      len : Integer := 0;
   begin
      for c in s'Range loop
         if Is_AlNum (s (c)) then
            len := len + 1;
            tmp (len) := s (c);
         end if;
      end loop;
      declare
         ret : constant String (1 .. len) := tmp (1 .. len); --  SPARK: "subtype constraint cannot depend on len"
      begin
         return ret;
      end;
   end Strip_Non_Alphanum;

end MyStrings;
