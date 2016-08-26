
package body Pathname_Manipulation is
   function Root_Dir (Path : Pathname) return Pathname is
      First : Integer := Path'First;
      Last  : Integer;
   begin

      if Path'Length = 0 then
         return "";
      end if;

      while First < Path'Last and then Path (First) = '/' loop
         First := First + 1;
      end loop;

      Last := First;
      while Last + 1 < Path'Last and then Path (Last + 1) /= '/' loop
         Last := Last + 1;
      end loop;

      if Last = First then
         return "";
      else
         return Path (First .. Last);
      end if;
   end Root_Dir;
end Pathname_Manipulation;
