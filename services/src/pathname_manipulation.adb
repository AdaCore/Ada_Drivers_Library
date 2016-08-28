package body Pathname_Manipulation is

   --------------
   -- Root_Dir --
   --------------

   procedure Root_Dir (Path        :     Pathname;
                       Start, Stop : out Integer)
   is
   begin
      Start := Path'First;
      Stop := Start;
      if Path'Length = 0 then
         return;
      end if;

      while Start <= Path'Last and then Path (Start) = '/' loop
         Start := Start + 1;
      end loop;

      Stop := Start;
      while Stop + 1 <= Path'Last and then Path (Stop + 1) /= '/' loop
         Stop := Stop + 1;
      end loop;
   end Root_Dir;

   --------------
   -- Root_Dir --
   --------------

   function Root_Dir (Path : Pathname) return Pathname is
      Start, Stop : Integer;
   begin
      Root_Dir (Path, Start, Stop);
      if Start not in Path'Range or else Stop not in Path'Range then
         return "";
      else
         return Path (Start .. Stop);
      end if;
   end Root_Dir;

end Pathname_Manipulation;
