
package body Pathname_Manipulation is
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
end Pathname_Manipulation;
