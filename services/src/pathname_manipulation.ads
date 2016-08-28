with HAL.Filesystem; use HAL.Filesystem;

package Pathname_Manipulation is

   procedure Root_Dir (Path        :     Pathname;
                       Start, Stop : out Integer);

   function Root_Dir (Path : Pathname) return Pathname;

end Pathname_Manipulation;
