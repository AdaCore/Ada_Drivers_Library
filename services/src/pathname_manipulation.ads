with HAL.Filesystem; use HAL.Filesystem;

package Pathname_Manipulation is
   procedure Root_Dir (Path        :     Pathname;
                       Start, Stop : out Integer);
end Pathname_Manipulation;
