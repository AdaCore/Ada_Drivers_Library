package body Sudoku is

   function checkValidity (val : Integer; x : Integer; y : Integer;  sudoku_ar : in  sudoku_ar_t) return Boolean
   is
   begin
      for i in 0 .. 8 loop

         if (sudoku_ar (y * 9 + i) = val or sudoku_ar (i * 9 + x) = val) then
            return False;
         end if;
      end loop;

      declare
         startX : constant Integer := (x / 3) * 3;
         startY : constant Integer := (y / 3) * 3;
      begin
         for i in startY .. startY + 2 loop
            for j in startX .. startX + 2 loop
               if (sudoku_ar (i * 9 + j) = val) then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end;
   end checkValidity;



   function placeNumber (pos : Integer; sudoku_ar : in out sudoku_ar_t) return Boolean
   is
      ret : Boolean := False;
   begin
      if (pos = 81) then
         return True;
      end if;
      if (sudoku_ar (pos) > 0) then
         ret := placeNumber (pos + 1, sudoku_ar);
         if ret then
            return True;
         else
            return False;
         end if;
      end if;
      for n in 1 .. 9 loop
         if (checkValidity (n,  pos mod 9, pos / 9, sudoku_ar)) then
            sudoku_ar (pos) := n;
            ret := placeNumber (pos + 1, sudoku_ar);
            if ret then
               return True;
            end if;
            sudoku_ar (pos) := 0;
         end if;
      end loop;
      return False;
   end placeNumber;


   function solve (sudoku_ar : in out sudoku_ar_t) return Boolean
   is
      ret : Boolean := False;
   begin
      ret := placeNumber (0, sudoku_ar);
      if ret then
         return True;
      else
         return False;
      end if;
   end solve;




   procedure test is
      sudoku_ar : sudoku_ar_t :=
        (
         8, 5, 0, 0, 0, 2, 4, 0, 0,
         7, 2, 0, 0, 0, 0, 0, 0, 9,
         0, 0, 4, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 1, 0, 7, 0, 0, 2,
         3, 0, 5, 0, 0, 0, 9, 0, 0,
         0, 4, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 8, 0, 0, 7, 0,
         0, 1, 7, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 3, 6, 0, 4, 0
        );
   begin
      if solve (sudoku_ar) then
         null;
      end if;
   end test;
end Sudoku;

