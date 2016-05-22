with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Gen_Cos_Table is
   use Ada.Numerics;

   FName : constant String := "../src/cos.ads";
   F     : File_Type;

   function Image (F : Float) return String
   is
      S : constant String := F'Img;
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Image;

begin
   Create (F, Out_File, FName);
   Put_Line (F, "package Cos is");
   New_Line (F);
   Put_Line (F, "   type Degree is mod 3600;");
   New_Line (F);
   Put_Line (F, "   Cos_Table : constant array (Degree) of Float :=");
   Put      (F, "                 (");
   for J in 0 .. 3599 loop
      declare
         Val : Float := Cos (Float (J) / 1800.0 * Pi);
         Val_Str : constant String := Image (Val);
      begin
         if J /= 3599 then
            Put_Line (F, Val_Str & ",");
            Put (F, "                  ");
         else
            Put_Line (F, Val_Str & ");");
         end if;
      end;
   end loop;
   Put_Line (F, "end Cos;");
   New_Line (F);
   Close (F);
end Gen_Cos_Table;
