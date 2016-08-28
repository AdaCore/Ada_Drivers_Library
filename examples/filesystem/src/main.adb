with HAL;           use HAL;
with Virtual_File_System; use Virtual_File_System;
with HAL.Filesystem; use HAL.Filesystem;
with Semihosting;
with Semihosting.Filesystem; use Semihosting.Filesystem;

procedure Main is

   procedure List_Dir (FS : in out FS_Driver'Class; Path : Pathname);

   procedure List_Dir (FS : in out FS_Driver'Class; Path : Pathname) is
      Status : Status_Kind;
      DH : Directory_Handle_Ref;
   begin
      Status := FS.Open_Directory (Path, DH);
      if Status /= Status_Ok then
         Semihosting.Log_Line ("Open Directory '" & Path & "' Error: " & Status'Img);
      else
         declare
            Ent : Directory_Entry;
            Index : Positive := 1;
         begin

            Semihosting.Log_Line ("Listing '" & Path & "' content:");
            loop
               Status := DH.Read_Entry (Index, Ent);
               if Status = Status_Ok then
                  Semihosting.Log_Line (" - '" & DH.Entry_Name (Index) & "'");
                  Semihosting.Log_Line ("   Kind: " & Ent.Entry_Type'Img);
               else
                  exit;
               end if;
               Index := Index + 1;
            end loop;
         end;
      end if;
   end List_Dir;

   My_VFS : VFS;
   My_VFS2 : aliased VFS;
   My_VFS3 : aliased VFS;
   My_SHFS : aliased SHFS;
   Status : Status_Kind;
   FH : File_Handle_Ref;
   Data : Byte_Array (1 .. 10);

begin

   Status := My_VFS.Mount (Path       => "test1",
                           Filesystem => My_VFS2'Unchecked_Access);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;
   Status := My_VFS2.Mount (Path       => "test2",
                            Filesystem => My_VFS3'Unchecked_Access);

   if Status /= Status_Ok then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;

   Status := My_VFS.Mount (Path       => "host",
                            Filesystem => My_SHFS'Unchecked_Access);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;

   Status := My_VFS.Unlink ("/test1/no_file");
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Unlink Error: " & Status'Img);
   end if;
   Status := My_VFS.Unlink ("//test1/no_file");
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Unlink Error: " & Status'Img);
   end if;
   Status := My_VFS.Unlink ("/test1//no_file");
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Unlink Error: " & Status'Img);
   end if;
   Status := My_VFS.Unlink ("/test1/test2/no_file");
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Unlink Error: " & Status'Img);
   end if;

   Status := My_VFS.Open ("/host/tmp/test.shfs", FH);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Open Error: " & Status'Img);
   end if;

   Status := FH.Read (Data);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Read Error: " & Status'Img);
   end if;

   for C of Data loop
      Semihosting.Log (Character'Val (Integer (C)));
   end loop;
   Semihosting.Log_New_Line;

   Status := FH.Seek (10);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Seek Error: " & Status'Img);
   end if;

   Status := FH.Read (Data);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Read Error: " & Status'Img);
   end if;

   for C of Data loop
      Semihosting.Log (Character'Val (Integer (C)));
   end loop;
   Semihosting.Log_New_Line;

   Status := FH.Close;
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Close Error: " & Status'Img);
   end if;

   List_Dir (My_VFS, "/");
   List_Dir (My_VFS, "/test1");
   List_Dir (My_VFS, "/test1/");
end Main;
