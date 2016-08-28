with HAL;           use HAL;
with Virtual_File_System; use Virtual_File_System;
with HAL.Filesystem; use HAL.Filesystem;
with Semihosting;
with Semihosting.Filesystem; use Semihosting.Filesystem;

procedure Main is
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
end Main;
