with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Filesystem.VFS;

package body Helpers is

   Program_Abspath : constant String := Native.Filesystem.Join
     (Ada.Directories.Current_Directory, Ada.Command_Line.Command_Name, False);
   Test_Dir : constant String := Ada.Directories.Containing_Directory
     (Ada.Directories.Containing_Directory (Program_Abspath));
   Material_Dir : constant String :=
     Ada.Directories.Compose (Test_Dir, "material");

   ----------
   -- Test --
   ----------

   procedure Test (Status : Status_Code) is
   begin
      if Status /= OK then
         raise Program_Error;
      end if;
   end Test;

   ------------
   -- Create --
   ------------

   function Create
     (Root_Dir          : String;
      Create_If_Missing : Boolean := False)
      return Native.Filesystem.Native_FS_Driver_Access
   is
      use Native.Filesystem;
      Abs_Root_Dir : constant String := Ada.Directories.Compose
        (Material_Dir, Root_Dir);
      Result       : constant Native_FS_Driver_Access := new Native_FS_Driver;
   begin
      if Create_If_Missing
        and then not Ada.Directories.Exists (Abs_Root_Dir)
      then
         Ada.Directories.Create_Directory (Abs_Root_Dir);
      end if;
      Test (Create (Result.all, Abs_Root_Dir));
      return Result;
   end Create;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : in out File_Handle'Class) return UInt8_Array is
      type UInt8_Array_Access is access UInt8_Array;
      procedure Destroy is new Ada.Unchecked_Deallocation
        (UInt8_Array, UInt8_Array_Access);

      Buffer : UInt8_Array_Access := new UInt8_Array (1 .. 1024);
      Last   : Natural := 0;
      Amount : File_Size;
   begin
      loop
         --  If the buffer is full, reallocate it twice bigger

         if Last >= Buffer'Last then
            declare
               New_Buffer : constant UInt8_Array_Access :=
                 new UInt8_Array (1 .. 2 * Buffer'Length);
            begin
               New_Buffer (1 .. Buffer'Length) := Buffer.all;
               Destroy (Buffer);
               Buffer := New_Buffer;
            end;
         end if;

         --  As File_Handle.Read does not tell us how many bytes it could read
         --  when it cannot fill the buffer, read byte by byte...

         Amount := 1;
         case File.Read (Buffer (Last + 1)'Address, Amount) is
            when OK =>
               null;
            when Input_Output_Error =>
               exit;
            when others =>
               raise Program_Error;
         end case;
         Last := Last + 1;
      end loop;

      declare
         Result : constant UInt8_Array := Buffer (1 .. Last);
      begin
         Destroy (Buffer);
         return Result;
      end;
   end Read_File;

   -----------------
   -- Quote_Bytes --
   -----------------

   function Quote_Bytes (Bytes : UInt8_Array) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      Hex_Digits : constant array (UInt8 range 0 .. 15) of Character :=
        "0123456789abcdef";
   begin
      for B of Bytes loop
         declare
            C : constant Character := Character'Val (B);
         begin
            if C = '\' then
               Append (Result, "\\");
            elsif C in ' ' .. '~' then
               Append (Result, C);
            else
               Append
                 (Result, "\x" & Hex_Digits (B / 16) & Hex_Digits (B mod 16));
            end if;
         end;
      end loop;
      return To_String (Result);
   end Quote_Bytes;

   ----------
   -- Dump --
   ----------

   procedure Dump (Dir : String) is
      DH     : Any_Directory_Handle;
      Node   : Any_Node_Handle;
      I      : Positive := 1;
      Status : Status_Code;
   begin
      Put_Line ("Entering " & Dir);
      Test (Filesystem.VFS.Open (Dir, DH));

      loop
         Status := DH.Read (Node);
         exit when Status /= OK;
         Test (Status);

         declare
            Name : constant String :=
              Native.Filesystem.Join (Dir, Node.Basename, True);
         begin
            if Node.Is_Subdirectory then
               Dump (Name);
            else
               Put_Line ("  File: " & Name);
               declare
                  File : Any_File_Handle;
               begin
                  Test (Filesystem.VFS.Open (Name, Read_Mode, File));
                  declare
                     Content : constant UInt8_Array := Read_File (File.all);
                  begin
                     Put_Line ("  Contents: " & Quote_Bytes (Content));
                  end;
                  File.Close;
               end;
            end if;
         end;

         I := I + 1;
      end loop;

      Put_Line ("Leaving " & Dir);
      DH.Close;
   end Dump;

end Helpers;
