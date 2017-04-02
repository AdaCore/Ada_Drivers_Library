with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Pathname_Manipulation; use Pathname_Manipulation;
with Ada.Text_IO;           use Ada.Text_IO;

package body Helpers is

   Program_Abspath : constant Pathname := Native.Filesystem.Join
     (Ada.Directories.Current_Directory, Ada.Command_Line.Command_Name, False);
   Test_Dir : constant Pathname := Ada.Directories.Containing_Directory
     (Ada.Directories.Containing_Directory (Program_Abspath));
   Material_Dir : constant Pathname :=
     Ada.Directories.Compose (Test_Dir, "material");

   ----------
   -- Test --
   ----------

   procedure Test (Status : Status_Kind) is
   begin
      if Status /= Status_Ok then
         raise Program_Error;
      end if;
   end Test;

   ------------
   -- Create --
   ------------

   function Create
     (Root_Dir          : Pathname;
      Create_If_Missing : Boolean := False)
      return Native.Filesystem.Native_FS_Driver_Ref
   is
      use Native.Filesystem;
      Abs_Root_Dir : constant Pathname := Ada.Directories.Compose
        (Material_Dir, Root_Dir);
      Result       : constant Native_FS_Driver_Ref := new Native_FS_Driver;
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

         case File.Read (Buffer (Last + 1 .. Last + 1)) is
            when Status_Ok =>
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

   procedure Dump (FS : in out FS_Driver'Class; Dir : Pathname) is
      DH     : Any_Directory_Handle;
      DE     : Directory_Entry;
      I      : Positive := 1;
      Status : Status_Kind;
   begin
      Put_Line ("Entering " & Dir);
      Test (FS.Open_Directory (Dir, Parse (Dir), DH));

      loop
         Status := DH.Read_Entry (I, DE);
         exit when Status = No_Such_File_Or_Directory;
         Test (Status);

         declare
            Name : constant Pathname :=
              Native.Filesystem.Join (Dir, DH.Entry_Name (I), True);
         begin
            case DE.Entry_Type is
               when Regular_File =>
                  Put_Line ("  File: " & Name);
                  declare
                     File : Any_File_Handle;
                  begin
                     Test (FS.Open (Name, Parse (Name), Read_Only, File));
                     declare
                        Content : constant UInt8_Array := Read_File (File.all);
                     begin
                        Put_Line ("  Contents: " & Quote_Bytes (Content));
                     end;
                     Test (File.Close);
                  end;

               when Directory =>
                  Dump (FS, Name);
            end case;
         end;

         I := I + 1;
      end loop;

      Put_Line ("Leaving " & Dir);
      Test (DH.Close);
   end Dump;

end Helpers;
