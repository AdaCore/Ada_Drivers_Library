with HAL.Filesystem; use HAL.Filesystem;
with Ada.Streams;
with HAL; use HAL;

package Filesystem is

   ---------------------
   -- File_Descriptor --
   ---------------------

   type File_Descriptor is limited new Ada.Streams.Root_Stream_Type with private;

   function Valid (FD : File_Descriptor) return Boolean;

   function Read (FD   : in out File_Descriptor;
                  Data : out UInt8_Array) return Status_Kind;
   function Write (FD   : in out File_Descriptor;
                   Data : UInt8_Array) return Status_Kind;

   -----------------
   -- File system --
   -----------------

   function Create_File (Path : Pathname)
                         return Status_Kind;

   function Open (Path : String;
                  Mode : File_Mode;
                  FD   : out File_Descriptor)
                  return Status_Kind;

   function Close (FD : in out File_Descriptor) return Status_Kind;

   function Mount (Path : String;
                   FS   : not null Any_FS_Driver)
                   return Status_Kind;

private

   use Ada.Streams;

   type File_Descriptor is limited new Ada.Streams.Root_Stream_Type with record
      Index  : Integer := -1;
      Handle : Any_File_Handle := null;
   end record;

   overriding
   procedure Read
     (Stream : in out File_Descriptor;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out File_Descriptor;
      Item   : Stream_Element_Array);

end Filesystem;
