with Ada.Unchecked_Conversion;
with LCD_Std_Out;

package body FAT_Filesystem is

   Volumes : array (1 .. MAX_VOLUMES) of aliased FAT_Volume;
   --  Mounted volumes

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String
   is
   begin
      for J in reverse S'Range loop
         if S (J) /= ' ' then
            return S (S'First .. J);
         end if;
      end loop;

      return "";
   end Trim;

   ----------
   -- Open --
   ----------

   function Open
     (Controller : Media_Controller_Access;
      LBA        : Unsigned_32;
      Status     : out Status_Code) return FAT_Volume_Access
   is
      Sector : Block (0 .. 511);
      Ret    : FAT_Volume_Access;

      subtype Disk_Parameter_Block is Block (0 .. 91);
      function To_Disk_Parameter is new Ada.Unchecked_Conversion
        (Disk_Parameter_Block, FAT_Disk_Parameter);

      subtype FSInfo_Block is Block (0 .. 11);
      function To_FSInfo is new Ada.Unchecked_Conversion
        (FSInfo_Block, FAT_FS_Info);

   begin
      if not Controller.Read_Block (LBA, Sector) then
         Status := Media_Error;
         return Null_FAT_Volume;
      end if;

      Status := OK;

      if Sector (510 .. 511) /= (16#55#, 16#AA#) then
         Status := FAT_Error;
         return Null_FAT_Volume;
      end if;

      --  Find a free Volume handle
      Ret := null;

      for J in Volumes'Range loop
         if not Volumes (J).Mounted then
            Ret := Volumes (J)'Access;
            exit;
         end if;
      end loop;

      if Ret = null then
         raise Max_Handles_Reached with "Maximum number of Volumes mounted";
      end if;

      Ret.Mounted := True;
      Ret.Controller := Controller;
      Ret.Disk_Parameters :=
        To_Disk_Parameter (Sector (0 .. 91));
      Ret.LBA := LBA;

      if Ret.Version = FAT32 then
         if not Controller.Read_Block
           (LBA + Unsigned_32 (Ret.FSInfo_Block_Number),
            Sector)
         then
            Status := Media_Error;
            return Null_FAT_Volume;
         end if;

         --  Check the generic FAT block signature
         if Sector (510 .. 511) /= (16#55#, 16#AA#) then
            Status := FAT_Error;
            return Null_FAT_Volume;
         end if;

         Ret.FSInfo :=
           To_FSInfo (Sector (16#1E4# .. 16#1EF#));
      end if;

      declare
         Data_Offset_In_Block : constant Unsigned_32 :=
                                  Unsigned_32 (Ret.Reserved_Blocks) +
                                  Ret.FAT_Table_Size_In_Blocks *
                                    Unsigned_32 (Ret.Number_Of_FATs);
         Root_Dir_Size        : Unsigned_32 := 0;
      begin
         if Ret.Version = FAT16 then
            Root_Dir_Size :=
              Unsigned_32
                (Ret.Number_Of_Entries_In_Root_Dir) * 32;

            if (Root_Dir_Size mod Ret.Block_Size_In_Bytes) = 0 then
               Root_Dir_Size := Root_Dir_Size / Ret.Block_Size_In_Bytes;
            else
               Root_Dir_Size := 1 + Root_Dir_Size / Ret.Block_Size_In_Bytes;
            end if;
         end if;

         Ret.Data_Area := LBA + Data_Offset_In_Block + Root_Dir_Size;
      end;

      return Ret;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Volume : FAT_Volume_Access)
   is
   begin
      Volume.Mounted := False;
   end Close;

   -------------------------
   -- Open_Root_Directory --
   -------------------------

   function Open_Root_Directory
     (Volume : FAT_Volume_Access) return Directory_Handle
   is
      D : Directory_Handle;
      Data : Block (0 .. 511);

      procedure Prepend
        (Name : Wide_String;
         Full : in out String;
         Idx  : in out Natural);

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (Name : Wide_String;
         Full : in out String;
         Idx  : in out Natural)
      is
         Val : Unsigned_16;
      begin
         for J in reverse Name'Range loop
            Val := Wide_Character'Pos (Name (J));
            if Val /= 16#FFFF#
              and then Val /= 0
            then
               if Val < 255 then
                  Full (Idx) := Character'Val (Val);
               else
                  Full (Idx) := '?';
               end if;

               Idx := Idx - 1;
            end if;
         end loop;
      end Prepend;

   begin
      if Volume.Version = FAT16 then
         D.Block := Unsigned_32 (Volume.Reserved_Blocks) +
           Volume.FAT_Table_Size_In_Blocks *
             Unsigned_32 (Volume.Number_Of_FATs);
      else
         D.Block := Volume.Data_Area +
           (Volume.Root_Dir_Cluster - 2) *
             Unsigned_32 (Volume.Number_Of_Blocks_Per_Cluster);
      end if;

      D.Volume := Volume;

      if Volume.Controller.Read_Block (D.Block, Data) then
         declare
            D_Entry : FAT_Directory_Entry;
            V_Entry : VFAT_Directory_Entry;
            subtype Entry_Data is Block (1 .. 32);
            function To_Entry is new Ada.Unchecked_Conversion
              (Entry_Data, FAT_Directory_Entry);
            function To_VFAT_Entry is new Ada.Unchecked_Conversion
              (FAT_Directory_Entry, VFAT_Directory_Entry);

            C           : Unsigned_8;
            Entry_Idx   : Unsigned_16 := 0;
            Last_Seq    : VFAT_Sequence_Number := 0;
            Full_Name   : String (1 .. 256);
            Full_Idx    : Natural := Full_Name'Last;
            CRC         : Unsigned_8 := 0;
            Matches     : Boolean;
            Current_CRC : Unsigned_8;

         begin
            while Entry_Idx * 32 < Data'Last
              and then Data (Entry_Idx * 32) /= 0
            loop
               D_Entry :=
                 To_Entry (Data (Entry_Idx * 32 .. Entry_Idx * 32 + 31));

               --  Check if we have a VFAT entry here by checking that the
               --  attributes are 16#0F# (e.g. all attributes set except
               --  subdirectory and archive)
               if D_Entry.Attributes = VFAT_Directory_Entry_Attribute then
                  C := 0; --  Ignore the entry for normal parsing
                  V_Entry := To_VFAT_Entry (D_Entry);

                  if V_Entry.VFAT_Attr.Stop_Bit then
                     Full_Idx := Full_Name'Last;
                     Last_Seq := 0;

                  else
                     if Last_Seq = 0
                       or else Last_Seq - 1 /= V_Entry.VFAT_Attr.Sequence
                     then
                        Full_Idx := Full_Name'Last;
                     end if;

                     Last_Seq := V_Entry.VFAT_Attr.Sequence;

                     Prepend (V_Entry.Name_3, Full_Name, Full_Idx);
                     Prepend (V_Entry.Name_2, Full_Name, Full_Idx);
                     Prepend (V_Entry.Name_1, Full_Name, Full_Idx);

                     if V_Entry.VFAT_Attr.Sequence = 1 then
                        CRC := V_Entry.Checksum;
                     end if;
                  end if;

               elsif D_Entry.Attributes.Volume_Label then
                  C := 0;

               else
                  C := Character'Enum_Rep (D_Entry.Filename (1));
               end if;

               if C /= 0 and then C /= 16#E5# then
                  if Full_Idx = Full_Name'Last then
                     Matches := False;
                  else
                     Current_CRC := 0;
                     Last_Seq := 0;

                     for Ch of String'(D_Entry.Filename &
                                         D_Entry.Extension)
                     loop
                        C := Character'Enum_Rep (Ch);
                        Current_CRC := Shift_Right (Current_CRC and 16#FE#, 1)
                          or Shift_Left (Current_CRC and 16#01#, 7);
                        --  Modulo addition
                        Current_CRC := Current_CRC + C;
                     end loop;

                     Matches := Current_CRC = CRC;
                  end if;

                  if Matches then
                     LCD_Std_Out.Put
                       (Full_Name (Full_Idx + 1 .. Full_Name'Last));
                  elsif Trim (D_Entry.Extension)'Length > 0 then
                     LCD_Std_Out.Put
                       (Trim (D_Entry.Filename) & "." &
                          Trim (D_Entry.Extension));
                  else
                     LCD_Std_Out.Put
                       (Trim (D_Entry.Filename));
                  end if;

                  if D_Entry.Attributes.Subdirectory then
                     LCD_Std_Out.Put_Line ("/");
                  else
                     LCD_Std_Out.New_Line;
                  end if;
               end if;

               Entry_Idx := Entry_Idx + 1;
            end loop;
         end;

         LCD_Std_Out.New_Line;
      end if;

      return D;
   end Open_Root_Directory;

end FAT_Filesystem;
