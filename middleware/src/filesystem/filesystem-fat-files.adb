------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  This packages provide a low level driver for the FAT file system
--  architecture. It is recommended to _not_ use this interface directly but to
--  access the file system using the File_IO package. For more info, see the
--  file system chapter of the documentation.

with Ada.Unchecked_Conversion;
with Filesystem.FAT.Directories;

package body Filesystem.FAT.Files is


   function Absolute_Block (File : in out FAT_File_Handle) return Block_Number
     with Inline_Always;

   function Ensure_Buffer (File : in out FAT_File_Handle) return Status_Code
     with Inline_Always;

   function Next_Block
     (File : in out FAT_File_Handle;
      Inc  : Positive := 1) return Status_Code;

   --------------------
   -- Absolute_Block --
   --------------------

   function Absolute_Block (File : in out FAT_File_Handle) return Block_Number
   is (File.FS.LBA +
         File.FS.Cluster_To_Block (File.Current_Cluster) +
         File.Current_Block);

   -------------------
   -- Ensure_Buffer --
   -------------------

   function Ensure_Buffer (File : in out FAT_File_Handle) return Status_Code
   is
   begin
      if not File.Buffer_Filled and then File.Mode /= Write_Only then
         if not File.FS.Controller.Read
           (Absolute_Block (File),
            File.Buffer)
         then
            --  Read error
            return Disk_Error;
         end if;

         File.Buffer_Filled := True;
         File.Buffer_Dirty := False;
      end if;

      return OK;
   end Ensure_Buffer;

   ----------------
   -- Next_Block --
   ----------------

   function Next_Block
     (File : in out FAT_File_Handle;
      Inc  : Positive := 1) return Status_Code
   is
      Todo   : Block_Offset := Block_Offset (Inc);
      Status : Status_Code;
      Next   : Cluster_Type;
   begin
      --  First take care of uninitialized handlers:

      if File.Is_Free then
         return Invalid_Parameter;
      end if;

      if File.Current_Cluster = 0 then
         File.Current_Cluster := File.D_Entry.Start_Cluster;
         File.Current_Block   := 0;
         Todo := Todo - 1;

         if Todo = 0 then
            return OK;
         end if;
      end if;

      Status := Flush (File);

      if Status /= OK then
         return Status;
      end if;

      --  Invalidate the current block buffer
      File.Buffer_Filled := False;

      while Todo > 0 loop
         --  Move to the next block
         if File.Current_Block + Todo >= File.FS.Blocks_Per_Cluster then
            Todo := Todo + File.Current_Block - File.FS.Blocks_Per_Cluster;
            File.Current_Block := File.FS.Blocks_Per_Cluster;

         else
            File.Current_Block := File.Current_Block + Todo;
            Todo := 0;
         end if;

         --  Check if we're still in the same cluster
         if File.Current_Block = File.FS.Blocks_Per_Cluster then
            --  Move on to the next cluster
            File.Current_Block   := 0;
            Next := File.FS.Get_FAT (File.Current_Cluster);

            if Next < 2 then
               return Internal_Error;

            elsif not File.FS.Is_Last_Cluster (Next) then
               --  Nominal case: there's a next cluster
               File.Current_Cluster := Next;

            elsif File.Mode /= Read_Only then
               --  Allocate a new cluster
               File.Current_Cluster :=
                 File.FS.New_Cluster (File.Current_Cluster);

               if File.Current_Cluster = INVALID_CLUSTER then
                  return Disk_Full;
               end if;

            else
               --  Invalid operation: should not happen, so raise an internal
               --  error
               return Internal_Error;
            end if;
         end if;
      end loop;

      return OK;
   end Next_Block;

   ----------
   -- Open --
   ----------

   function Open
     (Parent : FAT_Node;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : FAT_File_Handle_Access) return Status_Code
   is
      Node : FAT_Node;
      Ret  : Status_Code;
   begin
      Ret := Directories.Find (Parent, Name, Node);

      if Ret /= OK then
         if Mode = Read_Only then
            return No_Such_File;
         end if;

         Ret := Directories.Create_File_Node (Parent, Name, Node);
      end if;

      if Ret /= OK then
         return Ret;
      end if;

      if Mode = Write_Only then
         Directories.Set_Size (Node, 0);
         --  Free the cluster chain if > 1 cluster
         Ret := Directories.Adjust_Clusters (Node);

         if Ret /= OK then
            return Ret;
         end if;
      end if;

      File.Is_Free         := False;
      File.FS              := Node.FS;
      File.Mode            := Mode;
      File.Current_Cluster := Node.Start_Cluster;
      File.Current_Block   := 0;
      File.Buffer          := (others => 0);
      File.Buffer_Filled   := False;
      File.Buffer_Dirty    := False;
      File.File_Index      := 0;
      File.D_Entry         := Node;
      File.Parent          := Parent;

      return OK;
   end Open;

   ----------
   -- Read --
   ----------

   function Read
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : in out FAT_File_Size) return Status_Code
   is
      type Address is mod 2 ** System.Word_Size;

      function To_Address is new Ada.Unchecked_Conversion
        (System.Address, Address);

      Data        : File_Data (1 .. Length) with Import, Address => Addr;
      --  Byte array representation of the buffer to read

      Addr_Int    : constant Unsigned_64 := Unsigned_64 (To_Address (Addr));

      Idx         : FAT_File_Size;
      --  Index from the current block

      Data_Length : FAT_File_Size := Data'Length;
      --  The total length to read

      Data_Idx    : FAT_File_Size := Data'First;
      --  Index into the data array of the next bytes to read

      R_Length    : FAT_File_Size;
      --  The size of the data to read in one operation

      N_Blocks    : Block_Offset;

      Status      : Status_Code;

   begin
      if File.Is_Free then
         Length := 0;
         return Invalid_Parameter;
      end if;

      if File.Mode = Write_Only then
         Length := 0;
         return Access_Denied;
      end if;

      --  Are we at then end of the file?
      if File.File_Index = File.D_Entry.Size
        or else Data_Length = 0
      then
         Length := 0;
         return OK;
      end if;

      Status := Flush (File);

      if Status /= OK then
         Length := 0;
         return Status;
      end if;

      --  Clamp the number of data to read to the size of the file
      Data_Length := FAT_File_Size'Min
        (File.D_Entry.Size - File.File_Index,
         Data_Length);

      loop
         Idx := File.File_Index mod File.FS.Block_Size;

         if Idx = 0 and then Data_Length >= File.FS.Block_Size then
            --  Case where the data to read is aligned on a block, and
            --  we have at least one block to read.

            --  Check the compatibility of the User's buffer with DMA transfers
            if (Addr_Int mod 4) = 0 then
               --  User data is aligned on words: we can directly perform DMA
               --  transfers to it

               --  Read as many blocks as possible withing the current cluster
               N_Blocks := Block_Offset'Min
                 (Block_Offset (Data_Length / File.FS.Block_Size),
                  File.FS.Blocks_Per_Cluster - File.Current_Block);

               if not File.FS.Controller.Read
                 (Absolute_Block (File),
                  HAL.UInt8_Array
                    (Data
                      (Data_Idx ..
                       Data_Idx +
                         FAT_File_Size (N_Blocks) * File.FS.Block_Size - 1)))
               then
                  --  Read error: return the number of data read so far
                  Length := Data_Idx - Data'First;
                  return Disk_Error;
               end if;

               Status := Next_Block (File, Positive (N_Blocks));

               if Status /= OK then
                  Length := Data_Idx - Data'First;
                  return Status;
               end if;

            else
               --  User data is not aligned: we thus have to use the Handle's
               --  cache (512 bytes)

               --  Reading one block
               N_Blocks := 1;

               --  Fill the buffer
               Status := Ensure_Buffer (File);

               if Status /= OK then
                  --  read error: return the number of bytes read so far
                  Length := Data_Idx - Data'First;
                  return Status;
               end if;

               Data (Data_Idx .. Data_Idx + 511) := File_Data (File.Buffer);

               Status := Next_Block (File);

               if Status /= OK then
                  Length := Data_Idx - Data'First;
                  return Status;
               end if;
            end if;

            Data_Idx := Data_Idx + FAT_File_Size (N_Blocks) * 512;
            File.File_Index :=
              File.File_Index + FAT_File_Size (N_Blocks) * 512;
            Data_Length := Data_Length - FAT_File_Size (N_Blocks) * 512;

         else
            --  Not aligned on a block, or less than 512 bytes to read
            --  We thus need to use our internal buffer.
            Status := Ensure_Buffer (File);

            if Status /= OK then
               --  read error: return the number of bytes read so far
               Length := Data_Idx - Data'First;
               return Status;
            end if;

            R_Length := FAT_File_Size'Min (File.Buffer'Length - Idx,
                                       Data_Length);
            Data (Data_Idx .. Data_Idx + R_Length - 1) := File_Data
              (File.Buffer (Natural (Idx) .. Natural (Idx + R_Length - 1)));

            Data_Idx           := Data_Idx + R_Length;
            File.File_Index := File.File_Index + R_Length;
            Data_Length        := Data_Length - R_Length;

            if Idx + R_Length = File.FS.Block_Size then
               Status := Next_Block (File);

               if Status /= OK then
                  Length := Data_Idx - Data'First;
                  return Status;
               end if;
            end if;
         end if;

         exit when Data_Length = 0;
      end loop;

      --  Update the number of bytes we actually read
      Length := Data_Idx - Data'First;

      return OK;
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (File   : in out  FAT_File_Handle;
      Addr   : System.Address;
      Length : FAT_File_Size) return Status_Code
   is
      procedure Inc_Size (Amount : FAT_File_Size);

      Data        : aliased File_Data (1 .. Length) with Address => Addr;
      --  Byte array representation of the data to write

      Idx         : FAT_File_Size;

      Data_Length : FAT_File_Size := Data'Length;
      --  The total length to read

      Data_Idx    : FAT_File_Size := Data'First;
      --  Index into the data array of the next bytes to write

      N_Blocks    : FAT_File_Size;
      --  The number of blocks to read at once

      W_Length    : FAT_File_Size;
      --  The size of the data to write in one operation

      Status      : Status_Code;

      --------------
      -- Inc_Size --
      --------------

      procedure Inc_Size (Amount : FAT_File_Size)
      is
      begin
         Data_Idx        := Data_Idx + Amount;
         File.File_Index := File.File_Index + Amount;
         Data_Length     := Data_Length - Amount;

         Directories.Set_Size (File.D_Entry, File.File_Index);
      end Inc_Size;

   begin
      if File.Is_Free or File.Mode = Read_Only then
         return Access_Denied;
      end if;

      if Data_Length = 0 then
         return OK;
      end if;

      --  Initialize the current cluster if not already done
      if File.Current_Cluster = 0 then
         Status := Next_Block (File);

         if Status /= OK then
            return Status;
         end if;
      end if;

      Idx := File.File_Index mod File.FS.Block_Size;

      if Data_Length < File.FS.Block_Size then
         --  First fill the buffer
         if Ensure_Buffer (File) /= OK then
            --  read error: return the number of bytes read so far
            return Disk_Error;
         end if;

         W_Length := FAT_File_Size'Min
           (File.Buffer'Length - Idx,
            Data'Length);

         File.Buffer (Natural (Idx) .. Natural (Idx + W_Length - 1)) :=
           Block (Data (Data_Idx .. Data_Idx + W_Length - 1));
         File.Buffer_Dirty := True;

         Inc_Size (W_Length);

         --  If we stopped on the boundaries of a new block, then move on to
         --  the next block
         if (File.File_Index mod File.FS.Block_Size) = 0 then
            Status := Next_Block (File);

            if Status /= OK then
               return Status;
            end if;
         end if;

         if Data_Length = 0 then
            --  We've written all the data, let's exit right now
            return OK;
         end if;
      end if;

      --  At this point, the buffer is empty and a new block is ready to be
      --  written. Check if we can write several blocks at once
      while Data_Length >= File.FS.Block_Size loop
         --  we have at least one full block to write.

         --  Determine the number of full blocks we need to write:
         N_Blocks := FAT_File_Size'Min
           (FAT_File_Size (File.FS.Blocks_Per_Cluster) -
                FAT_File_Size (File.Current_Block),
            Data_Length / File.FS.Block_Size);

         --  Writing all blocks in one operation
         W_Length := N_Blocks * File.FS.Block_Size;

         --  Fill directly the user data
         if not File.FS.Controller.Write
           (Absolute_Block (File),
            Block (Data (Data_Idx .. Data_Idx + W_Length - 1)))
         then
            return Disk_Error;
         end if;

         Inc_Size (W_Length);
         Status := Next_Block (File, Positive (N_Blocks));

         if Status /= OK then
            return Status;
         end if;
      end loop;

      --  Now everything that remains is smaller than a block. Let's fill the
      --  buffer with this data

      if Data_Length > 0 then
         --  First fill the buffer
         if Ensure_Buffer (File) /= OK then
            return Disk_Error;
         end if;

         File.Buffer (0 .. Natural (Data_Length - 1)) :=
           Block (Data (Data_Idx .. Data'Last));
         File.Buffer_Dirty := True;

         Inc_Size (Data_Length);
      end if;

      return OK;
   end Write;

   -----------
   -- Flush --
   -----------

   function Flush
     (File : in out FAT_File_Handle) return Status_Code
   is
   begin
      if File.Buffer_Dirty then
         if not File.FS.Controller.Write
           (Absolute_Block (File),
            File.Buffer)
         then
            return Disk_Error;
         end if;

         File.Buffer_Dirty := False;
      end if;

      return Directories.Update_Entry (Parent => File.Parent,
                                       Value  => File.D_Entry);
   end Flush;

   ----------
   -- Seek --
   ----------

   function Seek
     (File   : in out FAT_File_Handle;
      Amount : in out FAT_File_Size;
      Origin : Seek_Mode) return Status_Code
   is
      Status    : Status_Code;
      New_Pos   : FAT_File_Size;
      N_Blocks  : FAT_File_Size;

   begin
      case Origin is
         when From_Start =>
            if Amount > File.D_Entry.Size then
               Amount := File.D_Entry.Size;
            end if;

            New_Pos := Amount;

         when From_End =>
            if Amount > File.D_Entry.Size then
               Amount := File.D_Entry.Size;
            end if;

            New_Pos := File.D_Entry.Size - Amount;

         when Forward =>
            if Amount + File.File_Index > File.D_Entry.Size then
               Amount := File.D_Entry.Size - File.File_Index;
            end if;

            New_Pos := File.File_Index + Amount;

         when Backward =>
            if Amount > File.File_Index then
               Amount := File.File_Index;
            end if;

            New_Pos := File.File_Index - Amount;
      end case;

      if New_Pos < File.File_Index then
         --  Rewind the file pointer to the beginning of the file
         --  ??? A better check would be to first check if we're still in the
         --  same cluster, in which case we wouldn't need to do this rewind,
         --  but even if it's the case, we're still safe here, although a bit
         --  slower than we could.
         File.File_Index      := 0;
         File.Current_Cluster := File.D_Entry.Start_Cluster;
         File.Current_Block   := 0;
         File.Buffer_Filled   := False;
      end if;

      N_Blocks := (New_Pos - File.File_Index) / File.FS.Block_Size;

      if N_Blocks > 0 then
         Status := Next_Block (File, Positive (N_Blocks));

         if Status /= OK then
            return Status;
         end if;
      end if;

      File.File_Index := New_Pos;

      if Ensure_Buffer (File) /= OK then
         return Disk_Error;
      end if;

      return OK;
   end Seek;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out FAT_File_Handle)
   is
      Status : Status_Code with Unreferenced;
   begin
      Status := Flush (File);
   end Close;

end Filesystem.FAT.Files;
