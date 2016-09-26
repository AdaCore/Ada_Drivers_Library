
with Ada.Unchecked_Conversion;
with FAT_Filesystem;             use FAT_Filesystem;

package body FAT_Filesystem.Files is

   ---------------
   -- File_Open --
   ---------------

   function File_Open
     (Parent : Directory_Entry;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : out File_Handle) return Status_Code
   is
      Node : Directory_Entry;
      Ret  : Status_Code;
   begin
      Ret := Find (Parent, Name, Node);

      if Ret /= OK then
         if Mode = Read_Mode then
            return No_Such_File;
         end if;

         Ret := Create_File_Node (Parent, Name, Node);
      end if;

      if Ret /= OK then
         return Ret;
      end if;

      if Mode = Write_Mode then
         Set_Size (Node, 0);
         --  Free the cluster chain if > 1 cluster
         Ret := Adjust_Clusters (Node);

         if Ret /= OK then
            return Ret;
         end if;
      end if;

      File :=
        (Is_Open         => True,
         FS              => Get_FS (Node),
         Mode            => Mode,
         Current_Cluster => Get_Start_Cluster (Node),
         Current_Block   => 0,
         Buffer          => (others => 0),
         Buffer_Level    => 0,
         Bytes_Total     => 0,
         D_Entry         => Node,
         Parent          => Parent);

      return OK;
   end File_Open;

   ---------------
   -- File_Read --
   ---------------

   function File_Read
     (Handle : in out File_Handle;
      Addr   : System.Address;
      Length : Natural) return Integer
   is
      D : File_Data (1 .. Length) with Import, Address => Addr;
   begin
      return File_Read (Handle, D);
   end File_Read;

   ---------------
   -- File_Read --
   ---------------

   function File_Read
     (Handle : in out File_Handle;
      Data : out File_Data)
      return Integer
   is
      Idx         : Natural;
      --  Index from the current block

      Data_Length : Unsigned_32 := Data'Length;
      --  The total length to read

      Data_Idx    : Natural := Data'First;
      --  Index into the data array of the next bytes to read

      Block_Addr  : Unsigned_32;
      --  The actual address of the block to read

      R_Length    : Natural;
      --  The size of the data to read in one operation

      Status      : Status_Code with Unreferenced;

   begin
      if not Handle.Is_Open or Handle.Mode = Write_Mode then
         return -1;
      end if;

      if Handle.Mode = Read_Write_Mode then
         Status := File_Flush (Handle);
      end if;

      --  Clamp the number of data to read to the size of the file
      if Handle.Bytes_Total + Data'Length > Get_Size (Handle.D_Entry) then
         Data_Length := Get_Size (Handle.D_Entry) - Handle.Bytes_Total;
      end if;

      --  Initialize the current cluster if not already done
      if Handle.Current_Cluster = 0 then
         Handle.Current_Cluster := Get_Start_Cluster (Handle.D_Entry);
      end if;

      loop
         Idx := Natural (Handle.Bytes_Total mod Handle.FS.Bytes_Per_Block);
         Block_Addr := Handle.FS.Cluster_To_Block (Handle.Current_Cluster) +
           Handle.Current_Block;

         if Idx = 0 and then Data_Length >= Handle.FS.Bytes_Per_Block then
            --  Case where the data to read is aligned on a block, and
            --  we have at least one block to read.

            --  Check the compatibility of the User's buffer with DMA transfers
            if (Data'Alignment + Idx * 8) mod 32 = 0 then
               --  User data is aligned on words: we can directly perform DMA
               --  transfers to it
               R_Length :=
                 Natural (Data_Length / Handle.FS.Bytes_Per_Block) *
                 Natural (Handle.FS.Bytes_Per_Block);

               if not Handle.FS.Controller.Read
                 (Block_Addr, Data (Data_Idx .. Data_Idx + R_Length - 1))
               then
                  if Data_Idx = Data'First then
                     --  not a single byte read, report an error
                     return -1;
                  else
                     return Integer (Data_Idx - Data'First);
                  end if;
               end if;

            else
               --  User data is not aligned: we thus have to use the Handle's
               --  cache (512 bytes)
               --  Reading one block
               R_Length := Handle.Buffer'Length;

               --  Fill the buffer
               if not Handle.FS.Controller.Read
                 (Block_Addr, Handle.Buffer)
               then
                  if Data_Idx = Data'First then
                     --  not a single byte read, report an error
                     return -1;
                  else
                     return Integer (Data_Idx - Data'First);
                  end if;
               end if;

               Data (Data_Idx .. Data_Idx + R_Length - 1) := Handle.Buffer;
            end if;

            Data_Idx := Data_Idx + R_Length;
            Handle.Current_Block := Handle.Current_Block + 1;
            Handle.Bytes_Total := Handle.Bytes_Total + Unsigned_32 (R_Length);
            Handle.Buffer_Level := 0;
            Data_Length := Data_Length - Unsigned_32 (R_Length);

         else
            --  Not aligned on a block, or less than 512 bytes to read
            --  We thus need to use our internal buffer.
            if Handle.Buffer_Level = 0 then
               Block_Addr :=
                 Handle.FS.Cluster_To_Block (Handle.Current_Cluster) +
                 Handle.Current_Block;

               if not Handle.FS.Controller.Read
                 (Block_Addr,
                  Handle.Buffer)
               then
                  if Data_Idx = Data'First then
                     --  not a single byte read, report an error
                     return -1;
                  else
                     return Integer (Data_Idx - Data'First);
                  end if;
               end if;

               Handle.Buffer_Level := Handle.Buffer'Length;
            end if;

            R_Length := Natural
              (Unsigned_32'Min (Handle.Buffer'Length - Unsigned_32 (Idx),
                                Data_Length));
            Data (Data_Idx .. Data_Idx + R_Length - 1) :=
              Handle.Buffer (Idx .. Idx + R_Length - 1);

            Data_Idx           := Data_Idx + R_Length;
            Handle.Bytes_Total := Handle.Bytes_Total + Unsigned_32 (R_Length);
            Data_Length        := Data_Length - Unsigned_32 (R_Length);

            if Idx + R_Length = Natural (Handle.FS.Bytes_Per_Block) then
               Handle.Current_Block := Handle.Current_Block + 1;
               Handle.Buffer_Level  := 0;
            end if;
         end if;

         --  Check if we changed cluster
         if Handle.Current_Block =
           Unsigned_32 (Handle.FS.Blocks_Per_Cluster)
         then
            Handle.Current_Cluster := Handle.FS.Get_FAT (Handle.Current_Cluster);
            Handle.Current_Block   := 0;
         end if;

         exit when Data_Length = 0;
         exit when Handle.FS.Is_Last_Cluster (Handle.Current_Cluster);
      end loop;

      return Integer (Data_Idx - Data'First);
   end File_Read;

   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (File : in out File_Handle;
      Data : out T)
   is
      subtype B is File_Data (1 .. T'Size / 8);
      function To_T is new Ada.Unchecked_Conversion (B, T);
      Buf : B;
      Len : Natural;
   begin
      Len := File_Read (File, Buf);
      if Len = B'Length then
         Data := To_T (Buf);
      else
         raise Constraint_Error with "Not enough data in file";
      end if;
   end Generic_Read;

   ----------------
   -- File_Write --
   ----------------

   function File_Write
     (File   : in out File_Handle;
      Data   : File_Data) return Status_Code
   is
      procedure Inc_Size (Amount : Natural);

      Data_Length : Natural := Data'Length;
      --  The total length to read

      Data_Idx    : Natural := Data'First;
      --  Index into the data array of the next bytes to write

      N_Blocks    : Unsigned_32;
      --  The number of blocks to read at once

      Block_Addr  : Unsigned_32;
      --  The actual address of the block to read

      W_Length    : Natural;
      --  The size of the data to write in one operation

      --------------
      -- Inc_Size --
      --------------

      procedure Inc_Size (Amount : Natural)
      is
      begin
         Data_Idx := Data_Idx + Amount;
         File.Bytes_Total  := File.Bytes_Total + Unsigned_32 (Amount);
         Data_Length       := Data_Length - Amount;

         Set_Size (File.D_Entry, File.Bytes_Total);
      end Inc_Size;

   begin
      if not File.Is_Open or File.Mode = Read_Mode then
         return Access_Denied;
      end if;

      --  Initialize the current cluster if not already done
      if File.Current_Cluster = 0 then
         File.Current_Cluster := Get_Start_Cluster (File.D_Entry);
      end if;

      if File.Buffer_Level > 0 then
         --  First fill the buffer
         W_Length := Natural'Min (File.Buffer'Length - File.Buffer_Level,
                                  Data'Length);

         File.Buffer (File.Buffer_Level .. File.Buffer_Level + W_Length - 1) :=
           Data (Data_Idx .. Data_Idx + W_Length - 1);

         File.Buffer_Level := File.Buffer_Level + W_Length;
         Inc_Size (W_Length);

         if File.Buffer_Level > File.Buffer'Last then
            Block_Addr := File.FS.Cluster_To_Block (File.Current_Cluster) +
              File.Current_Block;

            File.Buffer_Level := 0;
            if not File.FS.Controller.Write (Block_Addr, File.Buffer) then
               return Disk_Error;
            end if;

            File.Current_Block := File.Current_Block + 1;

            if File.Current_Block = Unsigned_32 (File.FS.Blocks_Per_Cluster) then
               File.Current_Block := 0;
               File.Current_Cluster := File.FS.Get_FAT (File.Current_Cluster);
            end if;
         end if;

         if Data_Idx > Data'Last then
            return OK;
         end if;
      end if;

      --  At this point, the buffer is empty and a new block is ready to be
      --  written. Check if we can write several blocks at once
      while Unsigned_32 (Data_Length) >= File.FS.Bytes_Per_Block loop
         --  we have at least one full block to write.

         --  Determine the number of full blocks we need to write:
         N_Blocks := Unsigned_32'Min
           (Unsigned_32 (File.FS.Blocks_Per_Cluster) -
                File.Current_Block,
            Unsigned_32 (Data_Length) / File.FS.Bytes_Per_Block);

         --  Writing all blocks in one operation
         W_Length := Natural (N_Blocks * File.FS.Bytes_Per_Block);

         Block_Addr := File.FS.Cluster_To_Block (File.Current_Cluster) +
           File.Current_Block;

         --  Fill directly the user data
         if not File.FS.Controller.Write
           (Block_Addr,
            Data (Data_Idx .. Data_Idx + W_Length - 1))
         then
            return Disk_Error;
         end if;

         Inc_Size (W_Length);

         if File.Current_Block = Unsigned_32 (File.FS.Blocks_Per_Cluster) then
            File.Current_Block := 0;
            File.Current_Cluster := File.FS.Get_FAT (File.Current_Cluster);
         end if;
      end loop;

      --  Now everything that remains is smaller than a block. Let's fill the
      --  buffer with this data
      W_Length := Data'Last - Data_Idx + 1;
      File.Buffer (0 .. W_Length - 1) := Data (Data_Idx .. Data'Last);

      Inc_Size (W_Length);

      File.Buffer_Level := W_Length;

      return OK;
   end File_Write;

   ----------------
   -- File_Flush --
   ----------------

   function File_Flush
     (File : in out File_Handle)
      return Status_Code
   is
      Block_Addr  : Unsigned_32;
      --  The actual address of the block to read
   begin
      if File.Mode = Read_Mode
        or else File.Buffer_Level = 0
      then
         return OK;
      end if;

      Block_Addr := File.FS.Cluster_To_Block (File.Current_Cluster) +
        File.Current_Block;

      if not File.FS.Controller.Write (Block_Addr, File.Buffer) then
         return Disk_Error;
      end if;

      return OK;
   end File_Flush;

   ----------------
   -- File_Close --
   ----------------

   procedure File_Close (File : in out File_Handle) is
      Status : Status_Code with Unreferenced;
   begin
      Status := Update_Entry (File.Parent, File.D_Entry);
      Status := File_Flush (File);
      File.Is_Open := False;
   end File_Close;

   ------------------
   -- To_File_Data --
   ------------------

   function To_File_Data (S : String) return File_Data
   is
      subtype S_Type is String (S'Range);
      subtype D_Type is File_Data (0 .. S'Length - 1);
      function To_Data is new Ada.Unchecked_Conversion (S_Type, D_Type);
   begin
      return To_Data (S);
   end To_File_Data;

end FAT_Filesystem.Files;
