
with System;      use System;
with Ada.Text_IO;

package body Generic_Managed_Buffers_Test is

   procedure Memset (Addr : System.Address;
                     Len  : UInt64;
                     Data : UInt8);

   function Memcheck (Addr : System.Address;
                      Len  : UInt64;
                      Data : UInt8)
                      return Boolean;

   ------------
   -- Memset --
   ------------

   procedure Memset (Addr : System.Address;
                     Len  : UInt64;
                     Data : UInt8)
   is
      type Mapping is array (1 .. Len) of UInt8 with Pack;
      Mem : Mapping with Address => Addr;
   begin
      Mem := (others => Data);
   end Memset;

   --------------
   -- Memcheck --
   --------------

   function Memcheck (Addr : System.Address;
                      Len  : UInt64;
                      Data : UInt8)
                      return Boolean
   is
      type Mapping is array (1 .. Len) of UInt8 with Pack;
      Mem : Mapping with Address => Addr;
   begin
      return (for all Elt of Mem => Elt = Data);
   end Memcheck;

   Buffers : array (1 .. Test_Buffer_Nbr) of Any_Managed_Buffer;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Name : String) is

   begin
      --  Check that no buffer is are allocated
      if Remaining_Buffers /= Test_Buffer_Nbr then
         raise Program_Error with
           Name & ": Remaining should be " & Test_Buffer_Nbr'Img & " is :" &
             Remaining_Buffers'Img;
      end if;

      --  Allocate all managed buffers available
      for Index in Buffers'Range loop
         Buffers (Index) := Allocate_Buffer;
         if Buffers (Index) = null then
            raise Program_Error with Name & ": Unexpected allocation failure";
         end if;

      end loop;

      --  Now that we allocated all of them, check that there's no buffer available
      if Remaining_Buffers /= 0 then
         raise Program_Error with Name & ": Remaining should be zero is :" &
           Remaining_Buffers'Img;
      end if;

      --  Free some buffers
      Buffers (5).Release;
      Buffers (10).Release;
      Buffers (20).Release;

      --  Check that there's three buffers available
      if Remaining_Buffers /= 3 then
         raise Program_Error with Name & ": Remaining should be three is :" &
           Remaining_Buffers'Img;
      end if;

      --  Reallocate the buffers in a different order
      Buffers (10) := Allocate_Buffer;
      Buffers (20) := Allocate_Buffer;
      Buffers (5) := Allocate_Buffer;


      --  Check that we do not allocate the same address twice
      for Index1 in Buffers'Range  loop

         for Index2 in Buffers'Range loop

            if Index1 /= Index2
              and then
                Buffers (Index1).Buffer_Address = Buffers (Index2).Buffer_Address
            then
               raise Program_Error with
               Name & ": Unexpected duplicate address. Buffer" & Index1'Img & " and"  & Index2'Img;
            end if;
         end loop;
      end loop;

      --  Put some data in the buffers
      for Index in Buffers'Range loop
         Memset (Buffers (Index).Buffer_Address,
                 Buffers (Index).Buffer_Length,
                 UInt8 (Index));
      end loop;

      --  Check that the data is correct (no buffer overlaping)
      for Index in Buffers'Range loop
         if not Memcheck (Buffers (Index).Buffer_Address,
                          Buffers (Index).Buffer_Length,
                          UInt8 (Index))
         then
            raise Program_Error with
            Name & ": Unexpected data corruption in buffer nbr:" & Index'Img;
         end if;
      end loop;

      --  Take one more reference for one of the buffers
      Buffers (32).Take;

      for Index in Buffers'Range loop
         Buffers (Index).Release;
      end loop;

      --  Check that only one buffer was not de-allocated
      if Remaining_Buffers /= Test_Buffer_Nbr - 1 then
         raise Program_Error with
         Name & ": Remaining should be" & Natural'Image (Test_Buffer_Nbr - 1)
           & " is :" & Remaining_Buffers'Img;
      end if;

      --  Release the last reference for one of the buffers
      Buffers (32).Release;

      --  Check that all buffers are de-allocated
      if Remaining_Buffers /= Test_Buffer_Nbr then
         raise Program_Error with
           Name & ": Remaining should be " & Test_Buffer_Nbr'Img & " is :" &
             Remaining_Buffers'Img;
      end if;

      Ada.Text_IO.Put_Line (Name & " PASS");
   end Run_Test;
end Generic_Managed_Buffers_Test;
