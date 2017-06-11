
with HAL;                       use HAL;
with HAL.Managed_Buffers;       use HAL.Managed_Buffers;

with Static_Anonynous_Managed_Buffers;
with Synchronized_Static_Anonynous_Managed_Buffers;

with Dynamic_Anonynous_Managed_Buffers;
with Synchronized_Dynamic_Anonynous_Managed_Buffers;

with Generic_Managed_Buffers_Test;
with System;

package Test is

   Test_Buffer_Size : constant := 10;
   Test_Buffer_Nbr  : constant := 64;

   type Test_SAMB_Data_Array is array (1 .. Test_Buffer_Size * Test_Buffer_Nbr)
     of HAL.UInt8 with Pack;

   Test_SAMB_Data : Test_SAMB_Data_Array;

   package SAMB is new Static_Anonynous_Managed_Buffers
     (Mem_Start   => Test_SAMB_Data'Address,
      Buffers_Len => Test_Buffer_Size,
      Capacity    => Test_Buffer_Nbr);

   package Test_SAMB is new Generic_Managed_Buffers_Test
     (Test_Buffer_Nbr   => Test_Buffer_Nbr,
      Allocate_Buffer   => SAMB.Allocate,
      Remaining_Buffers => SAMB.Remaining);

   package DAMB is new Dynamic_Anonynous_Managed_Buffers
     (Buffers_Len => Test_Buffer_Size,
      Capacity    => Test_Buffer_Nbr);

   package Test_DAMB is new Generic_Managed_Buffers_Test
     (Test_Buffer_Nbr   => Test_Buffer_Nbr,
      Allocate_Buffer   => DAMB.Allocate,
      Remaining_Buffers => DAMB.Remaining);

   type Test_SSAMB_Data_Array is array (1 .. Test_Buffer_Size * Test_Buffer_Nbr)
     of HAL.UInt8 with Pack;

   Test_SSAMB_Data : Test_SSAMB_Data_Array;

   package SSAMB is new Synchronized_Static_Anonynous_Managed_Buffers
     (Mem_Start   => Test_SSAMB_Data'Address,
      Buffers_Len => Test_Buffer_Size,
      Capacity    => Test_Buffer_Nbr,
      Ceiling     => System.Default_Priority);

   package Test_SSAMB is new Generic_Managed_Buffers_Test
     (Test_Buffer_Nbr   => Test_Buffer_Nbr,
      Allocate_Buffer   => SSAMB.Allocate,
      Remaining_Buffers => SSAMB.Remaining);

   package SDAMB is new Synchronized_Dynamic_Anonynous_Managed_Buffers
     (Buffers_Len => Test_Buffer_Size,
      Capacity    => Test_Buffer_Nbr,
      Ceiling     => System.Default_Priority);

   package Test_SDAMB is new Generic_Managed_Buffers_Test
     (Test_Buffer_Nbr   => Test_Buffer_Nbr,
      Allocate_Buffer   => SDAMB.Allocate,
      Remaining_Buffers => SDAMB.Remaining);


end Test;
