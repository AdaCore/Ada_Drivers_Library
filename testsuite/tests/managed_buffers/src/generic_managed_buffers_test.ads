
with HAL;                       use HAL;
with HAL.Managed_Buffers;       use HAL.Managed_Buffers;

generic
   Test_Buffer_Nbr  : Positive;
   with function Allocate_Buffer return Any_Managed_Buffer;
   with function Remaining_Buffers return Natural;
package Generic_Managed_Buffers_Test is

   procedure Run_Test (Name : String);

end Generic_Managed_Buffers_Test;
