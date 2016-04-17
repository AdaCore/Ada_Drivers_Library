with Ada.Real_Time;        use Ada.Real_Time;
with Interfaces.Bit_Types; use Interfaces.Bit_Types;
with STM32.Board;
with LIS3DSH;

procedure Main is
   use type Byte;
   use type LIS3DSH.Axis_Acceleration;

   Id     : Byte;
   Values : LIS3DSH.Axes_Accelerations;
   Threshold_High : constant LIS3DSH.Axis_Acceleration :=  200;
   Threshold_Low  : constant LIS3DSH.Axis_Acceleration := -200;

   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;
begin
   STM32.Board.Initialize_LEDs;
   STM32.Board.Initialize_Accelerometer;
   loop
      Id := STM32.Board.Accelerometer.Device_Id;
      if Id /= LIS3DSH.I_Am_LIS3DSH then
         STM32.Board.All_LEDs_On;
         My_Delay (100);
         STM32.Board.All_LEDs_Off;
         My_Delay (100);
      else
         STM32.Board.Accelerometer.Get_Accelerations (Values);
         if abs Values.X > abs Values.Y then
            if Values.X > Threshold_High then
               STM32.Board.Red.Set;
            elsif Values.X < Threshold_Low then
               STM32.Board.Green.Set;
            end if;
            My_Delay (10);
         else
            if Values.Y > Threshold_High then
               STM32.Board.Orange.Set;
            elsif Values.Y < Threshold_Low then
               STM32.Board.Blue.Set;
            end if;
            My_Delay (10);
         end if;
      end if;
      STM32.Board.All_LEDs_Off;
   end loop;
end Main;
