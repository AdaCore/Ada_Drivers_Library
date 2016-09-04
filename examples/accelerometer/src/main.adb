with Ada.Real_Time; use Ada.Real_Time;
with HAL;           use HAL;
with STM32.Board;   use STM32.Board;
with LIS3DSH;       use LIS3DSH;
with Test_I2C_Proxies;

procedure Main is
   use type Byte;

   Values : LIS3DSH.Axes_Accelerations;

   Threshold_High : constant LIS3DSH.Axis_Acceleration :=  200;
   Threshold_Low  : constant LIS3DSH.Axis_Acceleration := -200;

   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;

begin

   Test_I2C_Proxies.Start_Test;

   Initialize_LEDs;

   Initialize_Accelerometer;

   Accelerometer.Configure
     (Output_DataRate => Data_Rate_100Hz,
      Axes_Enable     => XYZ_Enabled,
      SPI_Wire        => Serial_Interface_4Wire,
      Self_Test       => Self_Test_Normal,
      Full_Scale      => Fullscale_2g,
      Filter_BW       => Filter_800Hz);

   if Accelerometer.Device_Id /= I_Am_LIS3DSH then
      loop
         All_LEDs_On;
         My_Delay (100);
         All_LEDs_Off;
         My_Delay (100);
      end loop;
   end if;

   loop
      Accelerometer.Get_Accelerations (Values);
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
      All_LEDs_Off;
   end loop;
end Main;
