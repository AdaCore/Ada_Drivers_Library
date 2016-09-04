with I2C_Port_Printer; use I2C_Port_Printer;
with Proxies.I2C; use Proxies.I2C;
with Semihosting;
with System;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package Test_I2C_Proxies is

   procedure Start_Test;

private

   task type I2C_Worker (Prio      : System.Priority;
                         Period_Ms : Positive;
                         Sync      : not null access Suspension_Object) is
   end I2C_Worker;

   S1 : aliased Suspension_Object;
   S2 : aliased Suspension_Object;
   S3 : aliased Suspension_Object;
   S4 : aliased Suspension_Object;
   S5 : aliased Suspension_Object;
   S6 : aliased Suspension_Object;

   T1 : I2C_Worker (1, 100, S1'Access);
   T2 : I2C_Worker (2, 200, S2'Access);
   T3 : I2C_Worker (3, 300, S3'Access);
   T4 : I2C_Worker (4, 400, S4'Access);
   T5 : I2C_Worker (5, 500, S5'Access);
   T6 : I2C_Worker (6, 600, S6'Access);

   The_Printer : aliased I2C_Printer (Real_Port => null,
                                      Put_Line  => Semihosting.Log_Line'Access);
   The_Proxy   : I2C_Proxy (Target_Port      => The_Printer'Access,
                            Ceiling_Priority => System.Priority'Last);
end Test_I2C_Proxies;
