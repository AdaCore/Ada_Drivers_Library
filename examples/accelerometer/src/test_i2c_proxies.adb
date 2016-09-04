with Ada.Real_Time; use Ada.Real_Time;
with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package body Test_I2C_Proxies is

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test is
   begin
      Set_True (S1);
      Set_True (S2);
      Set_True (S3);
      Set_True (S4);
      Set_True (S5);
      Set_True (S6);

      delay until Clock + Seconds (10);
   end Start_Test;

   ----------------
   -- I2C_Worker --
   ----------------

   task body I2C_Worker is
      Next_Start : Time;
      Status     : I2C_Status;
      Data       : I2C_Data (1 .. 4);
   begin

      Suspend_Until_True (Sync.all);

      Next_Start := Clock;
      loop
         Next_Start := Next_Start + Milliseconds (Period_Ms);

         --  Create some fake I2C trafic...

         Proxies.I2C.Master_Receive (The_Proxy,
                                     I2C_Address (Period_Ms),
                                     Data,
                                     Status,
                                     1000);

         Proxies.I2C.Master_Transmit (The_Proxy,
                                      I2C_Address (Period_Ms),
                                      Data,
                                      Status,
                                      1000);

         Proxies.I2C.Mem_Read (The_Proxy,
                               I2C_Address (Period_Ms),
                               Short (Period_Ms),
                               Memory_Size_16b,
                               Data,
                               Status,
                               1000);

         Proxies.I2C.Mem_Write (The_Proxy,
                                I2C_Address (Period_Ms),
                                Short (Period_Ms),
                                Memory_Size_8b,
                                Data,
                                Status,
                                1000);
         delay until Next_Start;
      end loop;
   end I2C_Worker;

end Test_I2C_Proxies;
