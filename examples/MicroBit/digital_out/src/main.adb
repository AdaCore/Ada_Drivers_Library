with MicroBit.IOs;
with MicroBit.Time;

procedure Main is
begin

   --  Loop forever
   loop
      --  Turn on the LED connected to pin 0
      MicroBit.IOs.Set (0, True);

      --  Wait 500 milliseconds
      MicroBit.Time.Delay_Ms (500);

      --  Turn off the LED connected to pin 0
      MicroBit.IOs.Set (0, False);

      --  Wait 500 milliseconds
      MicroBit.Time.Delay_Ms (500);
   end loop;
end Main;
