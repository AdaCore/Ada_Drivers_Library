with HT16K33; use HT16K33;
with HAL;     use HAL;

package AdaFruit_Trellis is

   type Trellis_Device is new HT16K33_Device with private;

   type Trellis_Coord is new UInt2;

   procedure Set_LED (This   : in out Trellis_Device;
                      X, Y   : Trellis_Coord;
                      Enable : Boolean := True);
   --  This procedure only changes the internal buffer, use the Update_LEDs
   --  procedure to actually update the LEDs state on the device.

   function Key (This : in out Trellis_Device;
                 X, Y : Trellis_Coord) return Boolean;
   --  This function only reads the internal buffer, use the Update_Keys
   --  procedure to synchronize keys state from the device.

private
   type Trellis_Device is new HT16K33_Device with null record;
end AdaFruit_Trellis;
