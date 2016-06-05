with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package HT16K33 is

   type HT16K33_Device (Port : not null I2C_Port_Ref;
                        Addr : UInt3) is tagged private;

   type HT16K33_Brightness is new Positive range 1 .. 16;
   type HT16K33_Blink is (Blink_Off, Blink_2hz, Blink_1hz, Blink_05hz);

   subtype LED_Row_Addr is UInt4;
   subtype LED_Column_Addr is UInt3;

   subtype Key_Row_Addr is Integer range 0 .. 5;
   subtype Key_Column_Addr is UInt3;

   procedure Enable (This : in out HT16K33_Device);
   procedure Disable (This : in out HT16K33_Device);

   procedure Set_Brightness (This       : in out HT16K33_Device;
                             Brightness : HT16K33_Brightness);

   procedure Set_Blink (This  : in out HT16K33_Device;
                        Blink : HT16K33_Blink);

   procedure Set_LED (This   : in out HT16K33_Device;
                      Row    : LED_Row_Addr;
                      Column : LED_Column_Addr;
                      Enable : Boolean := True);
   --  This procedure only changes the internal buffer, use the Update_LEDs
   --  procedure to actually update the LEDs state on the device.

   procedure Set_Row (This : in out HT16K33_Device;
                      Addr : LED_Row_Addr;
                      Row  : Byte);
   --  This procedure only changes the internal buffer, use the Update_LEDs
   --  procedure to actually update the LEDs state on the device.

   function Get_Key (This   : in out HT16K33_Device;
                     Row    : Key_Row_Addr;
                     Column : Key_Column_Addr) return Boolean;
   --  This function only reads the internal buffer, use the Update_Keys
   --  procedure to synchronize keys state from the device.

   procedure Update_LEDs (This : in out HT16K33_Device);
   --  Send internal LED state buffer to the device

   procedure Update_Keys (This : in out HT16K33_Device);
   --  Update internal keys status buffer with data from the device.
   --  Wait about 5ms between each key update to avoid false positive and false
   --  negative.

private

   for HT16K33_Blink use (Blink_Off  => 2#00#,
                          Blink_2hz  => 2#01#,
                          Blink_1hz  => 2#10#,
                          Blink_05hz => 2#11#);

   type HT16K33_Device (Port : not null I2C_Port_Ref;
                        Addr : UInt3) is tagged
      record
         Enabled : Boolean := False;
         Blink   : HT16K33_Blink := Blink_Off;
         LEDs    : I2C_Data (0 .. 15) := (others => 0);
         Keys    : I2C_Data (Key_Row_Addr) := (others => 0);
   end record;
end HT16K33;
