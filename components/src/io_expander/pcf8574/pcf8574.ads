--
--  Copyright 2022 (C) Rolf Ebert
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;

-- I2C 8-bit IO expander with quasi bidirectional I/O, no data
-- direction, no latch

package PCF8574 is

   subtype PCF8574_Address is I2C_Address range 16#40# .. 16#5F#;

   type PCF8574_Module (Port : not null Any_I2C_Port;
                        Addr : I2C_Address) is tagged private;

   type Any_PCF8574_Module is access all PCF8574_Module'Class;


   procedure Set (This : PCF8574_Module; Data : UInt8);

   function Get (This : PCF8574_Module) return UInt8;
   procedure Get (This : PCF8574_Module; Data : out UInt8);
   -- when reading the input from keys (buttons) carefully read the
   -- datasheet. The input line should be set high before reading.
   -- E.g. if all lines are key input:
   -- M.Set (16#FF#);
   -- Keys := M.Get;

private

   type PCF8574_Module (Port : not null Any_I2C_Port;
                        Addr : I2C_Address) is tagged null record;

end PCF8574;
