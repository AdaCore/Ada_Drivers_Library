--
--  Copyright 2022 (C) Rolf Ebert
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;
with HAL.I2C.Master;    use HAL.I2C.Master;

-- I2C 8-bit IO expander with quasi bidirectional I/O, no data
-- direction, no latch

package PCF8574 is

   subtype Module_Address is I2C_7bit_Address range 16#20# .. 16#2F#;

   type Module is tagged private;
   type Any_Module is access all Module'Class;

   procedure Configure (This : in out Module;
                        Port : Any_I2C_Master_Port;
                        Addr : Module_Address);

   procedure Set (This : Module; Data : UInt8);

   function Get (This : Module) return UInt8;
   procedure Get (This : Module; Data : out UInt8);
   -- when reading the input from keys (buttons) carefully read the
   -- datasheet. The input line should be set high before reading.
   -- E.g. if all lines are key input:
   -- M.Set (16#FF#);
   -- Keys := M.Get;

private
   type Module is tagged record
      Port : Any_I2C_Master_Port;
      Addr : Module_Address;
   end record;

end PCF8574;
