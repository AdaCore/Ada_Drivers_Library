--
--  Copyright 2022 (C) Rolf Ebert
--
--  SPDX-License-Identifier: BSD-3-Clause
--


package body PCF8574 is

   -----------------
   --  Configure  --
   -----------------

   procedure Configure (This : in out PCF8574_Module;
                        Port : Any_I2C_Port;
                        Addr : PCF8574_Address)
   is begin
      This.Port := Port;
      This.Addr := Addr;
   end Configure;

   -----------
   --  Get  --
   -----------

   function Get (This : PCF8574_Module) return UInt8
   is
      Val    : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      This.Port.Receive (This.Addr, Val, Status);
      return Val(1);
   end Get;

   procedure Get (This : Module; Data : out UInt8)
   is begin
      Data := Get (This);
   end Get;

   -----------
   --  Set  --
   -----------

   procedure Set (This : PCF8574_Module; Data : UInt8)
   is
      Status : I2C_Status;
   begin
      This.Port.Transmit (This.Addr, (1=>Data), Status);
   end Set;

end PCF8574;
