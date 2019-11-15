------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

package body IS31FL3731 is

   subtype Dispatch is Device'Class;

   --  Dev_Address : constant UInt10 := 16#E8#;
   Control_Bank : constant UInt8 := 16#0B#;

   Enable_Offset : constant := 16#00#;
   Blink_Offset  : constant := 16#12#;
   Bright_Offset : constant := 16#24#;

   procedure Select_Bank (This : in out Device;
                          Bank :        UInt8);
   procedure Write (This : in out Device;
                    Data :        UInt8_Array);

   procedure Read (This : in out Device;
                   Reg  :        UInt8;
                   Data :    out UInt8)
     with Unreferenced;

   function Dev_Address (This : Device) return I2C_Address
   is (I2C_Address ((16#74# or UInt10 (This.AD)) * 2));

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Device;
                    Data :        UInt8_Array)
   is
      Status : I2C_Status;
   begin
      This.Port.Master_Transmit (This.Dev_Address, Data, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Device;
                   Reg  :        UInt8;
                   Data :    out UInt8)
   is
      Status : I2C_Status;
      Tmp    : UInt8_Array (0 .. 0);
   begin
      This.Write ((0 => Reg));

      This.Port.Master_Receive (This.Dev_Address, Tmp, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      Data := Tmp (Tmp'First);
   end Read;

   -----------------This.
   -- Select_Bank --
   -----------------

   procedure Select_Bank (This : in out Device;
                          Bank :        UInt8)
   is
   begin
      This.Write ((0 => 16#FD#, 1 => Bank));
   end Select_Bank;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Device) is
   begin
      This.Select_Bank (Control_Bank);

      --  Disable Shutdown
      This.Write ((0 => 16#0A#, 1 => 16#01#));

      --  Picture mode
      This.Write ((0 => 16#00#, 1 => 16#00#));

      --  Blink mode
      This.Write ((0 => 16#05#, 1 => 16#00#));

      for Frame in Frame_Id loop
         This.Select_Bank (UInt8 (Frame));
         This.Clear;
      end loop;

      This.Select_Bank (UInt8 (This.Frame_Sel));
   end Initialize;

   -------------------
   -- Display_Frame --
   -------------------

   procedure Display_Frame (This  : in out Device;
                            Frame :        Frame_Id)
   is
   begin
      This.Select_Bank (Control_Bank);

      This.Write ((0 => 16#01#, 1 => UInt8 (Frame)));

      This.Select_Bank (UInt8 (This.Frame_Sel));
   end Display_Frame;

   ------------------
   -- Select_Frame --
   ------------------

   procedure Select_Frame (This  : in out Device;
                           Frame :        Frame_Id)
   is
   begin
      This.Frame_Sel := Frame;
      This.Select_Bank (UInt8 (This.Frame_Sel));
   end Select_Frame;

   ----------
   -- Fill --
   ----------

   procedure Fill (This       : in out Device;
                   Brightness :        UInt8;
                   Blink      :        Boolean := False)
   is
   begin
      --  Enable all LEDs
      This.Write ((0       => Enable_Offset,
                   1 .. 18 => 16#FF#));
      This.Enable_Bitmask (This.Frame_Sel) := (others => 16#FF#);

      --  Set all blink
      This.Write ((0       => Blink_Offset,
                   1 .. 18 => (if Blink then 16#FF# else 16#00#)));
      This.Blink_Bitmask (This.Frame_Sel) :=
        (others => (if Blink then 16#FF# else 16#00#));

      --  Set all brighness
      This.Write ((0        => Bright_Offset,
                   1 .. 144 => Brightness));
   end Fill;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Device) is
   begin
      --  Disable all LEDs
      This.Write ((0       => Enable_Offset,
                   1 .. 18 => 0));
      This.Enable_Bitmask (This.Frame_Sel) := (others => 0);

      --  Disable all blink
      This.Write ((0       => Blink_Offset,
                   1 .. 18 => 0));
      This.Blink_Bitmask (This.Frame_Sel) := (others => 0);

      --  Set all brighness to zero
      This.Write ((0        => Bright_Offset,
                   1 .. 144 => 0));
   end Clear;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Device;
                     X    :        X_Coord;
                     Y    :        Y_Coord)
   is
      LED        : constant LED_Id := Dispatch (This).LED_Address (X, Y);
      Reg_Offset : constant UInt8 := UInt8 (LED) / 8;
      Bit        : constant UInt8 := Shift_Left (UInt8 (1), Integer (LED) mod 8);

      Mask       : UInt8 renames This.Enable_Bitmask
        (This.Frame_Sel) (LED_Bitmask_Index (Reg_Offset));
   begin
      Mask := Mask or Bit;
      This.Write ((0 => Enable_Offset + Reg_Offset, 1 => Mask));
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Device;
                      X    :        X_Coord;
                      Y    :        Y_Coord)
   is
      LED        : constant LED_Id := Dispatch (This).LED_Address (X, Y);
      Reg_Offset : constant UInt8 := UInt8 (LED) / 8;
      Bit        : constant UInt8 := Shift_Left (UInt8 (1), Integer (LED) mod 8);

      Mask       : UInt8 renames This.Enable_Bitmask
        (This.Frame_Sel) (LED_Bitmask_Index (Reg_Offset));
   begin
      Mask := Mask and not Bit;
      This.Write ((0 => Enable_Offset + Reg_Offset, 1 => Mask));
   end Disable;

   --------------------
   -- Set_Brightness --
   --------------------

   procedure Set_Brightness (This : in out Device;
                             X    :        X_Coord;
                             Y    :        Y_Coord;
                             Brightness : UInt8)
   is
      LED : constant LED_Id := Dispatch (This).LED_Address (X, Y);
   begin
      This.Write ((0  => Bright_Offset + UInt8 (LED),
                   1  => Brightness));
   end Set_Brightness;

   ------------------
   -- Enable_Blink --
   ------------------

   procedure Enable_Blink (This : in out Device;
                           X    :        X_Coord;
                           Y    :        Y_Coord)
   is
      LED        : constant LED_Id := Dispatch (This).LED_Address (X, Y);
      Reg_Offset : constant UInt8 := UInt8 (LED) / 8;
      Bit        : constant UInt8 := Shift_Left (UInt8 (1), Integer (LED) mod 8);

      Mask       : UInt8 renames This.Blink_Bitmask
        (This.Frame_Sel) (LED_Bitmask_Index (Reg_Offset));
   begin
      Mask := Mask or Bit;
      This.Write ((0 => Blink_Offset + Reg_Offset, 1 => Mask));
   end Enable_Blink;

   -------------------
   -- Disable_Blink --
   -------------------

   procedure Disable_Blink (This : in out Device;
                            X    :        X_Coord;
                            Y    :        Y_Coord)
   is
      LED        : constant LED_Id := Dispatch (This).LED_Address (X, Y);
      Reg_Offset : constant UInt8 := UInt8 (LED) / 8;
      Bit        : constant UInt8 := Shift_Left (UInt8 (1), Integer (LED) mod 8);

      Mask       : UInt8 renames This.Blink_Bitmask
        (This.Frame_Sel) (LED_Bitmask_Index (Reg_Offset));
   begin
      Mask := Mask or Bit;
      This.Write ((0 => Blink_Offset + Reg_Offset, 1 => Mask));
   end Disable_Blink;

   --------------------
   -- Set_Blink_Rate --
   --------------------

   procedure Set_Blink_Rate (This : in out Device;
                             A    :        UInt3)
   is
   begin
      This.Select_Bank (Control_Bank);

      if A = 0 then
         This.Write ((0 => 16#05#,
                      1 => 2#0000_0000#));
      else
         This.Write ((0 => 16#05#,
                      1 => 2#0000_1000# or UInt8 (A)));
      end if;

      This.Select_Bank (UInt8 (This.Frame_Sel));
   end Set_Blink_Rate;

end IS31FL3731;
