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

--  Driver for the IS31FL3731 matrix LED driver

with HAL;     use HAL;
with HAL.I2C; use HAL.I2C;

package IS31FL3731 is

   type Device
     (Port : not null Any_I2C_Port;
      AD   : UInt2)
   is abstract tagged limited private;

   subtype X_Coord is Natural;
   subtype Y_Coord is Natural;

   type Frame_Id is range 0 .. 7;
   --  Identify one of the 8 frames available on the IS31FL3731

   procedure Initialize (This : in out Device);
   --  Initialize the device and clear all the frames

   procedure Display_Frame (This  : in out Device;
                            Frame :        Frame_Id);
   --  Set the frame to be displayed

   procedure Select_Frame (This  : in out Device;
                           Frame :        Frame_Id);
   --  Select the frame to be modified

   procedure Fill (This       : in out Device;
                   Brightness :        UInt8;
                   Blink      :        Boolean := False);
   --  Turn on and set brighness for all the LEDs at once in the selected frame

   procedure Clear (This : in out Device);
   --  Turn off all the LEDs at once in the selected frame

   procedure Enable (This : in out Device;
                     X    :        X_Coord;
                     Y    :        Y_Coord);
   --  Turn on a given LED in the selected frame

   procedure Disable (This : in out Device;
                      X    :        X_Coord;
                      Y    :        Y_Coord);
   --  Turn off a given LED in the selected frame

   procedure Set_Brightness (This       : in out Device;
                             X          :        X_Coord;
                             Y          :        Y_Coord;
                             Brightness :        UInt8);
   --  Set the brightness of a given LED in the selected frame

   procedure Enable_Blink (This : in out Device;
                           X    :        X_Coord;
                           Y    :        Y_Coord);
   --  Enable blink of a given LED in the selected frame

   procedure Disable_Blink (This : in out Device;
                            X    :        X_Coord;
                            Y    :        Y_Coord);
   --  Disable blink of a given LED in the selected frame

   procedure Set_Blink_Rate (This : in out Device;
                             A    :        UInt3);
   --  Set global blink rate. Rate = A x 0.27s


   -- Implementation Specific --

   type LED_Id is range 0 .. 144;
   --  Identify one of the 145 LED that can be driven by the IS31FL3731.
   --  This type is used internally in the conversion from X/Y coords.

   function LED_Address (This : Device;
                         X    : X_Coord;
                         Y    : Y_Coord)
                         return LED_Id
                         is abstract;
   --  This function has to be implemented for specific arrangement of LEDs

private

   type LED_Bitmask_Index is range 0 .. 17;
   type LED_Bitmask_Array is array (LED_Bitmask_Index) of UInt8;
   type LED_Bitmasks is array (Frame_Id) of LED_Bitmask_Array;

   type Device
     (Port : not null Any_I2C_Port;
      AD   : UInt2)
   is abstract tagged limited record
      Frame_Sel : Frame_Id := 0;

      Enable_Bitmask : LED_Bitmasks := (others => (others => 0));
      --  Store the state of the Enable bitmasks so we don't have to read it
      --  from the device each time.

      Blink_Bitmask  : LED_Bitmasks := (others => (others => 0));
      --  Store the state of the Blink bitmasks so we don't have to read it
      --  from the device each time.
   end record;

end IS31FL3731;
