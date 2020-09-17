------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--                                                                          --
--  This is a driver for the PCD8544 monochrome LCD controller              --
--                                                                          --
--  Datasheet:                                                              --
--  https://www.sparkfun.com/datasheets/LCD/Monochrome/Nokia5110.pdf        --
--                                                                          --
--  Use a SPI clock of 4 MHz or less. The SPI peripheral should be          --
--  configured to transmit MSB-first, 8 bit data size, CPOL=0, CPHA=1       --
--  This is sometimes referred to as a "Mode 1" SPI configuration.          --
--                                                                          --
--  Minimum transition time for RST, CS, and DC is 100ns. HAL.Time doesn't  --
--  support nanosecond delays, so this module uses a delay of 1 microsecond --
--  instead.                                                                --
--                                                                          --
--  CS may be null if it's connected to the SPI controller or tied to       --
--  ground.                                                                 --
--                                                                          --
--  RST may be null if it's controlled externally or you only expect to     --
--  Initialize once per power cycle.                                        --
------------------------------------------------------------------------------

with HAL;                  use HAL;
with HAL.GPIO;             use HAL.GPIO;
with HAL.SPI;              use HAL.SPI;
with HAL.Framebuffer;      use HAL.Framebuffer;
with HAL.Bitmap;           use HAL.Bitmap;
with HAL.Time;             use HAL.Time;
with Memory_Mapped_Bitmap; use Memory_Mapped_Bitmap;
with PCD8544_Reg;          use PCD8544_Reg;

package PCD8544 is
   type PCD8544_Device
     (Port    : not null Any_SPI_Port;
      DC      : not null Any_GPIO_Point;
      RST, CS : Any_GPIO_Point;
      Time    : not null HAL.Time.Any_Delays)
   is limited new HAL.Framebuffer.Frame_Buffer_Display with private;

   type Any_PCD8544_Device is access all PCD8544_Device'Class;

   --  See datasheet section 8.9 for contrast (Vop) values
   subtype PCD8544_Contrast is UInt8 range 0 .. 127;
   PCD8544_Default_Contrast : constant PCD8544_Contrast := 48;

   --  See datasheet Table 4 for bias mux rate values. We default to 1:48 as
   --  that reportedly works well on most LCDs.
   subtype PCD8544_Bias is UInt8 range 0 .. 4;
   PCD8544_Default_Bias : constant PCD8544_Bias := 3;

   --  See datasheet Figure 7 for TC values. Use higher values at lower
   --  temperatures.
   subtype PCD8544_Temperature_Coefficient is UInt8 range 0 .. 3;
   PCD8544_Default_Temperature_Coefficient :
       constant PCD8544_Temperature_Coefficient := 0;

   SPI_Error : exception;

   Device_Width  : constant := 48;
   Device_Height : constant := 84;

   procedure Initialize
       (This : in out PCD8544_Device);

   procedure Set_Contrast
       (This     : in out PCD8544_Device;
        Contrast : in PCD8544_Contrast);

   procedure Set_Bias
       (This : in out PCD8544_Device;
        Bias : in PCD8544_Bias);

   procedure Set_Temperature
       (This : in out PCD8544_Device;
        TC   : in PCD8544_Temperature_Coefficient);

   procedure Set_Display_Mode
       (This   : in out PCD8544_Device;
        Enable : in Boolean;
        Invert : in Boolean);

   overriding
   function Max_Layers (This : PCD8544_Device) return Positive;

   overriding
   function Supported
       (This : PCD8544_Device;
        Mode : FB_Color_Mode)
       return Boolean;

   overriding
   procedure Set_Orientation
       (This        : in out PCD8544_Device;
        Orientation : Display_Orientation);

   overriding
   procedure Set_Mode
       (This : in out PCD8544_Device;
        Mode : Wait_Mode);

   overriding
   function Initialized
       (This : PCD8544_Device) return Boolean;

   overriding
   function Width
       (This : PCD8544_Device) return Positive;

   overriding
   function Height
       (This : PCD8544_Device) return Positive;

   overriding
   function Swapped
       (This : PCD8544_Device) return Boolean;

   overriding
   procedure Set_Background
       (This    : PCD8544_Device;
        R, G, B : UInt8);

   overriding
   procedure Initialize_Layer
       (This          : in out PCD8544_Device;
        Layer         : Positive;
        Mode          : FB_Color_Mode;
        X, Y          : Natural := 0;
        Width, Height : Positive := Positive'Last);

   overriding
   function Initialized
       (This  : PCD8544_Device;
        Layer : Positive) return Boolean;

   overriding
   procedure Update_Layer
       (This      : in out PCD8544_Device;
        Layer     : Positive;
        Copy_Back : Boolean := False);

   overriding
   procedure Update_Layers
       (This : in out PCD8544_Device);

   overriding
   function Color_Mode
       (This  : PCD8544_Device;
        Layer : Positive) return FB_Color_Mode;

   overriding
   function Hidden_Buffer
       (This  : in out PCD8544_Device;
        Layer : Positive)
       return not null HAL.Bitmap.Any_Bitmap_Buffer;

   overriding
   function Pixel_Size
       (Display : PCD8544_Device;
        Layer   : Positive) return Positive;

private

   type PCD8544_Bitmap_Buffer is new Memory_Mapped_Bitmap_Buffer
       with record
         Data : UInt8_Array (1 .. ((Device_Width * Device_Height) / UInt8'Size));
       end record;

   type PCD8544_Device
     (Port    : not null Any_SPI_Port;
      DC      : not null Any_GPIO_Point;
      RST, CS : Any_GPIO_Point;
      Time    : not null HAL.Time.Any_Delays)
   is limited new HAL.Framebuffer.Frame_Buffer_Display with
      record
         Memory_Layer       : aliased PCD8544_Bitmap_Buffer;
         Device_Initialized : Boolean := False;
         Layer_Initialized  : Boolean := False;
         FR                 : PCD8544_Function_Register;
         DR                 : PCD8544_Display_Register;
      end record;

   procedure Chip_Select (This : PCD8544_Device; Enabled : in Boolean);
   procedure Reset (This : in out PCD8544_Device);
   procedure Transmit (This : PCD8544_Device; Data : in UInt8);
   procedure Transmit (This : PCD8544_Device; Data : in UInt8_Array);
   procedure Extended_Mode (This : in out PCD8544_Device);
   procedure Basic_Mode (This : in out PCD8544_Device);
   procedure Write_Raw_Pixels (This : in out PCD8544_Device; Data : in UInt8_Array);
end PCD8544;
