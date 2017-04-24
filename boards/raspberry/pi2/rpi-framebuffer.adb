------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Text_IO;              use Ada.Text_IO;

with RPi.DMA;                  use RPi.DMA;
with RPi.Firmware;             use RPi.Firmware;
with Rpi_Board;

package body RPi.Framebuffer is

   Debug : constant Boolean := False;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display     : in out Framebuffer_Display;
      Width       : Natural;
      Height      : Natural;
      Color_Depth : Color_Depth_Type)
   is
      Alloc_Data          : UInt32_Array := (16, 0);
      Alloc_Data_Offset   : Natural;
      Physical_Size_Data  : constant UInt32_Array :=
                              (UInt32 (Width), UInt32 (Height));
      Virtual_Size_Data   : constant UInt32_Array :=
                              (UInt32 (Width), UInt32 (Height * 2));
      Virtual_Offset_Data : constant UInt32_Array := (0, 0);
      Depth_Data          : constant UInt32 := 8 * (UInt32 (Color_Depth));
      Generic_Offset      : Natural with Unreferenced;
      Color_Mode          : constant HAL.Bitmap.Bitmap_Color_Mode :=
                              (case Color_Depth is
                                  when 1 => L_8,
                                  when 2 => RGB_565,
                                  when 3 => RGB_888,
                                  when 4 => ARGB_8888);

      FB_Addr : BUS_Address;

   begin
      RPi.Firmware.Lock;

      Alloc_Data_Offset :=
        Add_Message (Tag_Allocate_Buffer, Alloc_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Physical_Display_Size, Physical_Size_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Virtual_Buffer_Size, Virtual_Size_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Virtual_Offset, Virtual_Offset_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Depth, Depth_Data);

      RPi.Firmware.Do_Transaction;

      Alloc_Data := Get_Result (Alloc_Data_Offset, 2);

      RPi.Firmware.Unlock;

      --  Map to uncached address
      FB_Addr := BUS_Address (Alloc_Data (0));
      Display.FB :=
        (1 => (Addr       => To_ARM (FB_Addr),
               Width      => Width,
               Height     => Height,
               Color_Mode => Color_Mode,
               Swapped    => False),
         2 => (Addr       => To_ARM (FB_Addr +
                 BUS_Address (Width * Height * Natural (Color_Depth))),
               Width      => Width,
               Height     => Height,
               Color_Mode => Color_Mode,
               Swapped    => False));

      if Debug then
         Put ("FB BUS address: 0x");
         Put_Line (Image8 (UInt32 (FB_Addr)));
         Put ("FB size: 0x");
         Put_Line (Image8 (Alloc_Data (1)));
      end if;

      Display.Width         := Width;
      Display.Height        := Height;
      Display.Active_Buffer := 1;
      Display.Depth         := Color_Depth;

      --  Wait for screen on.
      delay until Clock + Seconds (1);
      if Debug then
         Put_Line ("Screen should  now be on...");
      end if;

      --  Enable the DMA channel used for framebuffer manipulations
      if Debug then
         Put_Line ("Enabling the DMA channel for fb purpose...");
      end if;

      --  The bitmap API requires DMA0 to accelerate the transfers.
      --  Let's use a pretty large number of control blocks to allow chaining
      --  those operations
      Initialize (Rpi_Board.DMA_0, 100);
   end Initialize;

   -----------
   -- Blank --
   -----------

   procedure Blank (Display : in out Framebuffer_Display;
                    State   : Boolean)
   is
      pragma Unreferenced (Display);
   begin
      Fw_Request_RO (Tag_Blank_Screen, (if State then 1 else 0));
   end Blank;

   --------------------
   -- Set_Alpha_Mode --
   --------------------

   procedure Set_Alpha_Mode (Display : in out Framebuffer_Display;
                             Mode    : Alpha_Mode)
   is
      pragma Unreferenced (Display);
   begin
      Fw_Request_RO (Tag_Set_Alpha_Mode, Alpha_Mode'Enum_Rep (Mode));
   end Set_Alpha_Mode;

   ----------
   -- Flip --
   ----------

   procedure Flip
     (Display : in out Framebuffer_Display)
   is
      Data : UInt32_Array :=
               (1 => 0,
                2 => (if Display.Active_Buffer = 1
                      then UInt32 (Display.Height)
                      else 0));

   begin
      Fw_Request (Tag_Set_Virtual_Offset, Data);
      Fw_Request (Tag_Set_VSync);

      if Data (2) = 0 then
         Display.Active_Buffer := 1;
      else
         Display.Active_Buffer := 2;
      end if;
   end Flip;

   ------------------------
   -- Hidden_Framebuffer --
   ------------------------

   function Hidden_Framebuffer
     (Display : in out Framebuffer_Display)
      return RPi.Bitmap.RPi_Bitmap_Buffer
   is
   begin
      if Display.Active_Buffer = 2 then
         return Display.FB (1);
      else
         return Display.FB (2);
      end if;
   end Hidden_Framebuffer;

end RPi.Framebuffer;
