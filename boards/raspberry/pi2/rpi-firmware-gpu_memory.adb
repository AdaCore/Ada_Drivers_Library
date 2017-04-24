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

with Interfaces;       use Interfaces;
pragma Warnings (Off);
with Interfaces.Cache; use Interfaces.Cache;
pragma Warnings (On);

with RPi.Mailbox; use RPi.Mailbox;

package body RPi.Firmware.GPU_Memory is

   procedure Memory_Allocate
     (Size      : UInt32;
      Alignment : UInt32;
      Flags     : Memory_Flag;
      Handle    : out Memory_Handle)
   is
      --  Important note: in this procedure and in Lock_Memory, we can't use
      --  the services from RPi.Firmware, as the initialisation of the firmware
      --  API needs to call Memory_Allocate and Lock_Memory.
      --  So we do direct mailbox access in this case.
      Msg : UInt32_Array :=
              (0,
               0,

               ARM_To_VC_Tag_Code (Tag_Allocate_Memory),
               12,
               12,
               Size,
               Alignment,
               UInt32 (Flags),

               0);
      for Msg'Alignment use 16;
      Res : UInt32 with Unreferenced;
      use type System.Storage_Elements.Storage_Offset;

   begin
      Msg (0) := Msg'Size / 8;

      --  Clean and invalidate so that GPU can read it
      Dcache_Flush_By_Range (Msg'Address, Msg'Length * 4);
      Mailbox_Write (Msg'Address, Property_Tags_ARM_To_VC);
      Res := Mailbox_Read (Property_Tags_ARM_To_VC);
      Dcache_Invalidate_By_Range (Msg'Address, Msg'Length * 4);

      if Msg (1) /= 16#8000_0000# then
         Handle := Invalid_Memory_Handle;
      else
         Handle := Memory_Handle (Msg (5));
      end if;
   end Memory_Allocate;

   -----------------
   -- Memory_Lock --
   -----------------

   procedure Memory_Lock
     (Handle : Memory_Handle;
      Addr   : out BUS_Address)
   is
      --  See Memory_Allocate above for the reasons we use the low level
      --  mailbox API here.
      Msg : UInt32_Array :=
              (0,
               0,

               ARM_To_VC_Tag_Code (Tag_Lock_Memory),
               4,
               4,
               UInt32 (Handle),

               0);
      for Msg'Alignment use 16;
      Res : UInt32 with Unreferenced;
      use type System.Storage_Elements.Storage_Offset;

   begin
      Msg (0) := Msg'Size / 8;

      --  Clean and invalidate so that GPU can read it
      Dcache_Flush_By_Range (Msg'Address, Msg'Length * 4);
      Mailbox_Write (Msg'Address, Property_Tags_ARM_To_VC);
      Res := Mailbox_Read (Property_Tags_ARM_To_VC);
      Dcache_Invalidate_By_Range (Msg'Address, Msg'Length * 4);

      if Msg (1) /= 16#8000_0000# then
         Addr := 0;
      else
         Addr := BUS_Address (Msg (5));
      end if;
   end Memory_Lock;

   -------------------
   -- Memory_Unlock --
   -------------------

   procedure Memory_Unlock
     (Handle : Memory_Handle;
      Status : out Boolean)
   is
      Value : UInt32 := UInt32 (Handle);
   begin
      Fw_Request (Tag_Unlock_Memory, Value);
      Status := Value = 0;
   end Memory_Unlock;

   --------------------
   -- Release_Memory --
   --------------------

   procedure Memory_Release
     (Handle : Memory_Handle;
      Status : out Boolean)
   is
      Value : UInt32 := UInt32 (Handle);
   begin
      Fw_Request (Tag_Release_Memory, Value);
      Status := Value = 0;
   end Memory_Release;

end RPi.Firmware.GPU_Memory;
