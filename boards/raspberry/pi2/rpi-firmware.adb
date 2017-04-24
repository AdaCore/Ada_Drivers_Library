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

with System;
with Interfaces;               use Interfaces;
pragma Warnings (Off);
with Interfaces.Cache;         use Interfaces.Cache;
pragma Warnings (On);
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with RPi.Mailbox;              use RPi.Mailbox;
with RPi.Firmware.GPU_Memory;

------------------
-- RPi.Firmware --
------------------

package body RPi.Firmware is

   Debug              : constant Boolean := False;

   Request_Code       : constant UInt32 := 16#0#;
   Request_Indicator  : constant UInt32 := 0;
--     Response_Indicator : constant UInt32 := 16#8000_0000#;
--     Response_Success   : constant UInt32 := 16#8000_0000#;
   Response_Error     : constant UInt32 := 16#8000_0001#;

   Message_Pool_Size  : constant := 4096;

   Initialized        : Boolean := False;

   type Message_Pool is access UInt8_Array (1 .. Message_Pool_Size);
   Pool               : Message_Pool := null;
   P_Index            : Natural := 0;

   protected Fw_Lock is
      entry Lock;
      procedure Unlock;
   private
      Unlocked : Boolean := True;
   end Fw_Lock;

   subtype B4 is UInt8_Array (1 .. 4);
   function As_UInt8_Array is new Ada.Unchecked_Conversion (UInt32, B4);
   function As_UInt32 is new Ada.Unchecked_Conversion (B4, UInt32);

   -------------
   -- Fw_Lock --
   -------------

   protected body Fw_Lock is

      ----------
      -- Lock --
      ----------

      entry Lock when Unlocked is
      begin
         Unlocked := False;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock
      is
      begin
         Unlocked := True;
      end Unlock;
   end Fw_Lock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      use RPi.Firmware.GPU_Memory;
      Handle    : Memory_Handle;
      BUS_Addr  : BUS_Address;
      Buf_Addr  : System.Address;

      function As_Pool is new Ada.Unchecked_Conversion
        (System.Address, Message_Pool);

   begin
      if Initialized then
         return;
      end if;

      --  We allocate the buffer for the Firmware messages in the GPU memory,
      --  so that we don't have to perform cache operations to communicate with
      --  it.
      Memory_Allocate
        (Message_Pool_Size * 4,
         16,
         Mem_Flag_L1_Non_Allocating or
           Mem_Flag_Hint_Permalock or Mem_Flag_Zero,
         Handle);

      if Handle = Invalid_Memory_Handle then
         Put_Line ("Cannot allocate GPU memory");
      end if;

      Memory_Lock (Handle, BUS_Addr);

      if Debug then
         Put_Line ("Firmware message pool BUS addr at 0x" &
                     Image8 (UInt32 (BUS_Addr)));
      end if;

      Buf_Addr := To_ARM (BUS_Addr);
      Pool     := As_Pool (Buf_Addr);

      if Pool /= null then
         Initialized := True;
      end if;
   end Initialize;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag : ARM_To_VC_Tag)
   is
   begin
      Lock;
      Add_Message (Tag);
      Do_Transaction;
      Unlock;
   end Fw_Request;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt8_Array)
   is
      Offset : Natural;
   begin
      Lock;
      Offset := Add_Message (Tag, Input);
      Do_Transaction;
      Input := Get_Result (Offset, Input'Length);
      Unlock;
   end Fw_Request;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt32_Array)
   is
      Offset : Natural;
   begin
      Lock;
      Offset := Add_Message (Tag, Input);
      Do_Transaction;
      Input := Get_Result (Offset, Input'Length);
      Unlock;
   end Fw_Request;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt32)
   is
      BA : UInt8_Array (1 .. 4) with Address => Input'Address;
   begin
      Fw_Request (Tag, BA);
   end Fw_Request;

   --------------------
   -- Gen_Do_Request --
   --------------------

   procedure Gen_Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out T)
   is
      L : constant Natural := T'Size / 8;
      BA : UInt8_Array (1 .. L) with Address => Input'Address;
   begin
      Fw_Request (Tag, BA);
   end Gen_Fw_Request;

   -------------------
   -- Do_Request_RO --
   -------------------

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt8_Array)
   is
      Offset : Natural with Unreferenced;
   begin
      Lock;
      Offset := Add_Message (Tag, Input);
      Do_Transaction;
      Unlock;
   end Fw_Request_RO;

   -------------------
   -- Do_Request_RO --
   -------------------

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32_Array)
   is
      BA : UInt8_Array (1 .. Input'Length * 4) with Address => Input'Address;
   begin
      Fw_Request_RO (Tag, BA);
   end Fw_Request_RO;

   -------------------
   -- Do_Request_RO --
   -------------------

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32)
   is
      BA : UInt8_Array (1 .. 4) with Address => Input'Address;
   begin
      Fw_Request_RO (Tag, BA);
   end Fw_Request_RO;

   -----------------------
   -- Gen_Do_Request_RO --
   -----------------------

   procedure Gen_Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : T)
   is
      BA : UInt8_Array (1 .. T'Size / 8) with Address => Input'Address;
   begin
      Fw_Request_RO (Tag, BA);
   end Gen_Fw_Request_RO;

   ----------
   -- Lock --
   ----------

   procedure Lock
   is
   begin
      Fw_Lock.Lock;

      if not Initialized then
         Initialize;
      end if;

      Pool (1 .. 4) := (others => 0);
      Pool (5 .. 8) := As_UInt8_Array (Request_Code);

      P_Index := 8;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock
   is
   begin
      Fw_Lock.UnLock;
   end Unlock;

   -----------------
   -- Add_Message --
   -----------------

   procedure Add_Message (Tag   : ARM_To_VC_Tag)
   is
   begin
      Pool (P_Index + 1 .. P_Index + 4) :=
        As_UInt8_Array (ARM_To_VC_Tag_Code (Tag));
      Pool (P_Index + 5 .. P_Index + 8) := As_UInt8_Array (0);
      Pool (P_Index + 9 .. P_Index + 12) := As_UInt8_Array (Request_Indicator);
      P_Index := P_Index + 12;
   end Add_Message;

   -----------------
   -- Add_Message --
   -----------------

   function Add_Message
     (Tag   : ARM_To_VC_Tag;
      Input : UInt8_Array) return Natural
   is
      Ret : Natural;
   begin
      Pool (P_Index + 1 .. P_Index + 4) :=
        As_UInt8_Array (ARM_To_VC_Tag_Code (Tag));
      Pool (P_Index + 5 .. P_Index + 8) := As_UInt8_Array (Input'Length);
      Pool (P_Index + 9 .. P_Index + 12) :=
        As_UInt8_Array (Request_Indicator or Input'Length);
      Ret := P_Index + 13;
      Pool (Ret .. Ret + Input'Length - 1) := Input;
      P_Index := Ret + Input'Length - 1;

      if P_Index mod 4 /= 0 then
         P_Index := P_Index + 4 - (P_Index mod 4);
      end if;

      return Ret;
   end Add_Message;

   -----------------
   -- Add_Message --
   -----------------

   function Add_Message (Tag   : ARM_To_VC_Tag;
                         Input : UInt32_Array) return Natural
   is
      L : constant Natural := Input'Length;
      subtype WA is UInt32_Array (1 .. L);
      subtype BA is UInt8_Array (1 .. L * 4);
      function To_BA is new Ada.Unchecked_Conversion (WA, BA);
   begin
      return Add_Message (Tag, To_BA (Input));
   end Add_Message;

   ---------------------
   -- Gen_Add_Message --
   ---------------------

   function Gen_Add_Message (Tag   : ARM_To_VC_Tag; Input : T) return Natural
   is
      L : constant Natural := T'Size / 8;
      subtype BA is UInt8_Array (1 .. L);
      function To_BA is new Ada.Unchecked_Conversion (T, BA);
   begin
      return Add_Message (Tag, To_BA (Input));
   end Gen_Add_Message;

   function Word_Add_Message is new Gen_Add_Message (UInt32);

   function Add_Message
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32) return Natural
      renames Word_Add_Message;

   --------------------
   -- Do_Transaction --
   --------------------

   procedure Do_Transaction
   is
      Status : Boolean;
   begin
      Status := Do_Transaction;

      if Debug and then not Status then
         Put_Line ("Firmware transaction reported an invalid status");
      end if;
   end Do_Transaction;

   --------------------
   -- Do_Transaction --
   --------------------

   function Do_Transaction return Boolean
   is
      use System.Storage_Elements;
      Res : UInt32 with Unreferenced;
      Ret : UInt32;

   begin
      --  Message size
      Pool (1 .. 4) := As_UInt8_Array (UInt32 (P_Index + 4));
      --  Trailing 0
      Pool (P_Index + 1 .. P_Index + 4) := (others => 0);

      --  Call the mailbox
      Mailbox_Write (Pool.all'Address, Property_Tags_ARM_To_VC);
      Res := Mailbox_Read (Property_Tags_ARM_To_VC);

      Ret := As_UInt32 (Pool (5 .. 8));
      return Ret /= Response_Error;
   end Do_Transaction;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Offset : Natural; Size : Natural) return UInt8_Array
   is
   begin
      return Pool (Offset .. Offset + Size - 1);
   end Get_Result;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Offset : Natural; Size : Natural) return UInt32_Array
   is
      subtype WA is UInt32_Array (1 .. Size);
      subtype BA is UInt8_Array (1 .. Size * 4);
      function To_WA is new Ada.Unchecked_Conversion (BA, WA);
   begin
      return To_WA (Get_Result (Offset, Size * 4));
   end Get_Result;

   --------------------
   -- Gen_Get_Result --
   --------------------

   function Gen_Get_Result (Offset : Natural) return T
   is
      L : constant Natural := T'Size / 8;
      subtype BA is UInt8_Array (1 .. L);
      function To_T is new Ada.Unchecked_Conversion (BA, T);
   begin
      return To_T (Get_Result (Offset, L));
   end Gen_Get_Result;

   ---------------------
   -- Word_Get_Result --
   ---------------------

   function Word_Get_Result is new Gen_Get_Result (UInt32);

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Offset : Natural) return UInt32
     renames Word_Get_Result;

end RPi.Firmware;
