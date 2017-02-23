------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with Ada.Unchecked_Conversion;

package body nRF51.Events is

   function To_UInt32 is new Ada.Unchecked_Conversion (System.Address, UInt32);
   function To_Address is new Ada.Unchecked_Conversion (UInt32, System.Address);

   ---------------
   -- Triggered --
   ---------------

   function Triggered (Evt : Event_Type) return Boolean is
      Reg : UInt32;
      for Reg'Address use System.Address (Evt);
   begin
      return Reg /= 0;
   end Triggered;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (Evt : Event_Type) is

      Reg_Addr : constant UInt32 := To_UInt32 (System.Address (Evt));
      Device_Base : constant UInt32 := Reg_Addr and 16#FFFF_F000#;

      Event_Index : constant UInt7 := UInt7 (Reg_Addr and 16#0000_007F#) / 4;
      --  The bit corresponding to an event is determined by the offset of the
      --  event. (nRF51 Series Reference Manual, section 9.1.6)

      Set_Register_Addr : constant UInt32 := Device_Base + 16#0000_0304#;
      --  For each device the "Interrupt enable set register" is always located
      --  at the same offset: 0x304. (nRF51 Series Reference Manual,
      --  section 9.1.6)

      Set_Register : UInt32 with Address => To_Address (Set_Register_Addr);
   begin
      Set_Register := 2**Natural (Event_Index);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (Evt : Event_Type) is

      Reg_Addr : constant UInt32 := To_UInt32 (System.Address (Evt));
      Device_Base : constant UInt32 := Reg_Addr and 16#FFFF_F000#;

      Event_Index : constant UInt7 := UInt7 (Reg_Addr and 16#0000_007F#) / 4;
      --  The bit corresponding to an event is determined by the offset of the
      --  event. (nRF51 Series Reference Manual, section 9.1.6)

      Clear_Register_Addr : constant UInt32 := Device_Base + 16#0000_0308#;
      --  For each device the "Interrupt enable clear register" is always
      --  located at the same offset: 0x308. (nRF51 Series Reference Manual,
      --  section 9.1.6)

      Clear_Register : UInt32 with Address => To_Address (Clear_Register_Addr);
   begin
      Clear_Register := 2**Natural (Event_Index);
   end Disable_Interrupt;

   -----------
   -- Clear --
   -----------

   procedure Clear (Evt : Event_Type) is
      Reg : UInt32 with Address => System.Address (Evt);
   begin
      Reg := 0;
   end Clear;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Evt : Event_Type) return System.Address is
   begin
      return System.Address (Evt);
   end Get_Address;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Evt : Event_Type) return UInt32 is
   begin
      return To_UInt32 (System.Address (Evt));
   end Get_Address;


end nRF51.Events;
