------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body HT16K33 is

   HT16K33_Base_Addr : constant I2C_Address := 2#1110_0000#;
   Reg_Display_Setup : constant UInt8 := 16#80#;
   Reg_Dimming       : constant UInt8 := 16#E0#;

   function I2C_Addr (This : HT16K33_Device) return I2C_Address;

   procedure I2C_Read (This   : in out HT16K33_Device;
                       Reg    : UInt8;
                       Data   : out I2C_Data;
                       Status : out Boolean);

   procedure I2C_Write (This   : in out HT16K33_Device;
                        Reg    : UInt8;
                        Data   : I2C_Data;
                        Status : out Boolean);

   procedure I2C_Command (This : in out HT16K33_Device;
                          Cmd  : UInt8;
                          Status : out Boolean);


   procedure Update_Setup_Reg (This : in out HT16K33_Device);

   --------------
   -- I2C_Addr --
   --------------

   function I2C_Addr (This : HT16K33_Device) return I2C_Address is
     ((I2C_Address (This.Addr) * 2) or HT16K33_Base_Addr);

   --------------
   -- I2C_Read --
   --------------

   procedure I2C_Read (This   : in out HT16K33_Device;
                      Reg    : UInt8;
                      Data   : out I2C_Data;
                      Status : out Boolean)
   is
      Tmp_Status : I2C_Status;
   begin
      This.Port.Mem_Read
        (I2C_Addr (This),
         UInt16 (Reg),
         Memory_Size_8b,
         Data,
         Tmp_Status,
         1000);
      Status := Tmp_Status = Ok;
   end I2C_Read;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write (This   : in out HT16K33_Device;
                        Reg    : UInt8;
                        Data   : I2C_Data;
                        Status : out Boolean)
   is
      Tmp_Status : I2C_Status;
   begin
      This.Port.Mem_Write
        (I2C_Addr (This),
         UInt16 (Reg),
         Memory_Size_8b,
         Data,
         Tmp_Status,
         1000);
      Status := Tmp_Status = Ok;
   end I2C_Write;

   -----------------
   -- I2C_Command --
   -----------------

   procedure I2C_Command (This : in out HT16K33_Device;
                          Cmd  : UInt8;
                          Status : out Boolean)
   is
      Data : constant I2C_Data (1 .. 1) := (others => Cmd);
      Tmp_Status : I2C_Status;
   begin
      This.Port.Master_Transmit
        (I2C_Addr (This),
         Data,
         Tmp_Status,
         1000);
      Status := Tmp_Status = Ok;
   end I2C_Command;

   ----------------------
   -- Update_Setup_Reg --
   ----------------------

   procedure Update_Setup_Reg (This : in out HT16K33_Device) is
      Status  : Boolean;
      Enabled : constant UInt8 := (if This.Enabled then 1 else 0);
      Blink   : constant UInt8 :=
        Shift_Left (HT16K33_Blink'Enum_Rep (This.Blink), 1);
   begin
      I2C_Command (This, Reg_Display_Setup or Blink or Enabled, Status);

      if not Status then
         raise Program_Error;
      end if;
   end Update_Setup_Reg;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out HT16K33_Device) is
   begin
      This.Enabled := True;
      Update_Setup_Reg (This);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out HT16K33_Device) is
   begin
      This.Enabled := False;
      Update_Setup_Reg (This);
   end Disable;

   --------------------
   -- Set_Brightness --
   --------------------

   procedure Set_Brightness (This       : in out HT16K33_Device;
                             Brightness : HT16K33_Brightness)
   is
      Status : Boolean;
   begin

      I2C_Command (This, Reg_Dimming or (UInt8 (Brightness) - 1), Status);

      if not Status then
         raise Program_Error;
      end if;
   end Set_Brightness;


   ---------------
   -- Set_Blink --
   ---------------

   procedure Set_Blink (This  : in out HT16K33_Device;
                        Blink : HT16K33_Blink)
   is
   begin
      This.Blink := Blink;
      Update_Setup_Reg (This);
   end Set_Blink;

   -------------
   -- Set_LED --
   -------------

   procedure Set_LED (This   : in out HT16K33_Device;
                      Row    : LED_Row_Addr;
                      Column : LED_Column_Addr;
                      Enable : Boolean := True)
   is
   begin
      if Enable then
         This.LEDs (Integer (Row)) :=
           This.LEDs (Integer (Row)) or Shift_Left (1, Integer (Column));
      else
         This.LEDs (Integer (Row)) :=
           This.LEDs (Integer (Row)) and (not Shift_Left (1, Integer (Column)));
      end if;

   end Set_LED;

   -------------
   -- Set_Row --
   -------------

   procedure Set_Row (This : in out HT16K33_Device;
                      Addr : LED_Row_Addr;
                      Row  : UInt8)
   is
   begin
      This.LEDs (Integer (Addr)) := Row;
   end Set_Row;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (This   : in out HT16K33_Device;
                     Row    : Key_Row_Addr;
                     Column : Key_Column_Addr) return Boolean
   is
   begin
      return
        (This.Keys (Integer (Row)) and Shift_Left (1, Integer (Column))) /= 0;
   end Get_Key;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs (This : in out HT16K33_Device) is
      Status : Boolean;
   begin
      I2C_Write (This, 16#00#, This.LEDs, Status);

      if not Status then
         raise Program_Error;
      end if;
   end Update_LEDs;

   -----------------
   -- Update_Keys --
   -----------------

   procedure Update_Keys (This : in out HT16K33_Device) is
      Status : Boolean;
   begin
      I2C_Read (This   => This,
                Reg    => 16#40#,
                Data   => This.Keys,
                Status => Status);

      if not Status then
         raise Program_Error;
      end if;
   end Update_Keys;

end HT16K33;
