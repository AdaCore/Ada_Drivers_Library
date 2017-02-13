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

with Ada.Unchecked_Conversion;
with Interfaces;    use Interfaces;

with HAL.I2C;       use HAL.I2C;

package body AK8963 is

   AK9863_ADDRESS_00 : constant := 16#18#;
   AK9863_ADDRESS_01 : constant := 16#1A#;
   AK9863_ADDRESS_10 : constant := 16#1C#;
   AK9863_ADDRESS_11 : constant := 16#1E#;

   AK8963_RA_WIA     : constant := 16#00#;
--     AK8963_RA_INFO    : constant := 16#01#;
   AK8963_RA_ST1     : constant := 16#02#;
   AK8963_RA_HXL     : constant := 16#03#;
--     AK8963_RA_HXH     : constant := 16#04#;
--     AK8963_RA_HYL     : constant := 16#05#;
--     AK8963_RA_HYH     : constant := 16#06#;
--     AK8963_RA_HZL     : constant := 16#07#;
--     AK8963_RA_HZH     : constant := 16#08#;
   AK8963_RA_ST2     : constant := 16#09#;
   AK8963_RA_CNTL    : constant := 16#0A#;
--     AK8963_RA_RSV     : constant := 16#0B#;
   AK8963_RA_ASTC    : constant := 16#0C#;
--     AK8963_RA_I2CDIS  : constant := 16#0F#;
   AK8963_RA_ASAX    : constant := 16#10#;
--     AK8963_RA_ASAY    : constant := 16#11#;
--     AK8963_RA_ASAZ    : constant := 16#12#;

   --  In 16-bit output, decimal value 32760 is 4912 micro Tesla
   --  so 49.12 Gauss
   MAG_TO_GAUSS : constant := 49.12 / 32_760.0;

   AK8963_ST_X_MIN   : constant := -200.0 * MAG_TO_GAUSS;
   AK8963_ST_X_MAX   : constant := 200 * MAG_TO_GAUSS;
   AK8963_ST_Y_MIN   : constant := -200 * MAG_TO_GAUSS;
   AK8963_ST_Y_MAX   : constant := 200 * MAG_TO_GAUSS;
   AK8963_ST_Z_MIN   : constant := -3200 * MAG_TO_GAUSS;
   AK8963_ST_Z_MAX   : constant := -800 * MAG_TO_GAUSS;

   function I2C_Read
     (Device : AK8963_Device;
      Reg    : UInt16) return UInt8;

   function I2C_Read
     (Device : AK8963_Device;
      Reg    : UInt16;
      Bit    : Natural) return Boolean;

   procedure I2C_Write
     (Device : AK8963_Device;
      Reg    : UInt16;
      Data   : UInt8);

   procedure I2C_Write
     (Device : AK8963_Device;
      Reg    : UInt16;
      Bit    : Natural;
      State  : Boolean);

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read
     (Device : AK8963_Device;
      Reg    : UInt16) return UInt8
   is
      Data   : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      Device.I2C_Port.Mem_Read
        (Device.Address,
         Reg,
         Memory_Size_8b,
         Data,
         Status);
      return Data (1);
   end I2C_Read;

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read
     (Device : AK8963_Device;
      Reg    : UInt16;
      Bit    : Natural) return Boolean
   is
      Mask : constant UInt8 := 2 ** Bit;
      Data : constant UInt8 := I2C_Read (Device, Reg);
   begin
      return (Data and Mask) /= 0;
   end I2C_Read;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (Device : AK8963_Device;
      Reg    : UInt16;
      Data   : UInt8)
   is
      Status : I2C_Status with Unreferenced;
   begin
      Device.I2C_Port.Mem_Write
        (Device.Address,
         Reg,
         Memory_Size_8b,
         (1 => Data),
         Status);
   end I2C_Write;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (Device : AK8963_Device;
      Reg    : UInt16;
      Bit    : Natural;
      State  : Boolean)
   is
      Val  : UInt8 := I2C_Read (Device, Reg);
      Mask : constant UInt8 := 2 ** Bit;
   begin
      if State then
         Val := Val or Mask;
      else
         Val := Val and not Mask;
      end if;

      I2C_Write (Device, Reg, Val);
   end I2C_Write;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Device : in out AK8963_Device)
   is
   begin
      if Device.Is_Init then
         return;
      end if;

      Device.Address :=
        (case Device.Address_Selector is
            when Add_00 => AK9863_ADDRESS_00,
            when Add_01 => AK9863_ADDRESS_01,
            when Add_10 => AK9863_ADDRESS_10,
            when Add_11 => AK9863_ADDRESS_11);
      Device.Is_Init := True;
   end Initialize;

   ---------------------
   -- Test_Connection --
   ---------------------

   function Test_Connection (Device : AK8963_Device) return Boolean
   is
      Data   : HAL.I2C.I2C_Data (1 .. 1) := (others => 0);
      Status : HAL.I2C.I2C_Status;
   begin
      Device.I2C_Port.Mem_Read
        (Device.Address,
         AK8963_RA_WIA,
         HAL.I2C.Memory_Size_8b,
         Data,
         Status);

      return Status = Ok and then Data (1) = 16#48#;
   end Test_Connection;

   ---------------
   -- Self_Test --
   ---------------

   function Self_Test (Device : in out AK8963_Device) return Boolean
   is
      Mx, My, Mz : Gauss;
      Conf_Save  : UInt8;
      Retry      : Natural := 20;
      Ret        : Boolean;
      Dead       : Boolean with Unreferenced;

   begin
      if not Test_Connection (Device) then
         return False;
      end if;

      Ret := True;

      --  Save the current configuration
      Conf_Save := I2C_Read (Device, AK8963_RA_CNTL);

      --  Power_Down
      Set_Mode (Device, Power_Down);
      I2C_Write (Device, AK8963_RA_ASTC, 6, True); --  Set SELF bit to True
      Dead := Get_Overflow_Status (Device); --  Clears ST1 by reading ST2
      Set_Mode (Device, Self_Test, Mode_16bit);

      while Retry > 0 loop
         exit when Get_Data_Ready (Device);
         Retry := Retry - 1;
         Device.Time.Delay_Milliseconds (10);
      end loop;

      if Retry = 0 then
         Ret := False;
      end if;

      Get_Heading (Device, Mx, My, Mz);
      Dead := Get_Overflow_Status (Device); --  Clears ST1 by reading ST2
      I2C_Write (Device, AK8963_RA_ASTC, 6, False); --  Set SELF bit to False
      Set_Mode (Device, Power_Down);

      if Mx < AK8963_ST_X_MIN
        or else Mx > AK8963_ST_X_MAX
        or else My < AK8963_ST_Y_MIN
        or else My > AK8963_ST_Y_MAX
        or else Mz < AK8963_ST_Z_MIN
        or else Mz > AK8963_ST_Z_MAX
      then
         Ret := False;
      end if;

      --  Set back the saved config
      I2C_Write (Device, AK8963_RA_CNTL, Conf_Save);

      return Ret;
   end Self_Test;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Device         : in out AK8963_Device;
      Operation_Mode : AK8963_Operation_Mode;
      Sampling_Mode  : AK8963_Sampling_Mode)
   is
      Mode   : constant UInt8 :=
                 AK8963_Operation_Mode'Enum_Rep (Operation_Mode)
                   or AK8963_Sampling_Mode'Enum_Rep (Sampling_Mode);
   begin
      I2C_Write (Device, AK8963_RA_CNTL, Mode);
   end Set_Mode;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Device         : in out AK8963_Device;
      Operation_Mode : AK8963_Operation_Mode)
   is
      Mode   : constant UInt8 :=
                 AK8963_Operation_Mode'Enum_Rep (Operation_Mode);
   begin
      I2C_Write (Device, AK8963_RA_CNTL, Mode);
   end Set_Mode;

   --------------------
   -- Get_Data_Ready --
   --------------------

   function Get_Data_Ready (Device : AK8963_Device) return Boolean
   is
      AK8963_ST1_DRDY_BIT : constant := 0;
   begin
      return I2C_Read (Device, AK8963_RA_ST1, AK8963_ST1_DRDY_BIT);
   end Get_Data_Ready;

   -----------------
   -- Get_Heading --
   -----------------

   procedure Get_Heading
     (Device     : AK8963_Device;
      Mx, My, Mz : out Float)
   is
      Buffer : I2C_Data (1 .. 6);
      Status : I2C_Status;
      function To_Signed is new
        Ada.Unchecked_Conversion (UInt16, Integer_16);

   begin
      Mem_Read
        (Device.I2C_Port.all,
         Addr          => Device.Address,
         Mem_Addr      => AK8963_RA_HXL,
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Buffer,
         Status        => Status);

      if Status /= Ok then
         Mx := 0.0;
         My := 0.0;
         Mz := 0.0;
      else
         Mx :=
           Float (To_Signed (Shift_Left (UInt16 (Buffer (2)), 8)
                  or UInt16 (Buffer (1)))) * MAG_TO_GAUSS;
         My :=
           Float (To_Signed (Shift_Left (UInt16 (Buffer (4)), 8)
                  or UInt16 (Buffer (3)))) * MAG_TO_GAUSS;
         Mz :=
           Float (To_Signed (Shift_Left (UInt16 (Buffer (6)), 8)
                  or UInt16 (Buffer (5)))) * MAG_TO_GAUSS;

         --  Read the sensitivity adjustment data
         Mem_Read
           (Device.I2C_Port.all,
            Addr          => Device.Address,
            Mem_Addr      => AK8963_RA_ASAX,
            Mem_Addr_Size => Memory_Size_8b,
            Data          => Buffer (1 .. 3),
            Status        => Status);

         if Status = Ok then
            Mx := Mx * ((Float (Buffer (1)) - 128.0) / 256.0 + 1.0);
            My := My * ((Float (Buffer (2)) - 128.0) / 256.0 + 1.0);
            Mz := Mz * ((Float (Buffer (3)) - 128.0) / 256.0 + 1.0);
         end if;
      end if;
   end Get_Heading;

   -------------------------
   -- Get_Overflow_Status --
   -------------------------

   function Get_Overflow_Status (Device : AK8963_Device) return Boolean
   is
      AK8963_ST2_HOFL_BIT : constant := 3;
   begin
      return I2C_Read (Device, AK8963_RA_ST2, AK8963_ST2_HOFL_BIT);
   end Get_Overflow_Status;

end AK8963;
