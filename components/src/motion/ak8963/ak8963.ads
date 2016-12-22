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

--  AK8963 I2C device class package

with HAL.I2C;    use HAL;
with HAL.Time;

package AK8963 is

   type AK8963_Address_Selector is (Add_00, Add_01, Add_10, Add_11);

   type AK8963_Sampling_Mode is (Mode_14bit, Mode_16bit);

   subtype Gauss is Float;

   type AK8963_Operation_Mode is
     (Power_Down,
      Single_Measurement,
      Continuous_1,
      External_Trigger,
      Continuous_2,
      Self_Test,
      Fuse_ROM_Access);

   type AK8963_Device (I2C_Port         : not null HAL.I2C.Any_I2C_Port;
                       Address_Selector : AK8963_Address_Selector;
                        Time            : not null HAL.Time.Any_Delays) is private;

   procedure Initialize (Device : in out AK8963_Device);

   function Test_Connection (Device : AK8963_Device) return Boolean;

   function Self_Test (Device : in out AK8963_Device) return Boolean;

   procedure Set_Mode
     (Device         : in out AK8963_Device;
      Operation_Mode : AK8963_Operation_Mode;
      Sampling_Mode  : AK8963_Sampling_Mode);

   procedure Set_Mode
     (Device         : in out AK8963_Device;
      Operation_Mode : AK8963_Operation_Mode);

   function Get_Data_Ready (Device : AK8963_Device) return Boolean;

   procedure Get_Heading
     (Device     : AK8963_Device;
      Mx, My, Mz : out Gauss);

   function Get_Overflow_Status (Device : AK8963_Device) return Boolean;

private

   type AK8963_Device (I2C_Port         : not null HAL.I2C.Any_I2C_Port;
                       Address_Selector : AK8963_Address_Selector;
                        Time            : not null HAL.Time.Any_Delays)
   is record
      Address : UInt10;
      Is_Init : Boolean := False;
   end record;

   for AK8963_Sampling_Mode use
     (Mode_14bit => 16#00#,
      Mode_16bit => 16#10#);

   for AK8963_Operation_Mode use
     (Power_Down         => 16#00#,
      Single_Measurement => 16#01#,
      Continuous_1       => 16#02#,
      External_Trigger   => 16#04#,
      Continuous_2       => 16#06#,
      Self_Test          => 16#08#,
      Fuse_ROM_Access    => 16#0F#);

end AK8963;
