------------------------------------------------------------------------------
--                              Certyflie                                   --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  AK8963 I2C device class package

with HAL.I2C;    use HAL;

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

   type AK8963_Device (I2C_Port         : not null HAL.I2C.I2C_Port_Ref;
                       Address_Selector : AK8963_Address_Selector) is private;

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

   type AK8963_Device (I2C_Port         : not null HAL.I2C.I2C_Port_Ref;
                       Address_Selector : AK8963_Address_Selector)
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
