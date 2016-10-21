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

with HAL;           use HAL;
with HAL.I2C;       use HAL.I2C;
with nRF51.GPIO;    use nRF51.GPIO;
with NRF51_SVD.TWI;

package nRF51.TWI is

   type TWI_Speed is (TWI_100kbps, TWI_250kbps, TWI_400kbps);

   type TWI_Master (Periph : not null access NRF51_SVD.TWI.TWI_Peripheral) is
     new HAL.I2C.I2C_Port with private;

   procedure Enable (This : in out TWI_Master);

   procedure Disable (This : in out TWI_Master);

   function Enabled (This : TWI_Master) return Boolean;

   procedure Configure (This     : in out TWI_Master;
                        SCL, SDA : GPIO_Pin_Index;
                        Speed    : TWI_Speed);

   procedure Disconnect (This : in out TWI_Master);
   --  Disconect the peripheral from the GPIO points

   overriding
   procedure Master_Transmit
     (This    : in out TWI_Master;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
     with Pre => Enabled (This);

   overriding
   procedure Master_Receive
     (This    : in out TWI_Master;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
     with Pre => Enabled (This);

   overriding
   procedure Mem_Write
     (This          : in out TWI_Master;
      Addr          : I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
     with Pre => Enabled (This);

   overriding
   procedure Mem_Read
     (This          : in out TWI_Master;
      Addr          : I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
     with Pre => Enabled (This);

private

   type TWI_Master (Periph : not null access NRF51_SVD.TWI.TWI_Peripheral) is
     new HAL.I2C.I2C_Port with record
      Do_Stop_Sequence : Boolean := True;
   end record;

end nRF51.TWI;
