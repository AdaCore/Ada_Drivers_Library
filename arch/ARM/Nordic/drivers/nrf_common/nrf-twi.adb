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

with NRF_SVD.TWI; use NRF_SVD.TWI;

package body nRF.TWI is

   procedure Stop_Sequence (This : in out TWI_Master'Class);

   -------------------
   -- Stop_Sequence --
   -------------------

   procedure Stop_Sequence (This : in out TWI_Master'Class) is separate;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out TWI_Master) is
   begin
      This.Periph.ENABLE.ENABLE := Enabled;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out TWI_Master) is
   begin
      This.Periph.ENABLE.ENABLE := Disabled;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : TWI_Master) return Boolean is
   begin
      return This.Periph.ENABLE.ENABLE = Enabled;
   end Enabled;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This     : in out TWI_Master;
      SCL, SDA : GPIO_Pin_Index;
      Speed    : TWI_Speed)
   is
   begin
      This.Periph.PSELSCL := UInt32 (SCL);
      This.Periph.PSELSDA := UInt32 (SDA);
      This.Periph.FREQUENCY := (case Speed is
                                   when TWI_100kbps => 16#0198_0000#,
                                   when TWI_250kbps => 16#0400_0000#,
                                   when TWI_400kbps => 16#0668_0000#);
   end Configure;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (This : in out TWI_Master) is
   begin
      This.Periph.PSELSCL := 16#FFFF_FFFF#;
      This.Periph.PSELSDA := 16#FFFF_FFFF#;
   end Disconnect;

   ---------------------
   -- Master_Transmit --
   ---------------------

   overriding procedure Master_Transmit
     (This    : in out TWI_Master;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is separate;

   --------------------
   -- Master_Receive --
   --------------------

   overriding procedure Master_Receive
     (This    : in out TWI_Master;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is separate;

   ---------------
   -- Mem_Write --
   ---------------

   overriding procedure Mem_Write
     (This          : in out TWI_Master;
      Addr          : I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin

      This.Do_Stop_Sequence := False;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            This.Master_Transmit (Addr    => Addr,
                                  Data    => (0 => UInt8 (Mem_Addr)),
                                  Status  => Status,
                                  Timeout => Timeout);
         when Memory_Size_16b =>
            This.Master_Transmit (Addr    => Addr,
                                  Data    => (UInt8 (Shift_Right (Mem_Addr, 8)),
                                              UInt8 (Mem_Addr and 16#FF#)),
                                  Status  => Status,
                                  Timeout => Timeout);
      end case;


      This.Do_Stop_Sequence := True;

      if Status /= Ok then
         This.Stop_Sequence;
         return;
      end if;

      This.Master_Transmit (Addr    => Addr,
                            Data    => Data,
                            Status  => Status,
                            Timeout => Timeout);
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   overriding procedure Mem_Read
     (This          : in out TWI_Master;
      Addr          : I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      This.Do_Stop_Sequence := False;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            This.Master_Transmit (Addr    => Addr,
                                  Data    => (0 => UInt8 (Mem_Addr)),
                                  Status  => Status,
                                  Timeout => Timeout);
         when Memory_Size_16b =>
            This.Master_Transmit (Addr    => Addr,
                                  Data    => (UInt8 (Shift_Right (Mem_Addr, 8)),
                                              UInt8 (Mem_Addr and 16#FF#)),
                                  Status  => Status,
                                  Timeout => Timeout);
      end case;

      This.Do_Stop_Sequence := True;

      if Status /= Ok then
         This.Stop_Sequence;
         return;
      end if;

      This.Master_Receive (Addr    => Addr,
                           Data    => Data,
                           Status  => Status,
                           Timeout => Timeout);
   end Mem_Read;

end nRF.TWI;
