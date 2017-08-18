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

with NRF51_SVD.TWI; use NRF51_SVD.TWI;

package body nRF51.TWI is

   procedure Stop_Sequence (This : in out TWI_Master'Class);

   -------------------
   -- Stop_Sequence --
   -------------------

   procedure Stop_Sequence (This : in out TWI_Master'Class) is
   begin
      --  Stop sequence

      This.Periph.EVENTS_STOPPED := 0;

      This.Periph.TASKS_STOP := 1;

      while This.Periph.EVENTS_STOPPED = 0 loop
         null;
      end loop;
   end Stop_Sequence;

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
   is
      pragma Unreferenced (Timeout);
      Index : Integer := Data'First + 1;
      Evt_Err : UInt32;
      Err_Src : ERRORSRC_Register with Unreferenced;
   begin
      if Data'Length = 0 then
         Status := Ok;
         return;
      end if;

      --  Clear errors
      This.Periph.ERRORSRC.OVERRUN := Errorsrc_Overrun_Field_Reset;
      This.Periph.ERRORSRC.ANACK   := Errorsrc_Anack_Field_Reset;
      This.Periph.ERRORSRC.DNACK   := Errorsrc_Dnack_Field_Reset;

      --  Set Address
      This.Periph.ADDRESS.ADDRESS := UInt7 (Addr / 2);

      --  Prepare first byte
      This.Periph.TXD.TXD := Data (Data'First);

      --  Start TX sequence
      This.Periph.TASKS_STARTTX := 1;

      loop

         loop

            Evt_Err := This.Periph.EVENTS_ERROR;
            if Evt_Err /= 0 then
               Err_Src := This.Periph.ERRORSRC;
               Status := Err_Error;
               --  Clear the error
               This.Periph.EVENTS_ERROR := 0;

               --  Stop sequence
               This.Periph.TASKS_STOP := 1;

               return;
            end if;

            exit when This.Periph.EVENTS_TXDSENT /= 0;
         end loop;

         --  Clear the event
         This.Periph.EVENTS_TXDSENT := 0;

         exit when Index > Data'Last;

         This.Periph.TXD.TXD := Data (Index);
         Index := Index + 1;
      end loop;

      if This.Do_Stop_Sequence then
         Stop_Sequence (This);
      end if;

      Status := Ok;
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   overriding procedure Master_Receive
     (This    : in out TWI_Master;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
   begin
      if Data'Length = 0 then
         Status := Ok;
         return;
      end if;

      --  Clear errors
      This.Periph.ERRORSRC.OVERRUN := Clear;
      This.Periph.ERRORSRC.ANACK   := Clear;
      This.Periph.ERRORSRC.DNACK   := Clear;

      --  Set Address
      This.Periph.ADDRESS.ADDRESS := UInt7 (Addr);

      --  Configure SHORTS to automatically suspend TWI port when receiving a
      --  byte.
      This.Periph.SHORTS.BB_SUSPEND := Enabled;
      This.Periph.SHORTS.BB_STOP    := Disabled;

      --  Start RX sequence
      This.Periph.TASKS_STARTRX := 1;

      for Index in Data'Range loop

         loop

            if This.Periph.EVENTS_ERROR /= 0 then
               Status := Err_Error;
               --  Clear the error
               This.Periph.EVENTS_ERROR := 0;

               Stop_Sequence (This);

               return;
            end if;

            exit when This.Periph.EVENTS_RXDREADY /= 0;
         end loop;

         if Index = Data'Last and then This.Do_Stop_Sequence then

            --  Configure SHORTS to automatically stop the TWI port and produce
            --  a STOP event on the bus when receiving a byte.
            This.Periph.SHORTS.BB_SUSPEND := Disabled;
            This.Periph.SHORTS.BB_STOP    := Enabled;
         end if;

         --  Clear the event
         This.Periph.EVENTS_RXDREADY := 0;

         Data (Index) := This.Periph.RXD.RXD;
      end loop;

      Status := Ok;
   end Master_Receive;

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
         return;
      end if;

      This.Master_Receive (Addr    => Addr,
                           Data    => Data,
                           Status  => Status,
                           Timeout => Timeout);
   end Mem_Read;

end nRF51.TWI;
