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

with STM32.Device;    use STM32.Device;

package body STM32.CRC.DMA is

   procedure Configure_DMA
     (Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Width : DMA_Data_Transfer_Widths);
   --  Configures the DMA controller and stream for transfering memory blocks,
   --  of the width specified, to the CRC processor.

   ---------------------------
   -- Transfer_Input_To_CRC --
   ---------------------------

   procedure Transfer_Input_To_CRC
     (This          : in out CRC_32;
      Controller    : access DMA_Controller;
      Stream        : DMA_Stream_Selector;
      Input_Address : System.Address;
      Input_Length  : UInt16;
      Data_Width    : DMA_Data_Transfer_Widths)
   is
   begin
      Configure_DMA (Controller, Stream, Data_Width);
      --  We configure the unit each time to ensure the data width is right.

      Clear_All_Status (Controller.all, Stream);
      --  Ensure previous calls or other use hasn't set any status flags.

      Start_Transfer_with_Interrupts
        (Controller.all,
         Stream,
         Source             => Input_Address,
         Destination        => This.DR'Address,
         Data_Count         => Input_Length,
         Enabled_Interrupts => (Transfer_Complete_Interrupt => True,
                                others                      => False));
   end Transfer_Input_To_CRC;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This       : in out CRC_32;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Input      : Block_32) is
   begin
      Transfer_Input_To_CRC
        (This,
         Controller,
         Stream,
         Input'Address,
         Input'Length,
         Data_Width => Words);
   end Update_CRC;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This       : in out CRC_32;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Input      : Block_16) is
   begin
      Transfer_Input_To_CRC
        (This,
         Controller,
         Stream,
         Input'Address,
         Input'Length,
         Data_Width => HalfWords);
   end Update_CRC;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (This       : in out CRC_32;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Input      : Block_8) is
   begin
      Transfer_Input_To_CRC
        (This,
         Controller,
         Stream,
         Input'Address,
         Input'Length,
         Data_Width => Bytes);
   end Update_CRC;

   -------------------
   -- Configure_DMA --
   -------------------

   procedure Configure_DMA
     (Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Width : DMA_Data_Transfer_Widths)
   is
      Config : DMA_Stream_Configuration;
   begin
      --  See app note AN4187 Table 3 for this configuration (other than the
      --  channel number). It works, although it looks counterintuitive.

      Config.Channel                      := Channel_0;  -- arbitrary
      Config.Direction                    := Memory_To_Memory;
      Config.Memory_Data_Format           := Data_Width;
      Config.Peripheral_Data_Format       := Words;
      Config.Increment_Peripheral_Address := True;
      Config.Increment_Memory_Address     := False;
      Config.Operation_Mode               := Normal_Mode;
      Config.Priority                     := Priority_Very_High;
      Config.FIFO_Enabled                 := False;
      Config.Memory_Burst_Size            := Memory_Burst_Single;
      Config.Peripheral_Burst_Size        := Peripheral_Burst_Single;

      Configure (Controller.all, Stream, Config);
   end Configure_DMA;

end STM32.CRC.DMA;
