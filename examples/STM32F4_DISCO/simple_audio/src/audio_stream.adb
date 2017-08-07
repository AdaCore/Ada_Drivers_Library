------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body Audio_Stream is

   ------------------------------
   -- Double_Buffer_Controller --
   ------------------------------

   protected body Double_Buffer_Controller is

      -----------
      -- Start --
      -----------

      procedure Start
        (Destination : Address;
         Source_0    : Address;
         Source_1    : Address;
         Data_Count  : UInt16)
      is
         Config : DMA_Stream_Configuration;
      begin
         Enable_Clock (Controller.all);

         Config.Channel := Audio_TX_DMA_Chan;
         Config.Direction := Memory_To_Peripheral;
         Config.Increment_Peripheral_Address := False;
         Config.Increment_Memory_Address := True;
         Config.Peripheral_Data_Format := HalfWords;
         Config.Memory_Data_Format := HalfWords;
         Config.Operation_Mode := Circular_Mode;
         Config.Priority := Priority_High;
         Config.FIFO_Enabled := False;
         Config.FIFO_Threshold := FIFO_Threshold_Full_Configuration;
         Config.Memory_Burst_Size := Memory_Burst_Single;
         Config.Peripheral_Burst_Size := Peripheral_Burst_Single;
         Configure (Controller.all, Stream, Config);

         Configure_Data_Flow
           (Controller.all,
            Stream,
            Source      => Source_0,
            Destination => Destination,
            Data_Count  => Data_Count);

         for Selected_Interrupt in DMA_Interrupt loop
            Enable_Interrupt (Controller.all, Stream, Selected_Interrupt);
         end loop;

         Configure_Double_Buffered_Mode (This              => Controller.all,
                                         Stream            => Stream,
                                         Buffer_0_Value    => Source_0,
                                         Buffer_1_Value    => Source_1,
                                         First_Buffer_Used => Memory_Buffer_0);

         Buffer_0 := Source_0;
         Buffer_1 := Source_1;

         Enable_Double_Buffered_Mode (Controller.all, Stream);

         Clear_All_Status (Controller.all, Stream);

         Enable (Controller.all, Stream);
      end Start;

      --------------------------------
      -- Wait_For_Transfer_Complete --
      --------------------------------

      entry Wait_For_Transfer_Complete when Interrupt_Triggered is
      begin
         Interrupt_Triggered := False;
      end Wait_For_Transfer_Complete;

      ---------------------
      -- Not_In_Transfer --
      ---------------------

      function Not_In_Transfer return Address is
      begin
         case Current_Memory_Buffer (Controller.all, Stream) is
            when Memory_Buffer_0 =>
               return Buffer_1;
            when Memory_Buffer_1 =>
               return Buffer_0;
         end case;
      end Not_In_Transfer;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
      begin
         for Flag in DMA_Status_Flag loop
            if Status (Controller.all, Stream, Flag) then
               case Flag is
                  when Transfer_Complete_Indicated =>
                     Interrupt_Triggered := True;
                  when others =>
                     null;
               end case;
               Clear_Status (Controller.all, Stream, Flag);
            end if;
         end loop;
      end Interrupt_Handler;

   end Double_Buffer_Controller;

end Audio_Stream;
