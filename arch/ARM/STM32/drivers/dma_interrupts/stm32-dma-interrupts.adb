------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016-2017, AdaCore                      --
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

package body STM32.DMA.Interrupts is

   protected body DMA_Interrupt_Controller is

      --------------------
      -- Start_Transfer --
      --------------------

      procedure Start_Transfer (Source      : Address;
                                Destination : Address;
                                Data_Count  : UInt16)
      is
      begin
         No_Transfer_In_Progess := False;
         Had_Buffer_Error := False;

         Clear_All_Status (Controller.all, Stream);

         Start_Transfer_with_Interrupts (This               => Controller.all,
                                         Stream             => Stream,
                                         Source             => Source,
                                         Destination        => Destination,
                                         Data_Count         => Data_Count);
      end Start_Transfer;

      --------------------
      -- Abort_Transfer --
      --------------------

      procedure Abort_Transfer (Result : out DMA_Error_Code) is
      begin
         Abort_Transfer (This   => Controller.all,
                         Stream => Stream,
                         Result => Result);
         No_Transfer_In_Progess := Result = DMA_No_Error;
      end Abort_Transfer;

      --------------------------
      -- Clear_Transfer_State --
      --------------------------

      procedure Clear_Transfer_State
      is
      begin
         No_Transfer_In_Progess := True;
         Last_Status := DMA_Transfer_Error;
      end Clear_Transfer_State;

      ------------------
      -- Buffer_Error --
      ------------------

      function Buffer_Error return Boolean is
      begin
         return Had_Buffer_Error;
      end Buffer_Error;

      -------------------------
      -- Wait_For_Completion --
      -------------------------

      entry Wait_For_Completion (Status : out DMA_Error_Code)
        when No_Transfer_In_Progess
      is
      begin
         Status := Last_Status;
      end Wait_For_Completion;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
      begin
         for Flag in DMA_Status_Flag loop
            if Status (Controller.all, Stream, Flag) then
               case Flag is
                  when FIFO_Error_Indicated =>
                     Last_Status := DMA_FIFO_Error;
                     Had_Buffer_Error := True;
                     if not Enabled (Controller.all, Stream) then
                        --  If the stream was disabled by hardware, the transfer
                        --  is stopped. Otherwise we can ignore the even.
                        No_Transfer_In_Progess := True;
                     end if;
                  when Direct_Mode_Error_Indicated =>
                     Last_Status := DMA_Direct_Mode_Error;
                     if not Enabled (Controller.all, Stream) then
                        --  If the stream was disabled by hardware, the transfer
                        --  is stopped. Otherwise we can ignore the even.
                        No_Transfer_In_Progess := True;
                     end if;
                  when Transfer_Error_Indicated =>
                     Last_Status := DMA_Transfer_Error;
                     No_Transfer_In_Progess := True;
                  when Half_Transfer_Complete_Indicated =>
                     null;
                  when Transfer_Complete_Indicated =>
                     Last_Status := DMA_No_Error;
                     No_Transfer_In_Progess := True;
               end case;
               Clear_Status (Controller.all, Stream, Flag);
            end if;
         end loop;
      end Interrupt_Handler;
   end DMA_Interrupt_Controller;

end STM32.DMA.Interrupts;
