------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with STM32_SVD.DMA2D; use STM32_SVD.DMA2D;

package body STM32.DMA2D.Polling is

   Transferring : Boolean := False;

   procedure DMA2D_Init_Transfer;
   procedure DMA2D_Wait_Transfer;

   ---------------------------
   -- DMA2D_InitAndTransfer --
   ---------------------------

   procedure DMA2D_Init_Transfer is
   begin
      Transferring := True;
      DMA2D_Periph.IFCR.CTCIF := True;
      DMA2D_Periph.IFCR.CCTCIF := True;
      DMA2D_Periph.CR.START := True;
   end DMA2D_Init_Transfer;

   -------------------------
   -- DMA2D_Wait_Transfer --
   -------------------------

   procedure DMA2D_Wait_Transfer is
   begin
      if not Transferring then
         return;
      end if;

      Transferring := False;

      if DMA2D_Periph.ISR.CEIF then --  Conf error
         raise Constraint_Error with "DMA2D Configuration error";
      elsif DMA2D_Periph.ISR.TEIF then -- Transfer error
         raise Constraint_Error with "DMA2D Transfer error";
      else
         while not DMA2D_Periph.ISR.TCIF loop --  Transfer completed
            if DMA2D_Periph.ISR.TEIF then
               raise Constraint_Error with "DMA2D Transfer error";
            end if;
         end loop;

         DMA2D_Periph.IFCR.CTCIF := True; --  Clear the TCIF flag
      end if;
   end DMA2D_Wait_Transfer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      STM32.DMA2D.DMA2D_Init
        (Init => DMA2D_Init_Transfer'Access,
         Wait => DMA2D_Wait_Transfer'Access);
   end Initialize;

end STM32.DMA2D.Polling;
