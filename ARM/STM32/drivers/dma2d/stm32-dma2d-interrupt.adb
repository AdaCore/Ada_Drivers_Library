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

with Ada.Interrupts.Names;

with STM32_SVD.DMA2D;      use STM32_SVD.DMA2D;

package body STM32.DMA2D.Interrupt is

   procedure DMA2D_Start;
   procedure DMA2D_Wait;

   protected Sync is

      entry Wait;

      procedure Start_Transfer;

      procedure Interrupt;
      pragma Attach_Handler (Interrupt, Ada.Interrupts.Names.DMA2D_Interrupt);

   private
      Ready : Boolean := True;
      Error : Boolean := False;
   end Sync;

   ----------
   -- Sync --
   ----------

   protected body Sync is
      ----------
      -- Wait --
      ----------

      entry Wait when Ready is
      begin
         null;
      end Wait;

      --------------------
      -- Start_Transfer --
      --------------------

      procedure Start_Transfer is
      begin
         pragma Assert (Ready);
         Ready := False;
         Error := False;

         DMA2D_Periph.CR.CEIE := True;
         DMA2D_Periph.CR.TCIE := True;
         DMA2D_Periph.CR.TEIE := True;

         DMA2D_Periph.CR.START := True;
      end Start_Transfer;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt is
      begin
         if DMA2D_Periph.ISR.CEIF or DMA2D_Periph.ISR.TEIF then
            --  Conf or transfer error
            DMA2D_Periph.IFCR.CCEIF := True;
            DMA2D_Periph.IFCR.CTEIF := True;
            Error := True;
            Ready := True;

         elsif DMA2D_Periph.ISR.TCIF then
            --  Transfer completed
            DMA2D_Periph.IFCR.CTCIF := True;
            Error := False;
            Ready := True;

         else
            --  Unexpected interrupt.
            pragma Assert (False);
         end if;
      end Interrupt;
   end Sync;

   -----------------
   -- DMA2D_Start --
   -----------------

   procedure DMA2D_Start is
   begin
      Sync.Start_Transfer;
   end DMA2D_Start;

   ----------------
   -- DMA2D_Wait --
   ----------------

   procedure DMA2D_Wait is
   begin
      Sync.Wait;
   end DMA2D_Wait;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      DMA2D_Init (Init => DMA2D_Start'Access,
                  Wait => DMA2D_Wait'Access);
   end Initialize;

end STM32.DMA2D.Interrupt;
