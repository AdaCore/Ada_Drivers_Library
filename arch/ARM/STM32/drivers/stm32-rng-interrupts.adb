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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_rng.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   RNG HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Interrupts.Names;

package body STM32.RNG.Interrupts is

   type Buffer_Content is array (Integer range <>) of Unsigned_32;

   type Ring_Buffer is record
      Content : Buffer_Content (0 .. 9);
      Head    : Integer := 0;
      Tail    : Integer := 0;
   end record;

   --------------
   -- Receiver --
   --------------

   protected Receiver is
      pragma Interrupt_Priority;
      entry Get_Random_32 (Value : out Unsigned_32);
   private

      Last           : Unsigned_32 := 0;
      Buffer         : Ring_Buffer;
      Data_Available : Boolean := False;

      procedure Interrupt_Handler;

      pragma Attach_Handler
        (Interrupt_Handler,
         Ada.Interrupts.Names.HASH_RNG_Interrupt);

   end Receiver;

   --------------
   -- Receiver --
   --------------

   protected body Receiver is

      -------------------
      -- Get_Random_32 --
      -------------------

      entry Get_Random_32 (Value : out Unsigned_32) when Data_Available is
         Next : constant Integer :=
           (Buffer.Tail + 1) mod Buffer.Content'Length;
      begin
         --  Remove an item from our ring buffer.
         Value := Buffer.Content (Next);
         Buffer.Tail := Next;

         --  If the buffer is empty, make sure we block subsequent callers
         --  until the buffer has something in it.
         if Buffer.Tail = Buffer.Head then
            Data_Available := False;
         end if;

         Enable_RNG;
      end Get_Random_32;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
         Current : Unsigned_32;
      begin
         if RNG_Seed_Error_Status then
            Clear_RNG_Seed_Error_Status;

            --  Clear then set the RNGEN bit to reinitialize and restart
            --  the RNG.
            Reset_RNG;
         end if;

         if RNG_Clock_Error_Status then
            --  TODO: reconfigure the clock and make sure it's okay

            --  Clear the bit.
            Clear_RNG_Clock_Error_Status;
         end if;

         if RNG_Data_Ready then
            Current := RNG_Data;

            if Current /= Last then
               --  This number is good.
               if (Buffer.Head + 1) mod Buffer.Content'Length = Buffer.Tail
               then
                  --  But our buffer is full.  Turn off the RNG.
                  Disable_RNG;
               else
                  --  Add this new data to our buffer.
                  Buffer.Head := (Buffer.Head + 1) mod Buffer.Content'Length;
                  Buffer.Content (Buffer.Head) := Current;

                  Data_Available := True;
                  Last := Current;
               end if;
            end if;
         end if;
      end Interrupt_Handler;

   end Receiver;

   --------------------
   -- Initialize_RNG --
   --------------------

   procedure Initialize_RNG is
      Discard : Unsigned_32;
   begin
      Enable_RNG_Clock;
      Enable_RNG_Interrupt;
      Enable_RNG;

      --  Discard the first randomly generated number, according to STM32F4
      --  docs.
      Receiver.Get_Random_32 (Discard);
   end Initialize_RNG;

   ------------
   -- Random --
   ------------

   function Random return Unsigned_32 is
      Result : Unsigned_32;
   begin
      Receiver.Get_Random_32 (Result);
      return Result;
   end Random;

end STM32.RNG.Interrupts;
