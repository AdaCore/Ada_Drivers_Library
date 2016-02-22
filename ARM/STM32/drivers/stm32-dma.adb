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
--   @file    stm32f4xx_hal_dma.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   DMA HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System.Storage_Elements;

with STM32_SVD.DMA; use STM32_SVD.DMA;

package body STM32.DMA is

   type DMA_Stream_Record is record
      --  configuration register
      CR   : S0CR_Register;
      --  number of data register
      NDTR : S0NDTR_Register;
      --  peripheral address register
      PAR  : STM32_SVD.Word;
      --  memory 0 address register
      M0AR : STM32_SVD.Word;
      --  memory 1 address register
      M1AR : STM32_SVD.Word;
      --  FIFO control register
      FCR  : S0FCR_Register;
   end record with Volatile;

   for DMA_Stream_Record use record
      CR   at 0  range 0 .. 31;
      NDTR at 4  range 0 .. 31;
      PAR  at 8  range 0 .. 31;
      M0AR at 12 range 0 .. 31;
      M1AR at 16 range 0 .. 31;
      FCR  at 20 range 0 .. 31;
   end record;

   type DMA_Stream is access all DMA_Stream_Record;

   function Get_Stream
     (Port : DMA_Controller;
      Num  : DMA_Stream_Selector) return DMA_Stream
     with Inline;

   procedure Set_Interrupt_Enabler
     (This_Stream : DMA_Stream;
      Source      : DMA_Interrupt;
      Value       : Boolean);
   --  An internal routine, used to enable and disable the specified interrupt

   ----------------
   -- Get_Stream --
   ----------------

   function Get_Stream
     (Port : DMA_Controller;
      Num  : DMA_Stream_Selector) return DMA_Stream
   is
      Addr : System.Address;
      function To_Stream is new Ada.Unchecked_Conversion
        (System.Address, DMA_Stream);
   begin
      case Num is
         when Stream_0 =>
            Addr := Port.S0CR'Address;
         when Stream_1 =>
            Addr := Port.S1CR'Address;
         when Stream_2 =>
            Addr := Port.S2CR'Address;
         when Stream_3 =>
            Addr := Port.S3CR'Address;
         when Stream_4 =>
            Addr := Port.S4CR'Address;
         when Stream_5 =>
            Addr := Port.S5CR'Address;
         when Stream_6 =>
            Addr := Port.S6CR'Address;
         when Stream_7 =>
            Addr := Port.S7CR'Address;
      end case;

      return To_Stream (Addr);
   end Get_Stream;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (Unit, Stream).CR.EN := 1;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
   begin
      return Get_Stream (Unit, Stream).CR.EN = 1;
   end Enabled;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (Unit, Stream).CR.EN := 0;
      -- the STMicro Reference Manual RM0090, Doc Id 018909 Rev 6, pg 319, step
      -- 1 says we must await the bit actually clearing, to confirm no ongoing
      -- operation remains active
      loop
         exit when not Enabled (Unit, Stream);
      end loop;
   end Disable;

   ---------------------------
   -- Set_Interrupt_Enabler --
   ---------------------------

   procedure Set_Interrupt_Enabler
     (This_Stream : DMA_Stream;
      Source      : DMA_Interrupt;
      Value       : Boolean)
   is
   begin
      case Source is
         when Direct_Mode_Error_Interrupt =>
            This_Stream.CR.DMEIE := Boolean'Enum_Rep (Value);
         when Transfer_Error_Interrupt =>
            This_Stream.CR.TEIE := Boolean'Enum_Rep (Value);
         when Half_Transfer_Complete_Interrupt =>
            This_Stream.CR.HTIE := Boolean'Enum_Rep (Value);
         when Transfer_Complete_Interrupt =>
            This_Stream.CR.TCIE := Boolean'Enum_Rep (Value);
         when FIFO_Error_Interrupt =>
            This_Stream.FCR.FEIE := Boolean'Enum_Rep (Value);
      end case;
   end Set_Interrupt_Enabler;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
   is
   begin
      Set_Interrupt_Enabler (Get_Stream (Unit, Stream), Source, True);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
   is
   begin
      Set_Interrupt_Enabler (Get_Stream (Unit, Stream), Source, False);
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (Unit        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : DMA_Interrupt)
      return Boolean
   is
      Result      : Boolean;
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
      --  this is a bit heavy, considering it will be called from interrupt
      --  handlers.
      --  TODO: consider a much lower level implementation, based on bit-masks.
   begin
      case Source is
         when Direct_Mode_Error_Interrupt =>
            Result := This_Stream.CR.DMEIE = 1;
         when Transfer_Error_Interrupt =>
            Result := This_Stream.CR.TEIE = 1;
         when Half_Transfer_Complete_Interrupt =>
            Result := This_Stream.CR.HTIE = 1;
         when Transfer_Complete_Interrupt =>
            Result := This_Stream.CR.TCIE = 1;
         when FIFO_Error_Interrupt =>
            Result := This_Stream.FCR.FEIE = 1;
      end case;
      return Result;
   end Interrupt_Enabled;

   --------------------
   -- Start_Transfer --
   --------------------

   procedure Start_Transfer
     (Unit        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : Half_Word)
   is
   begin
      Disable (Unit, Stream);  --  per the RM, eg section 10.5.6 for the NDTR

      Configure_Data_Flow
        (Unit,
         Stream,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      Enable (Unit, Stream);
   end Start_Transfer;

   ------------------------------------
   -- Start_Transfer_with_Interrupts --
   ------------------------------------

   procedure Start_Transfer_With_Interrupts
     (Unit               : DMA_Controller;
      Stream             : DMA_Stream_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : Half_Word;
      Enabled_Interrupts : Interrupt_Selections := (others => True))
   is
   begin
      Disable (Unit, Stream);  --  per the RM, eg section 10.5.6 for the NDTR

      Configure_Data_Flow
        (Unit,
         Stream,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      for Selected_Interrupt in Enabled_Interrupts'Range loop
         if Enabled_Interrupts (Selected_Interrupt) then
            Enable_Interrupt (Unit, Stream, Selected_Interrupt);
         end if;
      end loop;

      Enable (Unit, Stream);
   end Start_Transfer_with_Interrupts;

   -------------------------
   -- Configure_Data_Flow --
   -------------------------

   procedure Configure_Data_Flow
     (Unit        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : Half_Word)
   is
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
      function W is new Ada.Unchecked_Conversion
        (Address, Word);
   begin
      This_Stream.NDTR.NDT := Data_Count;

      if This_Stream.CR.DIR = Memory_To_Peripheral'Enum_Rep then
         This_Stream.PAR  := W (Destination);
         This_Stream.M0AR := W (Source);
      else
         This_Stream.PAR  := W (Source);
         This_Stream.M0AR := W (Destination);
      end if;
   end Configure_Data_Flow;

   --------------------
   -- Abort_Transfer --
   --------------------

   procedure Abort_Transfer
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Result : out DMA_Error_Code)
   is
      Max_Abort_Time : constant Time_Span := Seconds (1);
      Timeout        : Time;
      This_Stream    : DMA_Stream renames Get_Stream (Unit, Stream);
   begin
      Disable (Unit, Stream);
      Timeout := Clock + Max_Abort_Time;
      loop
         exit when This_Stream.CR.EN = 0;
         if Clock > Timeout then
            Result := DMA_Timeout_Error;
            return;
         end if;
      end loop;
      Result := DMA_No_Error;
   end Abort_Transfer;

   -------------------------
   -- Poll_For_Completion --
   -------------------------

   procedure Poll_For_Completion
     (Unit           : in out DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Expected_Level : DMA_Transfer_Level;
      Timeout        : Time_Span;
      Result         : out DMA_Error_Code)
   is
      Deadline : constant Time := Clock + Timeout;
   begin
      Result := DMA_No_Error;  -- initially anyway

      Polling : loop
         if Expected_Level = Full_Transfer then
            exit when Status (Unit, Stream, Transfer_Complete_Indicated);
         else
            exit when Status (Unit, Stream, Half_Transfer_Complete_Indicated);
         end if;

         if Status (Unit, Stream, Transfer_Error_Indicated) or
            Status (Unit, Stream, FIFO_Error_Indicated) or
            Status (Unit, Stream, Direct_Mode_Error_Indicated)
         then
            Clear_Status (Unit, Stream, Transfer_Error_Indicated);
            Clear_Status (Unit, Stream, FIFO_Error_Indicated);
            Clear_Status (Unit, Stream, Direct_Mode_Error_Indicated);

            Result := DMA_Device_Error;
            return;
         end if;

         if Clock > Deadline then
            Result := DMA_Timeout_Error;
            return;
         end if;
      end loop Polling;

      Clear_Status (Unit, Stream, Half_Transfer_Complete_Indicated);

      if Expected_Level = Full_Transfer then
         Clear_Status (Unit, Stream, Transfer_Complete_Indicated);
      else
         Clear_Status (Unit, Stream, Half_Transfer_Complete_Indicated);
      end if;
   end Poll_For_Completion;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
   is
   begin
      case Stream is
         when Stream_0 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.LIFCR.CFEIF0 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.LIFCR.CDMEIF0 := 1;
               when Transfer_Error_Indicated =>
                  Unit.LIFCR.CTEIF0 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.LIFCR.CHTIF0 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.LIFCR.CTCIF0 := 1;
            end case;

         when Stream_1 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.LIFCR.CFEIF1 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.LIFCR.CDMEIF1 := 1;
               when Transfer_Error_Indicated =>
                  Unit.LIFCR.CTEIF1 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.LIFCR.CHTIF1 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.LIFCR.CTCIF1 := 1;
            end case;

         when Stream_2 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.LIFCR.CFEIF2 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.LIFCR.CDMEIF2 := 1;
               when Transfer_Error_Indicated =>
                  Unit.LIFCR.CTEIF2 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.LIFCR.CHTIF2 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.LIFCR.CTCIF2 := 1;
            end case;

         when Stream_3 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.LIFCR.CFEIF3 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.LIFCR.CDMEIF3 := 1;
               when Transfer_Error_Indicated =>
                  Unit.LIFCR.CTEIF3 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.LIFCR.CHTIF3 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.LIFCR.CTCIF3 := 1;
            end case;

         when Stream_4 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.HIFCR.CFEIF4 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.HIFCR.CDMEIF4 := 1;
               when Transfer_Error_Indicated =>
                  Unit.HIFCR.CTEIF4 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.HIFCR.CHTIF4 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.HIFCR.CTCIF4 := 1;
            end case;

         when Stream_5 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.HIFCR.CFEIF5 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.HIFCR.CDMEIF5 := 1;
               when Transfer_Error_Indicated =>
                  Unit.HIFCR.CTEIF5 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.HIFCR.CHTIF5 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.HIFCR.CTCIF5 := 1;
            end case;

         when Stream_6 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.HIFCR.CFEIF6 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.HIFCR.CDMEIF6 := 1;
               when Transfer_Error_Indicated =>
                  Unit.HIFCR.CTEIF6 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.HIFCR.CHTIF6 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.HIFCR.CTCIF6 := 1;
            end case;

         when Stream_7 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  Unit.HIFCR.CFEIF7 := 1;
               when Direct_Mode_Error_Indicated =>
                  Unit.HIFCR.CDMEIF7 := 1;
               when Transfer_Error_Indicated =>
                  Unit.HIFCR.CTEIF7 := 1;
               when Half_Transfer_Complete_Indicated =>
                  Unit.HIFCR.CHTIF7 := 1;
               when Transfer_Complete_Indicated =>
                  Unit.HIFCR.CTCIF7 := 1;
            end case;
      end case;
   end Clear_Status;

   ----------------------
   -- Clear_All_Status --
   ----------------------

   procedure Clear_All_Status
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      case Stream is
         when Stream_0 =>
            Unit.LIFCR.CFEIF0 := 1;
            Unit.LIFCR.CDMEIF0 := 1;
            Unit.LIFCR.CTEIF0 := 1;
            Unit.LIFCR.CHTIF0 := 1;
            Unit.LIFCR.CTCIF0 := 1;

         when Stream_1 =>
            Unit.LIFCR.CFEIF1 := 1;
            Unit.LIFCR.CDMEIF1 := 1;
            Unit.LIFCR.CTEIF1 := 1;
            Unit.LIFCR.CHTIF1 := 1;
            Unit.LIFCR.CTCIF1 := 1;

         when Stream_2 =>
            Unit.LIFCR.CFEIF2 := 1;
            Unit.LIFCR.CDMEIF2 := 1;
            Unit.LIFCR.CTEIF2 := 1;
            Unit.LIFCR.CHTIF2 := 1;
            Unit.LIFCR.CTCIF2 := 1;

         when Stream_3 =>
            Unit.LIFCR.CFEIF3 := 1;
            Unit.LIFCR.CDMEIF3 := 1;
            Unit.LIFCR.CTEIF3 := 1;
            Unit.LIFCR.CHTIF3 := 1;
            Unit.LIFCR.CTCIF3 := 1;

         when Stream_4 =>
            Unit.HIFCR.CFEIF4 := 1;
            Unit.HIFCR.CDMEIF4 := 1;
            Unit.HIFCR.CTEIF4 := 1;
            Unit.HIFCR.CHTIF4 := 1;
            Unit.HIFCR.CTCIF4 := 1;

         when Stream_5 =>
            Unit.HIFCR.CFEIF5 := 1;
            Unit.HIFCR.CDMEIF5 := 1;
            Unit.HIFCR.CTEIF5 := 1;
            Unit.HIFCR.CHTIF5 := 1;
            Unit.HIFCR.CTCIF5 := 1;

         when Stream_6 =>
            Unit.HIFCR.CFEIF6 := 1;
            Unit.HIFCR.CDMEIF6 := 1;
            Unit.HIFCR.CTEIF6 := 1;
            Unit.HIFCR.CHTIF6 := 1;
            Unit.HIFCR.CTCIF6 := 1;

         when Stream_7 =>
            Unit.HIFCR.CFEIF7 := 1;
            Unit.HIFCR.CDMEIF7 := 1;
            Unit.HIFCR.CTEIF7 := 1;
            Unit.HIFCR.CHTIF7 := 1;
            Unit.HIFCR.CTCIF7 := 1;
      end case;
   end Clear_All_Status;

   ------------
   -- Status --
   ------------

   function Status
     (Unit    : DMA_Controller;
      Stream  : DMA_Stream_Selector;
      Flag    : DMA_Status_Flag)
      return Boolean
   is
   begin
      case Stream is
         when Stream_0 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.LISR.FEIF0 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.LISR.DMEIF0 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.LISR.TEIF0 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.LISR.HTIF0 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.LISR.TCIF0 = 1;
            end case;

         when Stream_1 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.LISR.FEIF1 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.LISR.DMEIF1 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.LISR.TEIF1 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.LISR.HTIF1 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.LISR.TCIF1 = 1;
            end case;

         when Stream_2 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.LISR.FEIF2 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.LISR.DMEIF2 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.LISR.TEIF2 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.LISR.HTIF2 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.LISR.TCIF2 = 1;
            end case;

         when Stream_3 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.LISR.FEIF3 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.LISR.DMEIF3 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.LISR.TEIF3 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.LISR.HTIF3 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.LISR.TCIF3 = 1;
            end case;

         when Stream_4 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.HISR.FEIF4 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.HISR.DMEIF4 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.HISR.TEIF4 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.HISR.HTIF4 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.HISR.TCIF4 = 1;
            end case;

         when Stream_5 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.HISR.FEIF5 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.HISR.DMEIF5 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.HISR.TEIF5 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.HISR.HTIF5 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.HISR.TCIF5 = 1;
            end case;

         when Stream_6 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.HISR.FEIF6 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.HISR.DMEIF6 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.HISR.TEIF6 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.HISR.HTIF6 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.HISR.TCIF6 = 1;
            end case;

         when Stream_7 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return Unit.HISR.FEIF7 = 1;
               when Direct_Mode_Error_Indicated =>
                  return Unit.HISR.DMEIF7 = 1;
               when Transfer_Error_Indicated =>
                  return Unit.HISR.TEIF7 = 1;
               when Half_Transfer_Complete_Indicated =>
                  return Unit.HISR.HTIF7 = 1;
               when Transfer_Complete_Indicated =>
                  return Unit.HISR.TCIF7 = 1;
            end case;
      end case;
   end Status;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_Counter
     (Unit       : DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Count : Half_Word)
   is
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
   begin
      This_Stream.NDTR.NDT := Data_Count;
   end Set_Counter;

   ---------------------
   -- Current_Counter --
   ---------------------

   function Current_Counter
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Half_Word
   is
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
   begin
      return This_Stream.NDTR.NDT;
   end Current_Counter;

   ---------------------
   -- Double_Buffered --
   ---------------------

   function Double_Buffered
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
   begin
      return Get_Stream (Unit, Stream).CR.DBM = 1;
   end Double_Buffered;

   -------------------
   -- Circular_Mode --
   -------------------

   function Circular_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
   begin
      return Get_Stream (Unit, Stream).CR.CIRC = 1;
   end Circular_Mode;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Config : DMA_Stream_Configuration)
   is
      --  see HAL_DMA_Init in STM32F4xx_HAL_Driver\Inc\stm32f4xx_hal_dma.h
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
   begin
      -- the STMicro Reference Manual RM0090, Doc Id 018909 Rev 6, pg 319 says
      -- we must disable the stream before configuring it
      Disable (Unit, Stream);

      This_Stream.CR.CT := Memory_Buffer_0'Enum_Rep;

      This_Stream.CR.CHSEL :=
        DMA_Channel_Selector'Enum_Rep (Config.Channel);
      This_Stream.CR.DIR :=
        DMA_Data_Transfer_Direction'Enum_Rep (Config.Direction);
      This_Stream.CR.PINC :=
        Boolean'Enum_Rep (Config.Increment_Peripheral_Address);
      This_Stream.CR.MINC :=
        Boolean'Enum_Rep (Config.Increment_Memory_Address);
      This_Stream.CR.PSIZE :=
        DMA_Data_Transfer_Widths'Enum_Rep (Config.Peripheral_Data_Format);
      This_Stream.CR.MSIZE :=
        DMA_Data_Transfer_Widths'Enum_Rep (Config.Memory_Data_Format);
      This_Stream.CR.PL :=
        DMA_Priority_Level'Enum_Rep (Config.Priority);

      case Config.Operation_Mode is
         when Normal_Mode =>
            This_Stream.CR.PFCTRL := 0;
            This_Stream.CR.CIRC := 0;
         when Peripheral_Flow_Control_Mode =>
            This_Stream.CR.PFCTRL := 1;
            This_Stream.CR.CIRC := 0;
         when Circular_Mode =>
            This_Stream.CR.PFCTRL := 0;
            This_Stream.CR.CIRC := 1;
      end case;

      --  the memory burst and peripheral burst values are only used when
      --  the FIFO is enabled
      if Config.FIFO_Enabled then
         This_Stream.CR.MBURST :=
           DMA_Memory_Burst'Enum_Rep (Config.Memory_Burst_Size);
         This_Stream.CR.PBURST :=
           DMA_Peripheral_Burst'Enum_Rep (Config.Peripheral_Burst_Size);
      else
         This_Stream.CR.MBURST := Memory_Burst_Single'Enum_Rep;
         This_Stream.CR.PBURST := Peripheral_Burst_Single'Enum_Rep;
      end if;

      This_Stream.FCR.DMDIS := (if Config.FIFO_Enabled then 0 else 1);

      if Config.FIFO_Enabled then
         This_Stream.FCR.FTH :=
           DMA_FIFO_Threshold_Level'Enum_Rep (Config.FIFO_Threshold);
      else
         This_Stream.FCR.FTH :=
           FIFO_Threshold_1_Quart_Full_Configuration'Enum_Rep; -- 0, default
      end if;
   end Configure;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
   begin
      Disable (Unit, Stream);

      This_Stream.CR := (others => <>);
      This_Stream.NDTR.NDT := 0;
      This_Stream.PAR := 0;
      This_Stream.M0AR := 0;
      This_Stream.M1AR := 0;
      This_Stream.FCR := (others => <>);

      Clear_All_Status (Unit, Stream);
   end Reset;

   ---------------------------
   -- Peripheral_Data_Width --
   ---------------------------

   function Peripheral_Data_Width
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
   is
   begin
      return DMA_Data_Transfer_Widths'Val
        (Get_Stream (Unit, Stream).CR.PSIZE);
   end Peripheral_Data_Width;

   -----------------------
   -- Memory_Data_Width --
   -----------------------

   function Memory_Data_Width
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
   is
   begin
      return DMA_Data_Transfer_Widths'Val
        (Get_Stream (Unit, Stream).CR.MSIZE);
   end Memory_Data_Width;

   ------------------------
   -- Transfer_Direction --
   ------------------------

   function Transfer_Direction
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Direction
   is
   begin
      return DMA_Data_Transfer_Direction'Val
        (Get_Stream (Unit, Stream).CR.DIR);
   end Transfer_Direction;

   --------------------
   -- Operating_Mode --
   --------------------

   function Operating_Mode
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Mode
   is
   begin
      if Get_Stream (Unit, Stream).CR.PFCTRL = 1 then
         return Peripheral_Flow_Control_Mode;
      elsif Get_Stream (Unit, Stream).CR.CIRC = 1 then
         return Circular_Mode;
      end if;
      return Normal_Mode;
   end Operating_Mode;

   --------------
   -- Priority --
   --------------

   function Priority
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Priority_Level
   is
   begin
      return DMA_Priority_Level'Val (Get_Stream (Unit, Stream).CR.PL);
   end Priority;

   ---------------------------
   -- Current_Memory_Buffer --
   ---------------------------

   function Current_Memory_Buffer
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return Memory_Buffer_Target
   is
   begin
      return Memory_Buffer_Target'Val (Get_Stream (Unit, Stream).CR.CT);
   end Current_Memory_Buffer;

   -----------------------
   -- Set_Memory_Buffer --
   -----------------------

   procedure Set_Memory_Buffer
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target;
      To     : System.Address)
   is
      function W is new Ada.Unchecked_Conversion
        (System.Address, Word);
      This_Stream : DMA_Stream renames Get_Stream (Unit, Stream);
   begin
      case Buffer is
         when Memory_Buffer_0 =>
            This_Stream.M0AR := W (To);
         when Memory_Buffer_1 =>
            This_Stream.M1AR := W (To);
      end case;
   end Set_Memory_Buffer;

   ----------------------------------
   -- Select_Current_Memory_Buffer --
   ----------------------------------

   procedure Select_Current_Memory_Buffer
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target)
   is
   begin
      Get_Stream (Unit, Stream).CR.CT :=
        Memory_Buffer_Target'Enum_Rep (Buffer);
   end Select_Current_Memory_Buffer;

   ------------------------------------
   -- Configure_Double_Buffered_Mode --
   ------------------------------------

   procedure Configure_Double_Buffered_Mode
     (Unit              : DMA_Controller;
      Stream            : DMA_Stream_Selector;
      Buffer_0_Value    : Address;
      Buffer_1_Value    : Address;
      First_Buffer_Used : Memory_Buffer_Target)
   is
   begin
      Set_Memory_Buffer (Unit, Stream, Memory_Buffer_0, Buffer_0_Value);
      Set_Memory_Buffer (Unit, Stream, Memory_Buffer_1, Buffer_1_Value);
      Select_Current_Memory_Buffer (Unit, Stream, First_Buffer_Used);
   end Configure_Double_Buffered_Mode;

   ---------------------------------
   -- Enable_Double_Buffered_Mode --
   ---------------------------------

   procedure Enable_Double_Buffered_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (Unit, Stream).CR.DBM := 1;
   end Enable_Double_Buffered_Mode;

   ----------------------------------
   -- Disable_Double_Buffered_Mode --
   ----------------------------------

   procedure Disable_Double_Buffered_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (Unit, Stream).CR.DBM := 0;
   end Disable_Double_Buffered_Mode;

   ----------------------
   -- Selected_Channel --
   ----------------------

   function Selected_Channel
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Channel_Selector
   is
   begin
      return DMA_Channel_Selector'Val (Get_Stream (Unit, Stream).CR.CHSEL);
   end Selected_Channel;

   -------------
   -- Aligned --
   -------------

   function Aligned (This : Address;  Width : DMA_Data_Transfer_Widths)
      return Boolean
   is
      use System.Storage_Elements;
   begin
      case Width is
         when Words =>
            return To_Integer (This) mod 4 = 0;
         when HalfWords =>
            return To_Integer (This) mod 2 = 0;
         when Bytes =>
            return True;
      end case;
   end Aligned;

end STM32.DMA;
