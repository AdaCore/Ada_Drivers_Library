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

   type DMA_Stream is access all Stream_Cluster;

   function Get_Stream
     (Port : DMA_Controller;
      Num  : DMA_Stream_Selector) return DMA_Stream
     with Inline;

   procedure Set_Interrupt_Enabler
     (This_Stream : DMA_Stream;
      Source      : DMA_Interrupt;
      Value       : Boolean);
   --  An internal routine, used to enable and disable the specified interrupt

   function "-" is new Ada.Unchecked_Conversion
     (Memory_Buffer_Target, Boolean);
   function "-" is new Ada.Unchecked_Conversion
     (Boolean, Memory_Buffer_Target);

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
      Addr := Port.Stream (DMA_Stream_Selector'Enum_Rep (Num))'Address;

      return To_Stream (Addr);
   end Get_Stream;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (This, Stream).SxCR.EN := True;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
   begin
      return Get_Stream (This, Stream).SxCR.EN;
   end Enabled;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (This, Stream).SxCR.EN := False;
      --  the STMicro Reference Manual RM0090, Doc Id 018909 Rev 6, pg 319,
      --  step 1 says we must await the bit actually clearing, to confirm no
      --  ongoing operation remains active.
      loop
         exit when not Enabled (This, Stream);
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
            This_Stream.SxCR.DMEIE := Value;
         when Transfer_Error_Interrupt =>
            This_Stream.SxCR.TEIE := Value;
         when Half_Transfer_Complete_Interrupt =>
            This_Stream.SxCR.HTIE := Value;
         when Transfer_Complete_Interrupt =>
            This_Stream.SxCR.TCIE := Value;
         when FIFO_Error_Interrupt =>
            This_Stream.SxFCR.FEIE := Value;
      end case;
   end Set_Interrupt_Enabler;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
   is
   begin
      Set_Interrupt_Enabler (Get_Stream (This, Stream), Source, True);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
   is
   begin
      Set_Interrupt_Enabler (Get_Stream (This, Stream), Source, False);
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : DMA_Interrupt)
      return Boolean
   is
      Result      : Boolean;
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
      --  this is a bit heavy, considering it will be called from interrupt
      --  handlers.
      --  TODO: consider a much lower level implementation, based on bit-masks.
   begin
      case Source is
         when Direct_Mode_Error_Interrupt =>
            Result := This_Stream.SxCR.DMEIE;
         when Transfer_Error_Interrupt =>
            Result := This_Stream.SxCR.TEIE;
         when Half_Transfer_Complete_Interrupt =>
            Result := This_Stream.SxCR.HTIE;
         when Transfer_Complete_Interrupt =>
            Result := This_Stream.SxCR.TCIE;
         when FIFO_Error_Interrupt =>
            Result := This_Stream.SxFCR.FEIE;
      end case;
      return Result;
   end Interrupt_Enabled;

   --------------------
   -- Start_Transfer --
   --------------------

   procedure Start_Transfer
     (This        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16)
   is
   begin
      Disable (This, Stream);  --  per the RM, eg section 10.5.6 for the NDTR

      Configure_Data_Flow
        (This,
         Stream,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      Enable (This, Stream);
   end Start_Transfer;

   ------------------------------------
   -- Start_Transfer_with_Interrupts --
   ------------------------------------

   procedure Start_Transfer_with_Interrupts
     (This               : DMA_Controller;
      Stream             : DMA_Stream_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : UInt16;
      Enabled_Interrupts : Interrupt_Selections := (others => True))
   is
   begin
      Disable (This, Stream);  --  per the RM, eg section 10.5.6 for the NDTR

      Configure_Data_Flow
        (This,
         Stream,
         Source      => Source,
         Destination => Destination,
         Data_Count  => Data_Count);

      for Selected_Interrupt in Enabled_Interrupts'Range loop
         if Enabled_Interrupts (Selected_Interrupt) then
            Enable_Interrupt (This, Stream, Selected_Interrupt);
         end if;
      end loop;

      Enable (This, Stream);
   end Start_Transfer_with_Interrupts;

   -------------------------
   -- Configure_Data_Flow --
   -------------------------

   procedure Configure_Data_Flow
     (This        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16)
   is
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
      function W is new Ada.Unchecked_Conversion
        (Address, UInt32);
   begin
      --  the following assignment has NO EFFECT if flow is controlled by
      --  peripheral. The hardware resets it to 16#FFFF#, see RM0090 10.3.15.
      This_Stream.SxNDTR.NDT := Data_Count;

      if This_Stream.SxCR.DIR = Memory_To_Peripheral'Enum_Rep then
         This_Stream.SxM0AR := W (Source);
         This_Stream.SxPAR  := W (Destination);
      else
         This_Stream.SxPAR  := W (Source);
         This_Stream.SxM0AR := W (Destination);
      end if;
   end Configure_Data_Flow;

   --------------------
   -- Abort_Transfer --
   --------------------

   procedure Abort_Transfer
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Result : out DMA_Error_Code)
   is
      Max_Abort_Time : constant Time_Span := Seconds (1);
      Timeout        : Time;
      This_Stream    : DMA_Stream renames Get_Stream (This, Stream);
   begin
      Disable (This, Stream);
      Timeout := Clock + Max_Abort_Time;
      loop
         exit when not This_Stream.SxCR.EN;
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
     (This           : in out DMA_Controller;
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
            exit Polling when
              Status (This, Stream, Transfer_Complete_Indicated);
         else
            exit Polling when
              Status (This, Stream, Half_Transfer_Complete_Indicated);
         end if;

         if Status (This, Stream, Transfer_Error_Indicated) or
            Status (This, Stream, FIFO_Error_Indicated) or
            Status (This, Stream, Direct_Mode_Error_Indicated)
         then
            Clear_Status (This, Stream, Transfer_Error_Indicated);
            Clear_Status (This, Stream, FIFO_Error_Indicated);
            Clear_Status (This, Stream, Direct_Mode_Error_Indicated);

            Result := DMA_Device_Error;
            return;
         end if;

         if Clock > Deadline then
            Result := DMA_Timeout_Error;
            return;
         end if;
      end loop Polling;

      Clear_Status (This, Stream, Half_Transfer_Complete_Indicated);

      if Expected_Level = Full_Transfer then
         Clear_Status (This, Stream, Transfer_Complete_Indicated);
      else
         Clear_Status (This, Stream, Half_Transfer_Complete_Indicated);
      end if;
   end Poll_For_Completion;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
   is
   begin
      case Stream is
         when Stream_0 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.LIFCR.CFEIF0 := True;
               when Direct_Mode_Error_Indicated =>
                  This.LIFCR.CDMEIF0 := True;
               when Transfer_Error_Indicated =>
                  This.LIFCR.CTEIF0 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.LIFCR.CHTIF0 := True;
               when Transfer_Complete_Indicated =>
                  This.LIFCR.CTCIF0 := True;
            end case;

         when Stream_1 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.LIFCR.CFEIF1 := True;
               when Direct_Mode_Error_Indicated =>
                  This.LIFCR.CDMEIF1 := True;
               when Transfer_Error_Indicated =>
                  This.LIFCR.CTEIF1 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.LIFCR.CHTIF1 := True;
               when Transfer_Complete_Indicated =>
                  This.LIFCR.CTCIF1 := True;
            end case;

         when Stream_2 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.LIFCR.CFEIF2 := True;
               when Direct_Mode_Error_Indicated =>
                  This.LIFCR.CDMEIF2 := True;
               when Transfer_Error_Indicated =>
                  This.LIFCR.CTEIF2 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.LIFCR.CHTIF2 := True;
               when Transfer_Complete_Indicated =>
                  This.LIFCR.CTCIF2 := True;
            end case;

         when Stream_3 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.LIFCR.CFEIF3 := True;
               when Direct_Mode_Error_Indicated =>
                  This.LIFCR.CDMEIF3 := True;
               when Transfer_Error_Indicated =>
                  This.LIFCR.CTEIF3 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.LIFCR.CHTIF3 := True;
               when Transfer_Complete_Indicated =>
                  This.LIFCR.CTCIF3 := True;
            end case;

         when Stream_4 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.HIFCR.CFEIF4 := True;
               when Direct_Mode_Error_Indicated =>
                  This.HIFCR.CDMEIF4 := True;
               when Transfer_Error_Indicated =>
                  This.HIFCR.CTEIF4 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.HIFCR.CHTIF4 := True;
               when Transfer_Complete_Indicated =>
                  This.HIFCR.CTCIF4 := True;
            end case;

         when Stream_5 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.HIFCR.CFEIF5 := True;
               when Direct_Mode_Error_Indicated =>
                  This.HIFCR.CDMEIF5 := True;
               when Transfer_Error_Indicated =>
                  This.HIFCR.CTEIF5 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.HIFCR.CHTIF5 := True;
               when Transfer_Complete_Indicated =>
                  This.HIFCR.CTCIF5 := True;
            end case;

         when Stream_6 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.HIFCR.CFEIF6 := True;
               when Direct_Mode_Error_Indicated =>
                  This.HIFCR.CDMEIF6 := True;
               when Transfer_Error_Indicated =>
                  This.HIFCR.CTEIF6 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.HIFCR.CHTIF6 := True;
               when Transfer_Complete_Indicated =>
                  This.HIFCR.CTCIF6 := True;
            end case;

         when Stream_7 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  This.HIFCR.CFEIF7 := True;
               when Direct_Mode_Error_Indicated =>
                  This.HIFCR.CDMEIF7 := True;
               when Transfer_Error_Indicated =>
                  This.HIFCR.CTEIF7 := True;
               when Half_Transfer_Complete_Indicated =>
                  This.HIFCR.CHTIF7 := True;
               when Transfer_Complete_Indicated =>
                  This.HIFCR.CTCIF7 := True;
            end case;
      end case;
   end Clear_Status;

   ----------------------
   -- Clear_All_Status --
   ----------------------

   procedure Clear_All_Status
     (This   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      case Stream is
         when Stream_0 =>
            This.LIFCR.CFEIF0 := True;
            This.LIFCR.CDMEIF0 := True;
            This.LIFCR.CTEIF0 := True;
            This.LIFCR.CHTIF0 := True;
            This.LIFCR.CTCIF0 := True;

         when Stream_1 =>
            This.LIFCR.CFEIF1 := True;
            This.LIFCR.CDMEIF1 := True;
            This.LIFCR.CTEIF1 := True;
            This.LIFCR.CHTIF1 := True;
            This.LIFCR.CTCIF1 := True;

         when Stream_2 =>
            This.LIFCR.CFEIF2 := True;
            This.LIFCR.CDMEIF2 := True;
            This.LIFCR.CTEIF2 := True;
            This.LIFCR.CHTIF2 := True;
            This.LIFCR.CTCIF2 := True;

         when Stream_3 =>
            This.LIFCR.CFEIF3 := True;
            This.LIFCR.CDMEIF3 := True;
            This.LIFCR.CTEIF3 := True;
            This.LIFCR.CHTIF3 := True;
            This.LIFCR.CTCIF3 := True;

         when Stream_4 =>
            This.HIFCR.CFEIF4 := True;
            This.HIFCR.CDMEIF4 := True;
            This.HIFCR.CTEIF4 := True;
            This.HIFCR.CHTIF4 := True;
            This.HIFCR.CTCIF4 := True;

         when Stream_5 =>
            This.HIFCR.CFEIF5 := True;
            This.HIFCR.CDMEIF5 := True;
            This.HIFCR.CTEIF5 := True;
            This.HIFCR.CHTIF5 := True;
            This.HIFCR.CTCIF5 := True;

         when Stream_6 =>
            This.HIFCR.CFEIF6 := True;
            This.HIFCR.CDMEIF6 := True;
            This.HIFCR.CTEIF6 := True;
            This.HIFCR.CHTIF6 := True;
            This.HIFCR.CTCIF6 := True;

         when Stream_7 =>
            This.HIFCR.CFEIF7 := True;
            This.HIFCR.CDMEIF7 := True;
            This.HIFCR.CTEIF7 := True;
            This.HIFCR.CHTIF7 := True;
            This.HIFCR.CTCIF7 := True;
      end case;
   end Clear_All_Status;

   ------------
   -- Status --
   ------------

   function Status
     (This    : DMA_Controller;
      Stream  : DMA_Stream_Selector;
      Flag    : DMA_Status_Flag)
      return Boolean
   is
   begin
      case Stream is
         when Stream_0 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.LISR.FEIF0;
               when Direct_Mode_Error_Indicated =>
                  return This.LISR.DMEIF0;
               when Transfer_Error_Indicated =>
                  return This.LISR.TEIF0;
               when Half_Transfer_Complete_Indicated =>
                  return This.LISR.HTIF0;
               when Transfer_Complete_Indicated =>
                  return This.LISR.TCIF0;
            end case;

         when Stream_1 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.LISR.FEIF1;
               when Direct_Mode_Error_Indicated =>
                  return This.LISR.DMEIF1;
               when Transfer_Error_Indicated =>
                  return This.LISR.TEIF1;
               when Half_Transfer_Complete_Indicated =>
                  return This.LISR.HTIF1;
               when Transfer_Complete_Indicated =>
                  return This.LISR.TCIF1;
            end case;

         when Stream_2 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.LISR.FEIF2;
               when Direct_Mode_Error_Indicated =>
                  return This.LISR.DMEIF2;
               when Transfer_Error_Indicated =>
                  return This.LISR.TEIF2;
               when Half_Transfer_Complete_Indicated =>
                  return This.LISR.HTIF2;
               when Transfer_Complete_Indicated =>
                  return This.LISR.TCIF2;
            end case;

         when Stream_3 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.LISR.FEIF3;
               when Direct_Mode_Error_Indicated =>
                  return This.LISR.DMEIF3;
               when Transfer_Error_Indicated =>
                  return This.LISR.TEIF3;
               when Half_Transfer_Complete_Indicated =>
                  return This.LISR.HTIF3;
               when Transfer_Complete_Indicated =>
                  return This.LISR.TCIF3;
            end case;

         when Stream_4 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.HISR.FEIF4;
               when Direct_Mode_Error_Indicated =>
                  return This.HISR.DMEIF4;
               when Transfer_Error_Indicated =>
                  return This.HISR.TEIF4;
               when Half_Transfer_Complete_Indicated =>
                  return This.HISR.HTIF4;
               when Transfer_Complete_Indicated =>
                  return This.HISR.TCIF4;
            end case;

         when Stream_5 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.HISR.FEIF5;
               when Direct_Mode_Error_Indicated =>
                  return This.HISR.DMEIF5;
               when Transfer_Error_Indicated =>
                  return This.HISR.TEIF5;
               when Half_Transfer_Complete_Indicated =>
                  return This.HISR.HTIF5;
               when Transfer_Complete_Indicated =>
                  return This.HISR.TCIF5;
            end case;

         when Stream_6 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.HISR.FEIF6;
               when Direct_Mode_Error_Indicated =>
                  return This.HISR.DMEIF6;
               when Transfer_Error_Indicated =>
                  return This.HISR.TEIF6;
               when Half_Transfer_Complete_Indicated =>
                  return This.HISR.HTIF6;
               when Transfer_Complete_Indicated =>
                  return This.HISR.TCIF6;
            end case;

         when Stream_7 =>
            case Flag is
               when FIFO_Error_Indicated =>
                  return This.HISR.FEIF7;
               when Direct_Mode_Error_Indicated =>
                  return This.HISR.DMEIF7;
               when Transfer_Error_Indicated =>
                  return This.HISR.TEIF7;
               when Half_Transfer_Complete_Indicated =>
                  return This.HISR.HTIF7;
               when Transfer_Complete_Indicated =>
                  return This.HISR.TCIF7;
            end case;
      end case;
   end Status;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_NDT
     (This       : DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Count : UInt16)
   is
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
   begin
      This_Stream.SxNDTR.NDT := Data_Count;
   end Set_NDT;

   function Items_Transferred
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return UInt16
   is
      ndt : constant UInt16 := Current_NDT (This, Stream);
      items : UInt16;
   begin
      if Operating_Mode (This, Stream) = Peripheral_Flow_Control_Mode then
         items := 16#ffff# - ndt;
      else
         items := ndt;
      end if;
      return items;
   end Items_Transferred;

   function Current_NDT
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return UInt16
   is
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
   begin
      return This_Stream.SxNDTR.NDT;
   end Current_NDT;

   ---------------------
   -- Double_Buffered --
   ---------------------

   function Double_Buffered
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
   begin
      return Get_Stream (This, Stream).SxCR.DBM;
   end Double_Buffered;

   -------------------
   -- Circular_Mode --
   -------------------

   function Circular_Mode
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
   begin
      return Get_Stream (This, Stream).SxCR.CIRC;
   end Circular_Mode;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Config : DMA_Stream_Configuration)
   is
      --  see HAL_DMA_Init in STM32F4xx_HAL_Driver\Inc\stm32f4xx_hal_dma.h
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
   begin
      --  the STMicro Reference Manual RM0090, Doc Id 018909 Rev 6, pg 319 says
      --  we must disable the stream before configuring it
      Disable (This, Stream);

      This_Stream.SxCR.CT  := -Memory_Buffer_0;
      This_Stream.SxCR.DBM := False;

      This_Stream.SxCR.CHSEL :=
        DMA_Channel_Selector'Enum_Rep (Config.Channel);
      This_Stream.SxCR.DIR :=
        DMA_Data_Transfer_Direction'Enum_Rep (Config.Direction);
      This_Stream.SxCR.PINC := Config.Increment_Peripheral_Address;
      This_Stream.SxCR.MINC := Config.Increment_Memory_Address;
      This_Stream.SxCR.PSIZE :=
        DMA_Data_Transfer_Widths'Enum_Rep (Config.Peripheral_Data_Format);
      This_Stream.SxCR.MSIZE :=
        DMA_Data_Transfer_Widths'Enum_Rep (Config.Memory_Data_Format);
      This_Stream.SxCR.PL :=
        DMA_Priority_Level'Enum_Rep (Config.Priority);

      case Config.Operation_Mode is
         when Normal_Mode =>
            This_Stream.SxCR.PFCTRL := False; --  DMA is the flow controller
            This_Stream.SxCR.CIRC   := False; --  Disable circular mode
         when Peripheral_Flow_Control_Mode =>
            This_Stream.SxCR.PFCTRL := True;  --  Peripheral is the flow ctrl.
            This_Stream.SxCR.CIRC   := False; --  Disable circular mode
         when Circular_Mode =>
            This_Stream.SxCR.PFCTRL := False; --  DMA is the flow controller
            This_Stream.SxCR.CIRC   := True;  --  Enable circular mode
      end case;

      --  the memory burst and peripheral burst values are only used when
      --  the FIFO is enabled
      if Config.FIFO_Enabled then
         This_Stream.SxCR.MBURST :=
           DMA_Memory_Burst'Enum_Rep (Config.Memory_Burst_Size);
         This_Stream.SxCR.PBURST :=
           DMA_Peripheral_Burst'Enum_Rep (Config.Peripheral_Burst_Size);
      else
         This_Stream.SxCR.MBURST := Memory_Burst_Single'Enum_Rep;
         This_Stream.SxCR.PBURST := Peripheral_Burst_Single'Enum_Rep;
      end if;

      --  DMDIS: disable the direct mode (set to True) when FIFO is enabled
      This_Stream.SxFCR.DMDIS := Config.FIFO_Enabled;

      if Config.FIFO_Enabled then
         This_Stream.SxFCR.FTH :=
           DMA_FIFO_Threshold_Level'Enum_Rep (Config.FIFO_Threshold);
      else
         This_Stream.SxFCR.FTH :=
           FIFO_Threshold_1_Quart_Full_Configuration'Enum_Rep; -- 0, default
      end if;
   end Configure;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (This   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
   begin
      Disable (This, Stream);

      This_Stream.SxCR := (others => <>);
      This_Stream.SxNDTR.NDT := 0;
      This_Stream.SxPAR := 0;
      This_Stream.SxM0AR := 0;
      This_Stream.SxM1AR := 0;
      This_Stream.SxFCR := (others => <>);

      Clear_All_Status (This, Stream);
   end Reset;

   ---------------------------
   -- Peripheral_Data_Width --
   ---------------------------

   function Peripheral_Data_Width
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
   is
   begin
      return DMA_Data_Transfer_Widths'Val
        (Get_Stream (This, Stream).SxCR.PSIZE);
   end Peripheral_Data_Width;

   -----------------------
   -- Memory_Data_Width --
   -----------------------

   function Memory_Data_Width
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
   is
   begin
      return DMA_Data_Transfer_Widths'Val
        (Get_Stream (This, Stream).SxCR.MSIZE);
   end Memory_Data_Width;

   ------------------------
   -- Transfer_Direction --
   ------------------------

   function Transfer_Direction
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Direction
   is
   begin
      return DMA_Data_Transfer_Direction'Val
        (Get_Stream (This, Stream).SxCR.DIR);
   end Transfer_Direction;

   --------------------
   -- Operating_Mode --
   --------------------

   function Operating_Mode
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Mode
   is
   begin
      if Get_Stream (This, Stream).SxCR.PFCTRL then
         return Peripheral_Flow_Control_Mode;
      elsif Get_Stream (This, Stream).SxCR.CIRC then
         return Circular_Mode;
      end if;
      return Normal_Mode;
   end Operating_Mode;

   --------------
   -- Priority --
   --------------

   function Priority
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Priority_Level
   is
   begin
      return DMA_Priority_Level'Val (Get_Stream (This, Stream).SxCR.PL);
   end Priority;

   ---------------------------
   -- Current_Memory_Buffer --
   ---------------------------

   function Current_Memory_Buffer
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return Memory_Buffer_Target
   is
   begin
      return -Get_Stream (This, Stream).SxCR.CT;
   end Current_Memory_Buffer;

   -----------------------
   -- Set_Memory_Buffer --
   -----------------------

   procedure Set_Memory_Buffer
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target;
      To     : System.Address)
   is
      function W is new Ada.Unchecked_Conversion
        (System.Address, UInt32);
      This_Stream : DMA_Stream renames Get_Stream (This, Stream);
   begin
      case Buffer is
         when Memory_Buffer_0 =>
            This_Stream.SxM0AR := W (To);
         when Memory_Buffer_1 =>
            This_Stream.SxM1AR := W (To);
      end case;
   end Set_Memory_Buffer;

   ----------------------------------
   -- Select_Current_Memory_Buffer --
   ----------------------------------

   procedure Select_Current_Memory_Buffer
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target)
   is
   begin
      Get_Stream (This, Stream).SxCR.CT := -Buffer;
   end Select_Current_Memory_Buffer;

   ------------------------------------
   -- Configure_Double_Buffered_Mode --
   ------------------------------------

   procedure Configure_Double_Buffered_Mode
     (This              : DMA_Controller;
      Stream            : DMA_Stream_Selector;
      Buffer_0_Value    : Address;
      Buffer_1_Value    : Address;
      First_Buffer_Used : Memory_Buffer_Target)
   is
   begin
      Set_Memory_Buffer (This, Stream, Memory_Buffer_0, Buffer_0_Value);
      Set_Memory_Buffer (This, Stream, Memory_Buffer_1, Buffer_1_Value);
      Select_Current_Memory_Buffer (This, Stream, First_Buffer_Used);
   end Configure_Double_Buffered_Mode;

   ---------------------------------
   -- Enable_Double_Buffered_Mode --
   ---------------------------------

   procedure Enable_Double_Buffered_Mode
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (This, Stream).SxCR.DBM := True;
   end Enable_Double_Buffered_Mode;

   ----------------------------------
   -- Disable_Double_Buffered_Mode --
   ----------------------------------

   procedure Disable_Double_Buffered_Mode
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
   begin
      Get_Stream (This, Stream).SxCR.DBM := False;
   end Disable_Double_Buffered_Mode;

   ----------------------
   -- Selected_Channel --
   ----------------------

   function Selected_Channel
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Channel_Selector
   is
   begin
      return DMA_Channel_Selector'Val (Get_Stream (This, Stream).SxCR.CHSEL);
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
