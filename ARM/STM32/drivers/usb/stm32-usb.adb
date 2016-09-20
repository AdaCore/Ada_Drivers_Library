------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--   @file    stm32f7xx_ll_usb.h                                            --
--   @author  MCD Application Team                                          --
--   @version V1.0.0                                                        --
--   @date    12-May-2015                                                   --
--   @brief   Header file of USB Core HAL module.                           --
--                                                                          --
--   COPYRIGHT(c) 2015 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time;        use Ada.Real_Time;
with STM32_SVD.USB_OTG_FS; use STM32_SVD.USB_OTG_FS;
with STM32_SVD.RCC;        use STM32_SVD.RCC;
with Ada.Interrupts.Names;
with STM32.RCC; use STM32.RCC;
with Semihosting;

package body STM32.USB is

   procedure Handle_Interrupt (Int : USB_All_Interrupts);

   protected IRQ_Handlers is
      pragma Interrupt_Priority;
      entry Get_Event;
   private

      Got_Event : Boolean;
      procedure WKUP_Interrupt_Handler;
      pragma Attach_Handler
        (WKUP_Interrupt_Handler,
         Ada.Interrupts.Names.OTG_FS_WKUP_Interrupt);

      procedure FS_Interrupt_Handler;
      pragma Attach_Handler
        (FS_Interrupt_Handler,
         Ada.Interrupts.Names.OTG_FS_Interrupt);

   end IRQ_Handlers;

   ----------------------
   -- Handle_Interrupt --
   ----------------------

   procedure Handle_Interrupt (Int : USB_All_Interrupts) is
   begin
      case Int is
         when Wakeup_Int =>
            --  Not handled...
            Semihosting.Log_Line ("USB Wakeup");
         when Session_Request_Int =>
            --  Check Session request/new session detected interrupt. This mean that
            --  a cable is connected.
            --  Not handled...
            Semihosting.Log_Line ("Session Request");
         when Disconnect_Detected_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Disconnect detected");
         when Connector_ID_Status_Change_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Connector ID status change");
         when Periodic_TxFIFO_Empty_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Periodic TxFIFO Empty");
         when Host_Channels_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Host Channels");
         when Host_Port_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Host Port");
         when Incomplete_Periodic_Transfer_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Incomplete Periodic Transfer");
         when Incomplete_Isochronous_IN_Transfer_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Incomplete Isochronous IN Transfer");
         when Incomplete_Isochronous_OUT_Transfer_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Incomplete Isochronous OUT Transfer");
         when OUT_Endpoint_Int =>
            --  Not handled...
            Semihosting.Log_Line ("OUT Endpoint Int");
         when IN_Endpoint_Int =>
            --  Not handled...
            Semihosting.Log_Line ("IN Endpoint Int");
         when End_Of_Periodic_Frame_Int =>
            --  Not handled...
            Semihosting.Log_Line ("End Of Periodic frame");
         when Isochronous_OUT_Packet_Dropped_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Isochronous OUT package dropped");
         when Enumeration_Done_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Enumeration Done");
         when USB_Reset_Int =>
            --  Not handled...
            Semihosting.Log_Line ("USB Reset");
         when USB_Suspend_Int =>
            --  Not handled...
            Semihosting.Log_Line ("USB Suspend");
         when Early_Suspend_Int =>
            --  Not handled...
            Semihosting.Log_Line ("USB Suspend");
         when Global_OUT_NAK_Effective_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Global OUT NAK Effective");
         when Global_IN_Non_Periodic_NAK_Effective_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Global IN Non Periodic NAK Effective");
         when Non_Periodic_TxFIFO_Empty_Int =>
            --  Not handled...
            Semihosting.Log_Line ("NON Periodic TxFIFO Empty");
         when RxFIFO_Non_Empty_Int =>
            --  Not handled...
            Semihosting.Log_Line ("RxFIFO Non Empty");
         when Start_Of_Frame_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Start of Frame");
         when Protocol_Event_Int =>
            --  Not handled...
            Semihosting.Log_Line ("OTG Protocol event");
         when Mode_Mismatch_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Mode Mismatch");
         when A_Device_Timeout_Change_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Device timout change");
         when Host_Negotiation_Detected_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Host negotiation detected");
         when Host_Negotiation_Success_Status_Change_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Host negotiation success status change");
         when Session_Request_Success_Status_Change_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Session request susccess status change");
         when Session_End_Detected_Int =>
            --  Not handled...
            Semihosting.Log_Line ("Session end detected");
         when Debounce_Done =>
            --  Not handled...
            Semihosting.Log_Line ("Debounce done");
      end case;
      Clear_Interrupt (Int);
   end Handle_Interrupt;

   ------------------
   -- IRQ_Handlers --
   ------------------

   protected body IRQ_Handlers is

      entry Get_Event when Got_Event is
      begin
         Got_Event := False;
      end Get_Event;

      procedure WKUP_Interrupt_Handler is
      begin
         Got_Event := True;
         raise Program_Error;
      end WKUP_Interrupt_Handler;

      procedure FS_Interrupt_Handler is
      begin
         Got_Event := True;

         for Int in USB_All_Interrupts loop
            if
              (Current_Mode = USB_Host and then Int in USB_Host_Interrutps)
              or else
              (Current_Mode = USB_Device and then Int in USB_Device_Interrutps)
            then
               if Interrupt_Triggered (Int) then
                  Handle_Interrupt (Int);
               end if;
            end if;
         end loop;
      end FS_Interrupt_Handler;
   end IRQ_Handlers;

   --------------------
   -- Wait_For_Event --
   --------------------

   procedure Wait_For_Event is
   begin
      IRQ_Handlers.Get_Event;
   end Wait_For_Event;

   ---------------
   -- Core_Init --
   ---------------

   procedure Core_Init (Mode                : USB_Mode;
                        Phy_Interface       : USB_PHY_Kind;
                        Speed               : HAL.USB.Speed;
                        Enable_DMA          : Boolean;
                        EP0_Max_Packet_Size : Word;
                        Use_External_VBus   : Boolean;
                        SOF_Output          : Boolean)
   is
      pragma Unreferenced (EP0_Max_Packet_Size, Enable_DMA, Use_External_VBus);
   begin
      SYSCFG_Clock_Enable;

      --  Enable clock
      RCC_Periph.AHB2ENR.OTGFSEN := True;

      case Phy_Interface is
         when USB_PHY_ULPI =>

            raise Program_Error;

         when USB_PHY_EMBEDDED =>

            --  Select embedded PHY
            OTG_FS_GLOBAL_Periph.FS_GUSBCFG.PHYSEL := True;

            Core_Reset;

            --  Leave power down
            OTG_FS_GLOBAL_Periph.FS_GCCFG.PWRDWN := False;
            OTG_FS_GLOBAL_Periph.FS_GCCFG.VBUSASEN := False;
            OTG_FS_GLOBAL_Periph.FS_GCCFG.VBUSBSEN := False;

            OTG_FS_GLOBAL_Periph.FS_GCCFG.SOFOUTEN := SOF_Output;
      end case;

--        if Enable_DMA then
--           OTG_FS_GLOBAL_Periph.FS_GAHBCFG.HBSTLEN := 2#0110#;
--        end if;
--        OTG_FS_GLOBAL_Periph.FS_GAHBCFG. := Enable_DMA;

      case Mode is
         when USB_Device =>

            OTG_FS_GLOBAL_Periph.FS_GUSBCFG.HNPCAP := True;
            OTG_FS_GLOBAL_Periph.FS_GUSBCFG.SRPCAP := True;


            if Speed = USB_Full_Speed then
               OTG_FS_DEVICE_Periph.FS_DCFG.DSPD := 2#11#;
            else
               OTG_FS_DEVICE_Periph.FS_DCFG.DSPD := 2#00#;
            end if;

            OTG_FS_DEVICE_Periph.FS_DCFG.NZLSOHSK := False;

            --  Disable all interrupts...
            for Interrupt in USB_All_Interrupts loop
               Enable_Interrupt (Interrupt, False);
            end loop;

            --  Clear all interrupts...
            for Interrupt in USB_All_Interrupts loop
               Clear_Interrupt (Interrupt);
            end loop;

            --  Enable all interupt for that mode
            if Mode = USB_Host then
               for Interrupt in USB_Host_Interrutps loop
                  Enable_Interrupt (Interrupt);
               end loop;
            else
               for Interrupt in USB_Device_Interrutps loop
                  Enable_Interrupt (Interrupt);
               end loop;
            end if;

            OTG_FS_GLOBAL_Periph.FS_GCCFG.VBUSBSEN := True;
         when USB_Host =>
            raise Program_Error;
      end case;

      OTG_FS_GLOBAL_Periph.FS_GUSBCFG.FHMOD := False;
      OTG_FS_GLOBAL_Periph.FS_GUSBCFG.FDMOD := False;

      case Mode is
         when USB_Device =>
            OTG_FS_GLOBAL_Periph.FS_GUSBCFG.FDMOD := True;
         when USB_Host =>
            OTG_FS_GLOBAL_Periph.FS_GUSBCFG.FHMOD := True;
      end case;

      delay until Clock + Milliseconds (50);

      --  Set the NAK bit for all OUT endpoints
      OTG_FS_DEVICE_Periph.DOEPCTL0.SNAK := True;
      OTG_FS_DEVICE_Periph.DOEPCTL1.SNAK := True;
      OTG_FS_DEVICE_Periph.DOEPCTL2.SNAK := True;
      OTG_FS_DEVICE_Periph.DOEPCTL3.SNAK := True;


      --  Set address and size in USB RAM
      OTG_FS_GLOBAL_Periph.FS_GRXFSIZ.RXFD      := 128;
      OTG_FS_GLOBAL_Periph.FS_DIEPTXF1.INEPTXSA := 128;
      OTG_FS_GLOBAL_Periph.FS_DIEPTXF1.INEPTXFD := 96;
      OTG_FS_GLOBAL_Periph.FS_HPTXFSIZ.PTXSA    := 128 + 96;
      OTG_FS_GLOBAL_Periph.FS_HPTXFSIZ.PTXFSIZ  := 96;

      OTG_FS_DEVICE_Periph.FS_DIEPCTL0.MPSIZ := 0; -- 64 bytes?
      OTG_FS_DEVICE_Periph.FS_DIEPCTL0.EPENA := True;

      --  Enable interrupts master switch
      OTG_FS_GLOBAL_Periph.FS_GAHBCFG.GINT := True;
   end Core_Init;

   ----------------
   -- Core_Reset --
   ----------------

   procedure Core_Reset is
      Cnt : Natural := 0;
   begin
      while not OTG_FS_GLOBAL_Periph.FS_GRSTCTL.AHBIDL loop
         Cnt := Cnt + 1;
         if Cnt > 200_000 then
            return;
         end if;
      end loop;

      Cnt := 0;
      OTG_FS_GLOBAL_Periph.FS_GRSTCTL.CSRST := True;

      while not OTG_FS_GLOBAL_Periph.FS_GRSTCTL.CSRST loop
         Cnt := Cnt + 1;
         if Cnt > 200_000 then
            return;
         end if;
      end loop;
   end Core_Reset;

   procedure Enable_Interrupt (Int : USB_All_Interrupts; Enable : Boolean := True) is
   begin
      case Int is
         when Wakeup_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.WUIM := Enable;
         when Session_Request_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.SRQIM := Enable;
         when Disconnect_Detected_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.DISCINT := Enable;
         when Connector_ID_Status_Change_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.CIDSCHGM := Enable;
         when Periodic_TxFIFO_Empty_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.PTXFEM := Enable;
         when Host_Channels_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.HCIM := Enable;
         when Host_Port_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.PRTIM := Enable;
         when Incomplete_Periodic_Transfer_Int | Incomplete_Isochronous_OUT_Transfer_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.IPXFRM_IISOOXFRM := Enable;
         when Incomplete_Isochronous_IN_Transfer_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.IISOIXFRM := Enable;
         when OUT_Endpoint_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.OEPINT := Enable;
         when IN_Endpoint_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.IEPINT := Enable;
         when End_Of_Periodic_Frame_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.EOPFM := Enable;
         when Isochronous_OUT_Packet_Dropped_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.ISOODRPM := Enable;
         when Enumeration_Done_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.ENUMDNEM := Enable;
         when USB_Reset_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.USBRST := Enable;
         when USB_Suspend_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.USBSUSPM := Enable;
         when Early_Suspend_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.ESUSPM := Enable;
         when Global_OUT_NAK_Effective_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.GONAKEFFM := Enable;
         when Global_IN_Non_Periodic_NAK_Effective_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.GINAKEFFM := Enable;
         when Non_Periodic_TxFIFO_Empty_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.NPTXFEM := Enable;
         when RxFIFO_Non_Empty_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.RXFLVLM := Enable;
         when Start_Of_Frame_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.SOFM := Enable;
         when Mode_Mismatch_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.MMISM := Enable;
         when Protocol_Event_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.OTGINT := Enable;
         when A_Device_Timeout_Change_Int =>
            null;
         when Host_Negotiation_Detected_Int =>
            null;
         when Host_Negotiation_Success_Status_Change_Int =>
            null;
         when Session_Request_Success_Status_Change_Int =>
            null;
         when Session_End_Detected_Int =>
            null;
         when Debounce_Done =>
            null;
      end case;
   end Enable_Interrupt;

   -------------------------
   -- Interrupt_Triggered --
   -------------------------

   function Interrupt_Triggered (Int : USB_All_Interrupts) return Boolean is
   begin
      case Int is
         when Wakeup_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.WKUPINT;
         when Session_Request_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.SRQINT;
         when Disconnect_Detected_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.DISCINT;
         when Connector_ID_Status_Change_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.CIDSCHG;
         when Periodic_TxFIFO_Empty_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.PTXFE;
         when Host_Channels_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.HCINT;
         when Host_Port_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.HPRTINT;
         when Incomplete_Periodic_Transfer_Int | Incomplete_Isochronous_OUT_Transfer_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.IPXFR_INCOMPISOOUT;
         when Incomplete_Isochronous_IN_Transfer_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.IISOIXFR;
         when OUT_Endpoint_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.OEPINT;
         when IN_Endpoint_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.IEPINT;
         when End_Of_Periodic_Frame_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.EOPF;
         when Isochronous_OUT_Packet_Dropped_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.ISOODRP;
         when Enumeration_Done_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.ENUMDNE;
         when USB_Reset_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.USBRST;
         when USB_Suspend_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.USBSUSP;
         when Early_Suspend_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.ESUSP;
         when Global_OUT_NAK_Effective_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.GOUTNAKEFF;
         when Global_IN_Non_Periodic_NAK_Effective_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.GINAKEFF;
         when Non_Periodic_TxFIFO_Empty_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.NPTXFE;
         when RxFIFO_Non_Empty_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.RXFLVL;
         when Start_Of_Frame_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.SOF;
         when Protocol_Event_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.OTGINT;
         when Mode_Mismatch_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GINTSTS.MMIS;
         when A_Device_Timeout_Change_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GOTGINT.ADTOCHG;
         when Host_Negotiation_Detected_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GOTGINT.HNGDET;
         when Host_Negotiation_Success_Status_Change_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GOTGINT.HNSSCHG;
         when Session_Request_Success_Status_Change_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GOTGINT.SRSSCHG;
         when Session_End_Detected_Int =>
            return OTG_FS_GLOBAL_Periph.FS_GOTGINT.SEDET;
         when Debounce_Done =>
            return OTG_FS_GLOBAL_Periph.FS_GOTGINT.DBCDNE;
      end case;
   end Interrupt_Triggered;

   ---------------------
   -- Clear_Interrupt --
   ---------------------

   procedure Clear_Interrupt (Int : USB_All_Interrupts) is
   begin
      case Int is
         when Wakeup_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.WKUPINT := True;
         when Session_Request_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.SRQINT := True;
         when Disconnect_Detected_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.DISCINT := True;
         when Connector_ID_Status_Change_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.CIDSCHG := True;
         when Periodic_TxFIFO_Empty_Int =>
            null; --  Read-only interrupt
         when Host_Channels_Int =>
            null; --  Read-only interrupt
         when Host_Port_Int =>
            null; --  Read-only interrupt
         when Incomplete_Periodic_Transfer_Int | Incomplete_Isochronous_OUT_Transfer_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.IPXFR_INCOMPISOOUT := True;
         when Incomplete_Isochronous_IN_Transfer_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.IISOIXFR := True;
         when OUT_Endpoint_Int =>
            null; --  Read-only interrupt
         when IN_Endpoint_Int =>
            null; --  Read-only interrupt
         when End_Of_Periodic_Frame_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.EOPF := True;
         when Isochronous_OUT_Packet_Dropped_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.ISOODRP := True;
         when Enumeration_Done_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.ENUMDNE := True;
         when USB_Reset_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.USBRST := True;
         when USB_Suspend_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.USBSUSP := True;
         when Early_Suspend_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.ESUSP := True;
         when Global_OUT_NAK_Effective_Int =>
            null; --  Read-only interrupt
         when Global_IN_Non_Periodic_NAK_Effective_Int =>
            null; --  Read-only interrupt
         when Non_Periodic_TxFIFO_Empty_Int =>
            null; --  Read-only interrupt
         when RxFIFO_Non_Empty_Int =>
            null; --  Read-only interrupt
         when Start_Of_Frame_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.SOF := True;
         when Protocol_Event_Int =>
            null; --  Read-only interrupt
         when Mode_Mismatch_Int =>
            OTG_FS_GLOBAL_Periph.FS_GINTSTS.MMIS := True;
         when A_Device_Timeout_Change_Int =>
            OTG_FS_GLOBAL_Periph.FS_GOTGINT.ADTOCHG := True;
         when Host_Negotiation_Detected_Int =>
            OTG_FS_GLOBAL_Periph.FS_GOTGINT.HNGDET := True;
         when Host_Negotiation_Success_Status_Change_Int =>
            OTG_FS_GLOBAL_Periph.FS_GOTGINT.HNSSCHG := True;
         when Session_Request_Success_Status_Change_Int =>
            OTG_FS_GLOBAL_Periph.FS_GOTGINT.SRSSCHG := True;
         when Session_End_Detected_Int =>
            OTG_FS_GLOBAL_Periph.FS_GOTGINT.SEDET := True;
         when Debounce_Done =>
            OTG_FS_GLOBAL_Periph.FS_GOTGINT.DBCDNE := True;
      end case;
   end Clear_Interrupt;

   ------------------
   -- Current_Mode --
   ------------------

   function Current_Mode return USB_Mode is
   begin
      if OTG_FS_GLOBAL_Periph.FS_GINTSTS.CMOD then
         return USB_Host;
      else
         return USB_Device;
      end if;
   end Current_Mode;

end STM32.USB;
