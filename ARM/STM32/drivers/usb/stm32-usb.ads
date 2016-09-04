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

with HAL.USB; use HAL.USB;

---------------
-- STM32.USB --
---------------

package STM32.USB is

   type USB_Mode is (USB_Host, USB_Device);

   type USB_PHY_Kind is (USB_PHY_ULPI, USB_PHY_EMBEDDED);

   procedure Core_Init (Mode                : USB_Mode;
                        Phy_Interface       : USB_PHY_Kind;
                        Speed               : HAL.USB.Speed;
                        Enable_DMA          : Boolean;
                        EP0_Max_Packet_Size : Word;
                        Use_External_VBus   : Boolean;
                        SOF_Output          : Boolean)
     with Pre => (if Phy_Interface = USB_PHY_EMBEDDED then Speed = USB_Full_Speed);

   procedure Core_Reset;

   type USB_All_Interrupts is
     (
      --  Accessible in host mode only.
      Periodic_TxFIFO_Empty_Int,
      Host_Channels_Int,
      Host_Port_Int,
      Incomplete_Periodic_Transfer_Int,
      Non_Periodic_TxFIFO_Empty_Int,
      Disconnect_Detected_Int,
      Debounce_Done,

      --  Accessible in both host and device modes
      Wakeup_Int,
      Session_Request_Int,
      Connector_ID_Status_Change_Int,
      RxFIFO_Non_Empty_Int,
      Start_Of_Frame_Int,
      Protocol_Event_Int,
      Mode_Mismatch_Int,
      A_Device_Timeout_Change_Int,
      Host_Negotiation_Detected_Int,
      Host_Negotiation_Success_Status_Change_Int,
      Session_Request_Success_Status_Change_Int,
      Session_End_Detected_Int,

      --  Accessible in device mode only.
      Incomplete_Isochronous_OUT_Transfer_Int,
      Incomplete_Isochronous_IN_Transfer_Int,
      OUT_Endpoint_Int,
      IN_Endpoint_Int,
      End_Of_Periodic_Frame_Int,
      Isochronous_OUT_Packet_Dropped_Int,
      Enumeration_Done_Int,
      USB_Reset_Int,
      USB_Suspend_Int,
      Early_Suspend_Int,
      Global_OUT_NAK_Effective_Int,
      Global_IN_Non_Periodic_NAK_Effective_Int
     );

   subtype USB_Host_Interrutps is USB_All_Interrupts range
     Periodic_TxFIFO_Empty_Int .. Session_End_Detected_Int;

   subtype USB_Device_Interrutps is USB_All_Interrupts range
     Wakeup_Int .. Global_IN_Non_Periodic_NAK_Effective_Int;

   procedure Enable_Interrupt (Int : USB_All_Interrupts; Enable : Boolean := True);
   function Interrupt_Triggered (Int : USB_All_Interrupts) return Boolean;
   procedure Clear_Interrupt (Int : USB_All_Interrupts);
   function Current_Mode return USB_Mode;

   procedure Wait_For_Event;
end STM32.USB;
