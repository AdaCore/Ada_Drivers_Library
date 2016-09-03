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

   type OTG_FS_All_Interrupts is
     (
      --  Accessible in host mode only.
      OTG_Periodic_TxFIFO_Empty_Int, -- Host
      OTG_Host_Channels_Int, -- Host
      OTG_Host_Port_Int, -- Host
      OTG_Incomplete_Periodic_Transfer_Int, -- Host
      OTG_Non_Periodic_TxFIFO_Empty_Int, -- Host
      OTG_Disconnect_Detected_Int, -- Host

      --  Accessible in both host and device modes
      OTG_Wakeup_Int, -- Both
      OTG_Session_Request_Int, -- Both
      OTG_Connector_ID_Status_Change_Int, -- Both
      OTG_RxFIFO_Non_Empty_Int, -- Both
      OTG_Start_Of_Frame_Int, -- Both
      OTG_Protocol_Event_Int, -- Both
      OTG_Mode_Mismatch_Int, -- Both

      --  Accessible in device mode only.
      OTG_Incomplete_Isochronous_OUT_Transfer_Int, -- Device
      OTG_Incomplete_Isochronous_IN_Transfer_Int, -- Device
      OTG_OUT_Endpoint_Int, -- Device
      OTG_IN_Endpoint_Int, -- Device
      OTG_End_Of_Periodic_Frame_Int, -- Device
      OTG_Isochronous_OUT_Packet_Dropped_Int, -- Device
      OTG_Enumeration_Done_Int, -- Device
      OTG_USB_Reset_Int, -- Device
      OTG_USB_Suspend_Int, -- Device
      OTG_Early_Suspend_Int, -- Device
      OTG_Global_OUT_NAK_Effective_Int, -- Device
      OTG_Global_IN_Non_Periodic_NAK_Effective_Int -- Device
     );

   subtype OTG_FS_Host_Interrutps is OTG_FS_All_Interrupts range
     OTG_Periodic_TxFIFO_Empty_Int .. OTG_Mode_Mismatch_Int;

   subtype OTG_FS_Device_Interrutps is OTG_FS_All_Interrupts range
     OTG_Wakeup_Int .. OTG_Global_IN_Non_Periodic_NAK_Effective_Int;

   procedure Enable_Interrupt (Int : OTG_FS_All_Interrupts; Enable : Boolean := True);
   function Interrupt_Triggered (Int : OTG_FS_All_Interrupts) return Boolean;
   procedure Clear_Interrupt (Int : OTG_FS_All_Interrupts);
   function Current_Mode return USB_Mode;

   procedure Wait_For_Event;
end STM32.USB;
