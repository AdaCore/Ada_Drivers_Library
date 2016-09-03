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

package body STM32.USB is

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

            OTG_FS_GLOBAL_Periph.FS_GINTMSK.USBRST   := True;
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.ENUMDNEM := True;
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.ESUSPM   := True;
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.USBSUSPM := True;
            OTG_FS_GLOBAL_Periph.FS_GINTMSK.SOFM     := True;

            OTG_FS_GLOBAL_Periph.FS_GCCFG.VBUSBSEN := True;

            while not OTG_FS_GLOBAL_Periph.FS_GINTMSK.USBRST loop
               null;
            end loop;
            while not OTG_FS_GLOBAL_Periph.FS_GINTMSK.ENUMDNEM loop
               null;
            end loop;

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

      --  Check Session request/new session detected interrupt. This mean that
      --  a cable is connected.
      while not OTG_FS_GLOBAL_Periph.FS_GINTSTS.SRQINT loop
         null;
      end loop;

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
end STM32.USB;
