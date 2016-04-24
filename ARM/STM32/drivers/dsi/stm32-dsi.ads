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
--   @file    stm32f4xx_hal_dsi.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.4.2                                                        --
--   @date    10-November-2015                                              --
--                                                                          --
--   COPYRIGHT(c) 2015 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32_SVD.DSIHOST;
with HAL.DSI;

package STM32.DSI is
   package SVD_DSI renames STM32_SVD.DSIHOST;

   type DSI_Number_Of_Lanes is
     (One_Data_Lane,
      Two_Data_Lanes)
     with Size => 2;
   --  Values 2#10# and 2#11# are reserved

   subtype DSI_PLLN_Div is UInt7 range 10 .. 125;
   subtype DSI_PLL_IDF is UInt4 range 0 .. 7;
   PLL_IN_DIV1 : constant DSI_PLL_IDF := 1;
   PLL_IN_DIV2 : constant DSI_PLL_IDF := 2;
   PLL_IN_DIV3 : constant DSI_PLL_IDF := 3;
   PLL_IN_DIV4 : constant DSI_PLL_IDF := 4;
   PLL_IN_DIV5 : constant DSI_PLL_IDF := 5;
   PLL_IN_DIV6 : constant DSI_PLL_IDF := 6;
   PLL_IN_DIV7 : constant DSI_PLL_IDF := 7;

   type DSI_PLL_ODF is
     (PLL_OUT_DIV1,
      PLL_OUT_DIV2,
      PLL_OUT_DIV4,
      PLL_OUT_DIV8)
     with Size => 2;

   type DSI_Color_Mode is
     (RGB565,
      RGB666,
      RGB888)
     with Size => 3;
   for DSI_Color_Mode use
     (RGB565 => 0, --  1 and 2 can also be used for this mode
      RGB666 => 3, --  4 can also be used for this mode
      RGB888 => 5);

   type DSI_Video_Mode is
     (Video_Mode_NB_Pulses,
      Video_Mode_NB_Events,
      Video_Mode_Burst);

   type DSI_Polarity is
     (Active_High,
      Active_Low)
     with Size => 1;

   type DSI_Edge is
     (Falling_Edge,
      Rising_Edge)
     with Size => 1;

   type DSI_TE_Polarity is
     (Rising_Edge,
      Falling_Edge)
     with size => 1;

   type DSI_Tearing_Effect_Source is
     (TE_DSI_Link,
      TE_External)
     with Size => 1;

   type DSI_Flow_Control is
     (Flow_Control_CRC_RX,
      Flow_Control_ECC_RX,
      Flow_Control_BTA,
      Flow_Control_EOTP_RX,
      Flow_Control_EOTP_TX);

   type DSI_Host (Periph : not null access SVD_DSI.DSIHOST_Peripheral) is
     limited new HAL.DSI.DSI_Port with private;


   procedure DSI_Initialize
     (This                     : in out DSI_Host;
      PLL_N_Div                : DSI_PLLN_Div;
      PLL_IN_Div               : DSI_PLL_IDF;
      PLL_OUT_Div              : DSI_PLL_ODF;
      Auto_Clock_Lane_Control  : Boolean;
      TX_Escape_Clock_Division : Byte;
      --  The TX_ESC clock division. 0 or 1 stops the clock.
      Number_Of_Lanes          : DSI_Number_Of_Lanes);

   procedure DSI_Deinit (This : in out DSI_Host);

   procedure DSI_Setup_Video_Mode
     (This                        : in out DSI_Host;
      Virtual_Channel             : HAL.DSI.DSI_Virtual_Channel_ID;
      Color_Coding                : DSI_Color_Mode;
      Loosely_Packed              : Boolean;
      Video_Mode                  : DSI_Video_Mode;
      Packet_Size                 : UInt15;
      Number_Of_Chunks            : UInt14;
      Null_Packet_Size            : UInt14;
      HSync_Polarity              : DSI_Polarity;
      VSync_Polarity              : DSI_Polarity;
      DataEn_Polarity             : DSI_Polarity;
      HSync_Active_Duration       : UInt13;
      Horizontal_BackPorch        : UInt13;
      Horizontal_Line             : UInt15;
      VSync_Active_Duration       : UInt10;
      Vertical_BackPorch          : UInt10;
      Vertical_FrontPorch         : UInt10;
      Vertical_Active             : UInt14;
      LP_Command_Enabled          : Boolean;
      LP_Largest_Packet_Size      : Byte;
      LP_VACT_Largest_Packet_Size : Byte;
      LP_H_Front_Porch_Enable     : Boolean;
      LP_H_Back_Porch_Enable      : Boolean;
      LP_V_Active_Enable          : Boolean;
      LP_V_Front_Porch_Enable     : Boolean;
      LP_V_Back_Porch_Enable      : Boolean;
      LP_V_Sync_Active_Enable     : Boolean;
      Frame_BTA_Ack_Enable        : Boolean);

   procedure DSI_Setup_Adapted_Command_Mode
     (This                        : in out DSI_Host;
      Virtual_Channel             : HAL.DSI.DSI_Virtual_Channel_ID;
      Color_Coding                : DSI_Color_Mode;
      Command_Size                : Short;
      Tearing_Effect_Source       : DSI_Tearing_Effect_Source;
      Tearing_Effect_Polarity     : DSI_TE_Polarity;
      HSync_Polarity              : DSI_Polarity;
      VSync_Polarity              : DSI_Polarity;
      DataEn_Polarity             : DSI_Polarity;
      VSync_Edge                  : DSI_Edge;
      Automatic_Refresh           : Boolean;
      TE_Acknowledge_Request      : Boolean);

   procedure DSI_Setup_Command
     (This                     : in out DSI_Host;
      LP_Gen_Short_Write_No_P  : Boolean := True;
      LP_Gen_Short_Write_One_P : Boolean := True;
      LP_Gen_Short_Write_Two_P : Boolean := True;
      LP_Gen_Short_Read_No_P   : Boolean := True;
      LP_Gen_Short_Read_One_P  : Boolean := True;
      LP_Gen_Short_Read_Two_P  : Boolean := True;
      LP_Gen_Long_Write        : Boolean := True;
      LP_DCS_Short_Write_No_P  : Boolean := True;
      LP_DCS_Short_Write_One_P : Boolean := True;
      LP_DCS_Short_Read_No_P   : Boolean := True;
      LP_DCS_Long_Write        : Boolean := True;
      LP_Max_Read_Packet       : Boolean := False;
      Acknowledge_Request      : Boolean := False);

   procedure DSI_Setup_Flow_Control
     (This         : in out DSI_Host;
      Flow_Control : DSI_Flow_Control);

   procedure DSI_Start (This : in out DSI_Host);
   procedure DSI_Stop (This : in out DSI_Host);
   procedure DSI_Refresh (This : in out DSI_Host);
   procedure DSI_Wrapper_Disable (This : in out DSI_Host);
   procedure DSI_Wrapper_Enable (This : in out DSI_Host);

   overriding
   procedure DSI_Short_Write
     (This       : in out DSI_Host;
      Channel_ID : HAL.DSI.DSI_Virtual_Channel_ID;
      Mode       : HAL.DSI.DSI_Short_Write_Packet_Data_Type;
      Param1     : Byte;
      Param2     : Byte);

   overriding
   procedure DSI_Long_Write
     (This       : in out DSI_Host;
      Channel_Id : HAL.DSI.DSI_Virtual_Channel_ID;
      Mode       : HAL.DSI.DSI_Long_Write_Packet_Data_Type;
      Param1     : Byte;
      Parameters : HAL.DSI.DSI_Data);
private
   type DSI_Host (Periph : not null access SVD_DSI.DSIHOST_Peripheral) is
     limited new HAL.DSI.DSI_Port with null record;

end STM32.DSI;
