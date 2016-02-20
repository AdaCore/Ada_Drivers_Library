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

with STM32.Device; use STM32.Device;

package STM32.DSI is

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

   procedure DSI_Initialize
     (PLL_N_Div                : DSI_PLLN_Div;
      PLL_IN_Div               : DSI_PLL_IDF;
      PLL_OUT_Div              : DSI_PLL_ODF;
      Auto_Clock_Lane_Control  : Boolean;
      TX_Escape_Clock_Division : Byte;
      --  The TX_ESC clock division. 0 or 1 stops the clock.
      Number_Of_Lanes          : DSI_Number_Of_Lanes);

   procedure DSI_Deinit;

   subtype DSI_Virtual_Channel_ID is Bits_2;

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

   procedure DSI_Setup_Video_Mode
     (Virtual_Channel             : DSI_Virtual_Channel_ID;
      Color_Coding                : DSI_Color_Mode;
      Loosely_Packed              : Boolean;
      Video_Mode                  : DSI_Video_Mode;
      Packet_Size                 : Bits_15;
      Number_Of_Chunks            : Bits_14;
      Null_Packet_Size            : Bits_14;
      HSync_Polarity              : DSI_Polarity;
      VSync_Polarity              : DSI_Polarity;
      DataEn_Polarity             : DSI_Polarity;
      HSync_Active_Duration       : Bits_13;
      Horizontal_BackPorch        : Bits_13;
      Horizontal_Line             : Bits_15;
      VSync_Active_Duration       : Bits_10;
      Vertical_BackPorch          : Bits_10;
      Vertical_FrontPorch         : Bits_10;
      Vertical_Active             : Bits_14;
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
     (Virtual_Channel             : DSI_Virtual_Channel_ID;
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
     (LP_Gen_Short_Write_No_P  : Boolean := True;
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
     (Flow_Control : DSI_Flow_Control);

   procedure DSI_Start;
   procedure DSI_Stop;
   procedure DSI_Refresh;

   type DSI_Data is array (Positive range <>) of Byte;

   type DSI_Pkt_Data_Type is
     (DCS_Short_Pkt_Write_P0, --  DCS Short write, no parameter
      DCS_Short_Pkt_Write_P1, --  DCS Short write, one parameter
      Gen_Short_Pkt_Write_P0, --  Generic Short write, no parameter
      Gen_Short_Pkt_Write_P1, --  Generic Short write, one parameter
      Gen_Short_Pkt_Write_P2, --  Generic Short write, two parameters
      DCS_Long_Pkt_Write,     --  DCS Long write
      Gen_Long_Pkt_Write,     --  Generic Long write
      DCS_Short_Pkt_Read,     --  DCS Short read
      Gen_Short_Pkg_Read_P0,  --  Gen read, no parameter
      Gen_Short_Pkg_Read_P1,  --  Gen read, one parameter
      Gen_Short_Pkg_Read_P2); --  Gen read, two parameter

   subtype DSI_Short_Write_Packet_Data_Type is DSI_Pkt_Data_Type range
     DCS_Short_Pkt_Write_P0 .. Gen_Short_Pkt_Write_P2;

   subtype DSI_Long_Write_Packet_Data_Type is DSI_Pkt_Data_Type range
     DCS_Long_Pkt_Write .. Gen_Long_Pkt_Write;

   procedure DSI_Short_Write
     (Channel_ID : DSI_Virtual_Channel_ID;
      Mode       : DSI_Short_Write_Packet_Data_Type;
      Param1     : Byte;
      Param2     : Byte);

   procedure DSI_Long_Write
     (Channel_Id : DSI_Virtual_Channel_ID;
      Mode       : DSI_Long_Write_Packet_Data_Type;
      Param1     : Byte;
      Parameters : DSI_Data);

end STM32.DSI;
