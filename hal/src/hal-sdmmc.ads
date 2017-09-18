------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2016-2017, AdaCore                        --
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

package HAL.SDMMC is

   type SD_Error is
     (OK,
      Error,
      Timeout_Error,
      Command_Timeout_Error,
      Unsupported_Card,
      Rx_Overrun,
      Tx_Underrun,
      Request_Not_Applicable,
      CRC_Check_Fail,
      Illegal_Cmd,
      Address_Out_Of_Range,
      Address_Missaligned,
      Block_Length_Error,
      Erase_Seq_Error,
      Bad_Erase_Parameter,
      Write_Protection_Violation,
      Lock_Unlock_Failed,
      Card_ECC_Failed,
      Card_ECC_Disabled,
      CC_Error,
      General_Unknown_Error,
      Stream_Read_Underrun,
      Stream_Write_Underrun,
      CID_CSD_Overwrite,
      WP_Erase_Skip,
      Erase_Reset,
      AKE_SEQ_Error,
      Invalid_Voltage_Range,
      Startbit_Not_Detected,
      DMA_Alignment_Error);

   --  Wide bus mode
   type Wide_Bus_Mode is
     (
      --  Default bus mode: SDMMC_D0 is used.
      Wide_Bus_1B,
      --  4-wide bus mode: SDMMC_D[3:0] used.
      Wide_Bus_4B,
      --  8-wide bus mode: SDMMC_D[7:0] used.
      Wide_Bus_8B)
     with Size => 2;
   for Wide_Bus_Mode use
     (Wide_Bus_1B => 0,
      Wide_Bus_4B => 1,
      Wide_Bus_8B => 2);

   type Supported_SD_Memory_Cards is
     (STD_Capacity_SD_Card_V1_1,
      STD_Capacity_SD_Card_v2_0,
      High_Capacity_SD_Card,
      Multimedia_Card,
      Secure_Digital_IO_Card,
      High_Speed_Multimedia_Card,
      Secure_Digital_IO_Combo_Card,
      High_Capacity_MMC_Card);

   type Card_Specific_Data_Register is record
      CSD_Structure                    : UInt8;
      System_Specification_Version     : UInt8;
      Reserved                         : UInt8;
      Data_Read_Access_Time_1          : UInt8;
      Data_Read_Access_Time_2          : UInt8; --  In CLK Cycles
      Max_Data_Transfer_Rate           : UInt8;
      Card_Command_Class               : UInt16;
      Max_Read_Data_Block_Length       : UInt8;
      Partial_Block_For_Read_Allowed   : Boolean;
      Write_Block_Missalignment        : Boolean;
      Read_Block_Missalignment         : Boolean;
      DSR_Implemented                  : Boolean;
      Reserved_2                       : UInt8;
      Device_Size                      : UInt32;
      Max_Read_Current_At_VDD_Min      : UInt8;
      Max_Read_Current_At_VDD_Max      : UInt8;
      Max_Write_Current_At_VDD_Min     : UInt8;
      Max_Write_Current_At_VDD_Max     : UInt8;
      Device_Size_Multiplier           : UInt8;
      Erase_Group_Size                 : UInt8;
      Erase_Group_Size_Multiplier      : UInt8;
      Write_Protect_Group_Size         : UInt8;
      Write_Protect_Group_Enable       : Boolean;
      Manufacturer_Default_ECC         : UInt8;
      Write_Speed_Factor               : UInt8;
      Max_Write_Data_Block_Length      : UInt8;
      Partial_Blocks_For_Write_Allowed : Boolean;
      Reserved_3                       : UInt8;
      Content_Protection_Application   : Boolean;
      File_Format_Group                : Boolean;
      Copy_Flag                        : Boolean;
      Permanent_Write_Protection       : Boolean;
      Temporary_Write_Protection       : Boolean;
      File_Format                      : UInt8;
      ECC_Code                         : UInt8;
      CSD_CRC                          : UInt8;
      Reserved_4                       : UInt8; --  Always 1
   end record;

   type Card_Revision is record
      Major : UInt4;
      Minor : UInt4;
   end record with Pack;

   type Manufacturing_Year is range 2000 .. 2255;
   type Manufacturing_Month is
     (January,
      February,
      March,
      April,
      May,
      June,
      July,
      August,
      September,
      October,
      November,
      December) with Size => 4;

   type Manufacturing_Date_Type is record
      Year : Manufacturing_Year;
      Month : Manufacturing_Month;
   end record;

   type Card_Identification_Data_Register is record
      Manufacturer_ID       : UInt8;
      OEM_Application_ID    : String (1 .. 2);
      Product_Name          : String (1 .. 5);
      Product_Revision      : Card_Revision;
      Product_Serial_Number : UInt32;
      Reserved_1            : UInt8;
      Manufacturing_Date    : Manufacturing_Date_Type;
      CID_CRC               : UInt8;
      Reserved_2            : UInt8; --  Always 1
   end record;

   type SDCard_Configuration_Register is record
      SCR_Structure         : UInt8;
      SD_Spec               : UInt8;
      Data_Stat_After_Erase : UInt8;
      SD_Security           : UInt8;
      SD_Bus_Widths         : UInt8;
      SD_Spec3              : Boolean;
      Ex_Security           : UInt8;
      SD_Spec4              : Boolean;
      Reserved_1            : UInt8;
      CMD_Support           : UInt8;
      Reserved_2            : UInt32;
   end record;

   type Card_Information is record
      SD_CSD          : Card_Specific_Data_Register;
      SD_CID          : Card_Identification_Data_Register;
      Card_Capacity   : UInt64;
      Card_Block_Size : UInt32;
      RCA             : UInt16; --  SD relative card address
      Card_Type       : Supported_SD_Memory_Cards :=
                          STD_Capacity_SD_Card_V1_1;
   end record;

   type SD_Command is new UInt6;

   --  Resets the SD memory card
   Go_Idle_State        : constant SD_Command := 0;

   --  Sends host capacity support information and activates the card's
   --  initialization process
   Send_Op_Cond         : constant SD_Command := 1;  --  Only for MMC

   --  Asks any card connected to the host to send the CID numbers on the
   --  CMD line.
   All_Send_CID         : constant SD_Command := 2;

   --  Asks the card to publish a new relative address (RCA).
   Send_Relative_Addr   : constant SD_Command := 3;

   --  Programs the DSR of all cards.
   Set_DSR              : constant SD_Command := 4;

   --  Sends host capacity support information (HCS) and asks the accessed
   --  card to send its operating condition register (OCR) content in the
   --  response on the CMD line.
   SDMMC_Send_Op_Cond   : constant SD_Command := 5;

   --  Checks switchable function (mode 0) and switch card function (mode 1).
   Switch_Func          : constant SD_Command := 6;

   --  Selects the card by its own relative address and gets deselected by
   --  any other address
   Select_Card          : constant SD_Command := 7;
   Deselect_Card        : constant SD_Command := 7;

   --  Sends SD Memory Card interface condition
   Send_If_Cond         : constant SD_Command := 8;

   --  Addressed card sends its card specific data
   Send_CSD             : constant SD_Command := 9;

   --  Addressed card sends its card identification (CID) on the CMD line.
   Send_CID             : constant SD_Command := 10;

   Read_Dat_Until_Stop  : constant SD_Command := 11;
   Stop_Transmission    : constant SD_Command := 12;
   Send_Status          : constant SD_Command := 13;
   HS_Bustest_Read      : constant SD_Command := 14;
   Go_Inactive_State    : constant SD_Command := 15;
   Set_Blocklen         : constant SD_Command := 16;
   Read_Single_Block    : constant SD_Command := 17;
   Read_Multi_Block     : constant SD_Command := 18;
   HS_Bustest_Write     : constant SD_Command := 19;
   Write_Dat_Until_Stop : constant SD_Command := 20;
   Set_Block_Count      : constant SD_Command := 23; --  Only for MMC
   Write_Single_Block   : constant SD_Command := 24;
   Write_Multi_Block    : constant SD_Command := 25;
   Prog_CID             : constant SD_Command := 26;
   Prog_CSD             : constant SD_Command := 27;
   Set_Write_Prot       : constant SD_Command := 28;
   Clr_Write_Prot       : constant SD_Command := 29;
   Send_Write_Prot      : constant SD_Command := 30;
   SD_Erase_Grp_Start   : constant SD_Command := 32;
   SD_Erase_Grp_End     : constant SD_Command := 33;
   Erase_Grp_Start      : constant SD_Command := 35;
   Erase_Grp_End        : constant SD_Command := 36;
   Erase                : constant SD_Command := 38;
   Fast_IO              : constant SD_Command := 39;
   Go_IRQ_State         : constant SD_Command := 40;
   Lock_Unlock          : constant SD_Command := 42;
   App_Cmd              : constant SD_Command := 55;
   Gen_Cmd              : constant SD_Command := 56;

   --  SD-Card speciric commands
   --  App_Cmd should be sent before sending these commands
   subtype SD_Specific_Command is SD_Command;

   SD_App_Set_Bus_Width               : constant SD_Specific_Command := 6;
   SD_App_Status                      : constant SD_Specific_Command := 13;
   SD_App_Secure_Read_Multi_Block     : constant SD_Specific_Command := 18;
   SD_App_Send_Num_Write_Blocks       : constant SD_Specific_Command := 22;
   SD_App_Set_Write_Block_Erase_Count : constant SD_Specific_Command := 23;
   SD_App_Secure_Write_Multi_Block    : constant SD_Specific_Command := 25;
   SD_App_Secure_Erase                : constant SD_Specific_Command := 38;
   SD_App_Send_Op_Cond                : constant SD_Specific_Command := 41;
   SD_App_Get_MKB                     : constant SD_Specific_Command := 43;
   SD_App_Get_MID                     : constant SD_Specific_Command := 44;
   SD_App_Set_CER_RN1                 : constant SD_Specific_Command := 45;
   SD_App_Get_CER_RN2                 : constant SD_Specific_Command := 46;
   SD_App_Set_CER_RES2                : constant SD_Specific_Command := 47;
   SD_App_Get_CER_RES1                : constant SD_Specific_Command := 48;
   SD_App_Change_Secure_Area          : constant SD_Specific_Command := 49;
   SD_App_Send_SCR                    : constant SD_Specific_Command := 51;

   type Rsp_Kind is
     (Rsp_Invalid,
      --  Unknown/invalid command

      Rsp_No,
      --  No response.

      Rsp_R1, Rsp_R1B, Rsp_R2, Rsp_R3, Rsp_R6, Rsp_R7
      --  Response with format
     );
   --  Response format

   type Tfr_Kind is
     (Tfr_Invalid, Tfr_No,
      Tfr_Read, Tfr_Write,
      Tfr_Read_Multi, Tfr_Write_Multi);
   --  Transfer

   type Cmd_Desc_Type is record
      Cmd : SD_Command;
      Rsp : Rsp_Kind;
      Tfr : Tfr_Kind;
   end record;

   type Cmd_Desc_Array is array (SD_Command) of Cmd_Desc_Type;
   type ACmd_Desc_Array is array (SD_Specific_Command) of Cmd_Desc_Type;

   Cmd_Desc : constant Cmd_Desc_Array :=
     (Go_Idle_State      => (Cmd => Go_Idle_State,
                             Rsp => Rsp_No,
                             Tfr => Tfr_No),
      Send_Op_Cond       => (Cmd => Send_Op_Cond,
                             Rsp => Rsp_R3,
                             Tfr => Tfr_No),
      All_Send_CID       => (Cmd => All_Send_CID,
                             Rsp => Rsp_R2,
                             Tfr => Tfr_No),
      Send_Relative_Addr => (Cmd => Send_Relative_Addr,
                             Rsp => Rsp_R3,
                             Tfr => Tfr_No),
      Switch_Func        => (Cmd => Switch_Func,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_Read),
      Select_Card        => (Cmd => Select_Card,
                             Rsp => Rsp_R1B,
                             Tfr => Tfr_No),
      Send_If_Cond       => (Cmd => Send_If_Cond,
                             Rsp => Rsp_R7,
                             Tfr => Tfr_No),
      Send_CSD           => (Cmd => Send_CSD,
                             Rsp => Rsp_R2,
                             Tfr => Tfr_No),
      Send_CID           => (Cmd => Send_CID,
                             Rsp => Rsp_R2,
                             Tfr => Tfr_No),
      Stop_Transmission  => (Cmd => Stop_Transmission,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_No),
      Read_Single_Block  => (Cmd => Read_Single_Block,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_Read),
      Read_Multi_Block   => (Cmd => Read_Multi_Block,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_Read_Multi),
      Send_Status        => (Cmd => Send_Status,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_No),
      Set_Blocklen       => (Cmd => Set_Blocklen,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_No),
      App_Cmd            => (Cmd => App_Cmd,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_No),
      Write_Single_Block => (Cmd => Write_Single_Block,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_Write),
      Write_Multi_Block  => (Cmd => Write_Multi_Block,
                             Rsp => Rsp_R1,
                             Tfr => Tfr_Write_Multi),
      others             => (Cmd => 0,
                             Rsp => Rsp_Invalid,
                             Tfr => Tfr_Invalid));

   Acmd_Desc : constant ACmd_Desc_Array :=
     (SD_App_Set_Bus_Width => (Cmd => SD_App_Set_Bus_Width,
                               Rsp => Rsp_R1,
                               Tfr => Tfr_No),
      SD_App_Send_Op_Cond  => (Cmd => SD_App_Send_Op_Cond,
                               Rsp => Rsp_R3,
                               Tfr => Tfr_No),
      SD_App_Send_SCR      => (Cmd => SD_App_Send_SCR,
                               Rsp => Rsp_R1,
                               Tfr => Tfr_Read),
      others               => (Cmd => 0,
                               Rsp => Rsp_Invalid,
                               Tfr => Tfr_Invalid));

   type SDMMC_Driver is limited interface;

   procedure Delay_Milliseconds
     (This   : SDMMC_Driver;
      Amount : Natural) is abstract;
   --  Do not use directly 'delay until' so that this package can still be
   --  used with the zfp profile

   procedure Reset
     (This   : in out SDMMC_Driver;
      Status : out SD_Error) is abstract;
   --  Initialize the driver, enable clocking.

   procedure Set_Clock
     (This : in out SDMMC_Driver;
      Freq : Natural) is abstract;
   --  Set clock frequency.

   procedure Set_Bus_Size
     (This : in out SDMMC_Driver;
      Mode : Wide_Bus_Mode) is abstract;
   --  Set host bus size; the command must have been set to the card.

   procedure Send_Cmd
     (This   : in out SDMMC_Driver;
      Cmd    : Cmd_Desc_Type;
      Arg    : UInt32;
      Status : out SD_Error) is abstract;
   --  Send a command (without data transfer) and wait for result.

   procedure Read_Cmd
     (This   : in out SDMMC_Driver;
      Cmd    : Cmd_Desc_Type;
      Arg    : UInt32;
      Buf    : out UInt32_Array;
      Status : out SD_Error) is abstract;
   --  Read data command

   procedure Read_Rsp48
     (This : in out SDMMC_Driver;
      Rsp  : out UInt32) is abstract;
   --  Read the 32 interesting bits of the last 48bits response (start bit,
   --  transmission bit, command index, crc and end bit are discarded).
   --  Cannot fail.

   procedure Read_Rsp136
     (This           : in out SDMMC_Driver;
      W0, W1, W2, W3 : out UInt32) is abstract;
   --  Read the 128 interesting bits of the last 136 bit response.
   --  W0 is the MSB, W3 the LSB
   --  Cannot fail.

   procedure Send_Cmd
     (This   : in out SDMMC_Driver'Class;
      Cmd    : SD_Command;
      Arg    : UInt32;
      Status : out SD_Error);
   --  Wrapper for Send_Cmd using a generic command.

   procedure Send_ACmd
     (This   : in out SDMMC_Driver'Class;
      Cmd    : SD_Specific_Command;
      Rca    : UInt16;
      Arg    : UInt32;
      Status : out SD_Error);
   --  Send application specific command

end HAL.SDMMC;
