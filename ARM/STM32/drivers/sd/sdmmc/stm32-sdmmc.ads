--  Initially based on stm32f7xx_hal_sd.h
--  V1.0.4
--  09-December-2015
--
--  SDCard driver. Controls the SDMMC peripheral.

with System;
with STM32_SVD.SDMMC; use STM32_SVD.SDMMC;

with STM32.DMA;

package STM32.SDMMC is

   type SDMMC_Controller is private;

   function As_Controller
     (Periph : access STM32_SVD.SDMMC.SDMMC1_Peripheral)
      return SDMMC_Controller;

   type SD_Error is
     (OK,
      Error,
      Timeout_Error,
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
      Invalid_Voltage_Range);

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
      CSD_Structure                    : Byte;
      System_Specification_Version     : Byte;
      Reserved                         : Byte;
      Data_Read_Access_Time_1          : Byte;
      Data_Read_Access_Time_2          : Byte; --  In CLK Cycles
      Max_Bus_Clock_Frequency          : Byte;
      Card_Command_Class               : Short;
      Max_Read_Data_Block_Length       : Byte;
      Partial_Block_For_Read_Allowed   : Boolean;
      Write_Block_Missalignment        : Boolean;
      Read_Block_Missalignment         : Boolean;
      DSR_Implemented                  : Boolean;
      Reserved_2                       : Byte;
      Device_Size                      : Word;
      Max_Read_Current_At_VDD_Min      : Byte;
      Max_Read_Current_At_VDD_Max      : Byte;
      Max_Write_Current_At_VDD_Min     : Byte;
      Max_Write_Current_At_VDD_Max     : Byte;
      Device_Size_Multiplier           : Byte;
      Erase_Group_Size                 : Byte;
      Erase_Group_Size_Multiplier      : Byte;
      Write_Protect_Group_Size         : Byte;
      Write_Protect_Group_Enable       : Boolean;
      Manufacturer_Default_ECC         : Byte;
      Write_Speed_Factor               : Byte;
      Max_Write_Data_Block_Length      : Byte;
      Partial_Blocks_For_Write_Allowed : Boolean;
      Reserved_3                       : Byte;
      Content_Protection_Application   : Boolean;
      File_Format_Group                : Boolean;
      Copy_Flag                        : Boolean;
      Permanent_Write_Protection       : Boolean;
      Temporary_Write_Protection       : Boolean;
      File_Format                      : Byte;
      ECC_Code                         : Byte;
      CSD_CRC                          : Byte;
      Reserved_4                       : Byte; --  Always 1
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
      Manufacturer_ID       : Byte;
      OEM_Application_ID    : String (1 .. 2);
      Product_Name          : String (1 .. 5);
      Product_Revision      : Card_Revision;
      Product_Serial_Number : Word;
      Reserved_1            : Byte;
      Manufacturing_Date    : Manufacturing_Date_Type;
      CID_CRC               : Byte;
      Reserved_2            : Byte; --  Always 1
   end record;

   type Card_Information is record
      SD_CSD          : Card_Specific_Data_Register;
      SD_CID          : Card_Identification_Data_Register;
      Card_Capacity   : Unsigned_64;
      Card_Block_Size : Unsigned_32;
      RCA             : Short; --  SD relative card address
      Card_Type       : Supported_SD_Memory_Cards :=
                          STD_Capacity_SD_Card_V1_1;
   end record;

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

   function Initialize
     (Controller : in out SDMMC_Controller;
      Info       : out Card_Information) return SD_Error;

   function Initialized (Controller : SDMMC_Controller) return Boolean;

   function Get_Card_Type
     (Controller : SDMMC_Controller) return Supported_SD_Memory_Cards
     with Pre => Initialized (Controller);

   function Configure_Wide_Bus_Mode
     (Controller : in out SDMMC_Controller;
      Wide_Mode  : Wide_Bus_Mode) return SD_Error;

   type SD_Data is array (Unsigned_32 range <>) of Byte
   with Pack;

   function Read_Blocks
     (Controller : in out SDMMC_Controller;
      Addr       : Unsigned_64;
      Data       : out SD_Data) return SD_Error
     with Pre => Data'Length mod 512 = 0;

   function Read_Blocks_DMA
     (Controller : in out SDMMC_Controller;
      Addr       : Unsigned_64;
      DMA        : STM32.DMA.DMA_Controller;
      Stream     : STM32.DMA.DMA_Stream_Selector;
      Data       : out SD_Data) return SD_Error;

   function Stop_Transfer
     (Controller : in out SDMMC_Controller) return SD_Error;

   function Get_FIFO_Address
     (Controller : SDMMC_Controller) return System.Address;

   function Get_Transfer_Status
     (Controller : in out SDMMC_Controller) return SD_Error;

   type SDMMC_Flags is
     (Data_End,
      Data_CRC_Fail,
      Data_Timeout,
      RX_Overrun,
      TX_Underrun,
      RX_Active);

   subtype SDMMC_Clearable_Flags is SDMMC_Flags range Data_End .. TX_Underrun;

   function Get_Flag
     (Controller : SDMMC_Controller;
      Flag       : SDMMC_Flags) return Boolean;

   procedure Clear_Flag
     (Controller : in out SDMMC_Controller;
      Flag       : SDMMC_Clearable_Flags);

   procedure Clear_Static_Flags (Controller : in out SDMMC_Controller);

   type SDMMC_Interrupts is
     (Data_End_Interrupt,
      Data_CRC_Fail_Interrupt,
      Data_Timeout_Interrupt,
      TX_FIFO_Empty_Interrupt,
      RX_FIFO_Full_Interrupt,
      TX_Underrun_Interrupt,
      RX_Overrun_Interrupt);

   procedure Enable_Interrupt
     (Controller : in out SDMMC_Controller;
      Interrupt  : SDMMC_Interrupts);

   procedure Disable_Interrupt
     (Controller : in out SDMMC_Controller;
      Interrupt  : SDMMC_Interrupts);

   type SDMMC_Operation is
     (No_Operation,
      Read_Single_Block_Operation,
      Read_Multiple_Blocks_Operation,
      Write_Single_Block_Operation,
      Write_Multiple_Blocks_Operation);

   function Last_Operation
     (Controller : SDMMC_Controller) return SDMMC_Operation;

private

   type SDMMC_Command is new Byte;

   --  Resets the SD memory card
   Go_Idle_State        : constant SDMMC_Command := 0;

   --  Sends host capacity support information and activates the card's
   --  initialization process
   Send_Op_Cond         : constant SDMMC_Command := 1;

   --  Asks any card connected to the host to send the CID numbers on the
   --  CMD line.
   All_Send_CID         : constant SDMMC_Command := 2;

   --  Asks the card to publish a new relative address (RCA).
   Set_Rel_Addr         : constant SDMMC_Command := 3;

   --  Programs the DSR of all cards.
   Set_DSR              : constant SDMMC_Command := 4;

   --  Sends host capacity support information (HCS) and asks the accessed
   --  card to send its operating condition register (OCR) content in the
   --  response on the CMD line.
   SDMMC_Send_Op_Cond   : constant SDMMC_Command := 5;

   --  Checks switchable function (mode 0) and switch card function (mode
   --  1).
   HS_Switch            : constant SDMMC_Command := 6;

   --  Selects the card by its own relative address and gets deselected by
   --  any other address
   Sel_Desel_Card       : constant SDMMC_Command := 7;

   --  Sends SD Memory Card interface condition
   HS_Send_Ext_CSD      : constant SDMMC_Command := 8;

   --  Addressed card sends its card specific data
   Send_CSD             : constant SDMMC_Command := 9;

   --  Addressed card sends its card identification (CID) on the CMD line.
   Send_CID             : constant SDMMC_Command := 10;

   Read_Dat_Until_Stop  : constant SDMMC_Command := 11;
   Stop_Transmission    : constant SDMMC_Command := 12;
   Send_Status          : constant SDMMC_Command := 13;
   HS_Bustest_Read      : constant SDMMC_Command := 14;
   Go_Inactive_State    : constant SDMMC_Command := 15;
   Set_Blocklen         : constant SDMMC_Command := 16;
   Read_Single_Block    : constant SDMMC_Command := 17;
   Read_Multi_Block     : constant SDMMC_Command := 18;
   HS_Bustest_Write     : constant SDMMC_Command := 19;
   Write_Dat_Until_Stop : constant SDMMC_Command := 20;
   Set_Block_Count      : constant SDMMC_Command := 23; --  Only for MMC
   Write_Single_Block   : constant SDMMC_Command := 24;
   Write_Multi_Block    : constant SDMMC_Command := 25;
   Prog_CID             : constant SDMMC_Command := 26;
   Prog_CSD             : constant SDMMC_Command := 27;
   Set_Write_Prot       : constant SDMMC_Command := 28;
   Clr_Write_Prot       : constant SDMMC_Command := 29;
   Send_Write_Prot      : constant SDMMC_Command := 30;
   SD_Erase_Grp_Start   : constant SDMMC_Command := 32;
   SD_Erase_Grp_End     : constant SDMMC_Command := 33;
   Erase_Grp_Start      : constant SDMMC_Command := 35;
   Erase_Grp_End        : constant SDMMC_Command := 36;
   Erase                : constant SDMMC_Command := 38;
   Fast_IO              : constant SDMMC_Command := 39;
   Go_IRQ_State         : constant SDMMC_Command := 40;
   Lock_Unlock          : constant SDMMC_Command := 42;
   App_Cmd              : constant SDMMC_Command := 55;
   Gen_Cmd              : constant SDMMC_Command := 56;
   No_Cmd               : constant SDMMC_Command := 64;

   --  SD-Card speciric commands
   --  App_Cmd should be sent before sending these commands
   subtype SD_Specific_Command is SDMMC_Command;

   SD_App_Set_Buswidth                : constant SD_Specific_Command := 6;
   SD_App_Status                      : constant SD_Specific_Command := 13;
   SD_App_Secure_Read_Multi_Block     : constant SD_Specific_Command := 18;
   SD_App_Send_Num_Write_Blocks       : constant SD_Specific_Command := 22;
   SD_App_Set_Write_Block_Erase_Count : constant SD_Specific_Command := 23;
   SD_App_Secure_Write_Multi_Block    : constant SD_Specific_Command := 25;
   SD_App_Secure_Erase                : constant SD_Specific_Command := 38;
   SD_App_Op_Cond                     : constant SD_Specific_Command := 41;
   SD_App_Get_MKB                     : constant SD_Specific_Command := 43;
   SD_App_Get_MID                     : constant SD_Specific_Command := 44;
   SD_App_Set_CER_RN1                 : constant SD_Specific_Command := 45;
   SD_App_Get_CER_RN2                 : constant SD_Specific_Command := 46;
   SD_App_Set_CER_RES2                : constant SD_Specific_Command := 47;
   SD_App_Get_CER_RES1                : constant SD_Specific_Command := 48;
   SD_App_Change_Secure_Area          : constant SD_Specific_Command := 49;
   SD_App_Send_SCR                    : constant SD_Specific_Command := 51;

   type Card_Data_Table is array (0 .. 3) of Word;

   type SDMMC_Controller is record
      Periph    : access STM32_SVD.SDMMC.SDMMC1_Peripheral;
      CID       : Card_Data_Table;
      CSD       : Card_Data_Table;
      Card_Type : Supported_SD_Memory_Cards :=
                    STD_Capacity_SD_Card_V1_1;
      RCA       : Word;
      Operation : SDMMC_Operation := No_Operation;
   end record;

   function As_Controller
     (Periph : access STM32_SVD.SDMMC.SDMMC1_Peripheral)
      return SDMMC_Controller
   is (Periph, CID => (others => 0), CSD => (others => 0), others => <>);

   function Initialized (Controller : SDMMC_Controller) return Boolean
   is (Controller.CID /= (0, 0, 0, 0));

   function Get_Card_Type
     (Controller : SDMMC_Controller) return Supported_SD_Memory_Cards
   is (Controller.Card_Type);

   type Data_Direction is (Read, Write);

   function Get_FIFO_Address
     (Controller : SDMMC_Controller) return System.Address
   is (Controller.Periph.FIFO'Address);

   function Get_Flag
     (Controller : SDMMC_Controller;
      Flag       : SDMMC_Flags) return Boolean
   is (case Flag is
          when Data_End      => Controller.Periph.STA.DATAEND,
          when Data_CRC_Fail => Controller.Periph.STA.DCRCFAIL,
          when Data_Timeout  => Controller.Periph.STA.DTIMEOUT,
          when RX_Overrun    => Controller.Periph.STA.RXOVERR,
          when TX_Underrun   => Controller.Periph.STA.TXUNDERR,
          when RX_Active     => Controller.Periph.STA.RXACT);

   function Last_Operation
     (Controller : SDMMC_Controller) return SDMMC_Operation
   is (Controller.Operation);

end STM32.SDMMC;
