pragma Ada_2012;

with HAL;
with L6470h_Constants; use L6470h_Constants;
with HAL.SPI;

with STM32.Device;
with STM32.SPI;
with STM32.GPIO;
with STM32_SVD.SPI;
with Ada.Interrupts.Names;
with System;

package L6470h is
   pragma Elaborate_Body;
   
   function Get_Status return STATUS_Record;
   function Cmd_Ok return Boolean;
   function Is_Initialized return Boolean;
   function Get_KVAL_RUN return KVAL_RUN_Val_Type;
   procedure Set_Step (Ratio : L6470h_Constants.STEP_MODE_Record);
   procedure Set_KVAL_RUN (Ratio : L6470h_Constants.KVAL_RUN_Val_Type);
   procedure Set_STALL_TH (Ratio : L6470h_Constants.STALL_TH_Val_Type);
   procedure Reset;
   procedure Run (Spd : L6470h_Constants.SPEED_Val_Type)
     with Post => Cmd_Ok;
   procedure Reset_Pos;
   
private
   Initialized : Boolean := False;
   
   subtype Data_Length is Natural range 1 .. 3;
   type Data_Type is array (Data_Length range <>) of Hal.UInt8;

   procedure Set_Param (Reg : in L6470h_Constants.Register'Class;
                        Value : in Data_Type);
   procedure Get_Param (Reg : in L6470h_Constants.Register'Class;
                        Value: out Data_Type );

   SPI    : STM32.SPI.SPI_Port renames STM32.Device.SPI_2;
   SPI_AF : STM32.GPIO_Alternate_Function renames STM32.Device.GPIO_AF_5_SPI2;

   Chip_Select_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PB12;
   SPI_SCK_Pin     : STM32.GPIO.GPIO_Point renames STM32.Device.PB13;
   SPI_MISO_Pin    : STM32.GPIO.GPIO_Point renames STM32.Device.PB14;
   SPI_MOSI_Pin    : STM32.GPIO.GPIO_Point renames STM32.Device.PB15;
   
   Reset_Pin       : STM32.GPIO.GPIO_Point renames STM32.Device.PB11;
   
   EL_POS        : EL_POS_Register;
   MARK          : MARK_Register;
   SPEED         : SPEED_Register;
   ACC           : ACC_Register;
   DEC           : DEC_Register;
   MAX_SPEED     : MAX_SPEED_Register;
   MIN_SPEED     : MIN_SPEED_Register;
   FS_SPD        : FS_SPD_Register;
   KVAL_HOLD     : KVAL_HOLD_Register;
   KVAL_RUN      : KVAL_RUN_Register;
   KVAL_ACC      : KVAL_ACC_Register;
   KVAL_DEC      : KVAL_DEC_Register;
   INT_SPEED     : INT_SPEED_Register;
   ST_SLP        : ST_SLP_Register;
   FN_SLP_ACC    : FN_SLP_ACC_Register;
   FN_SLP_DEC    : FN_SLP_DEC_Register;
   K_THERM       : K_THERM_Register;
   ADC_OUT       : ADC_OUT_Register;
   OCD_TH        : OCD_TH_Register;
   STALL_TH      : STALL_TH_Register;
   STEP_MODE     : STEP_MODE_Register;
   ALARM_EN      : ALARM_EN_Register;
   CONFIG        : CONFIG_Register;
   STATUS        : STATUS_Register;
end L6470h;
