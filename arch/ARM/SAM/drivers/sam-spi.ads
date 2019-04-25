private with SAM_SVD.SPI;

with HAL.SPI;

package SAM.SPI is

   SPI_Timeout : constant := 15000;

   type Chip_Select is new Integer range 0 .. 3
     with Size => 4;

   type Chip_Select_List is array (Integer range <>) of Chip_Select;

   type Internal_SPI_Port is private;

   type SPI_Port (Periph : not null access Internal_SPI_Port;
                  CS     : Chip_Select) is
   limited new HAL.SPI.SPI_Port with private;

   Unknown_Port : exception;

   type Mode is
     (Master, Slave);

   type Delay_Type is new Natural range 0 .. 255;

   type Clock_Polarity is
     (Default_Hi, Default_Lo);

   type Clock_Phase is
     (Cap_Rising, Cap_Falling);

   --  SPI Chip Select behavior modes while transferring.
   type Cs_Behavior is
     (Keep_Low,      --  CS does not rise until a new transfer is requested on different chip select
      Rise_No_Tx,    --  CS rises if there is no more data to transfer
      Rise_Forced);  --  CS is de-asserted systematically during a time DLYBCS.

   type Divider_Type is new Natural range 0 .. 255;

   type Configuration is record
      Md                  : Mode := Master;
      Variable_Peripheral : Boolean := False;
      Select_Decode       : Boolean := False;
      Fault_Detect        : Boolean := False;
      Tx_On_Rx_Empty      : Boolean := False;
      Dly_Btw_Cs          : Delay_Type := 6;
      Clk_Pol             : Clock_Polarity := Default_Lo;
      Clk_Phase           : Clock_Phase := Cap_Rising;
      Cs_Beh              : Cs_Behavior := Rise_No_Tx;
      BPT                 : HAL.SPI.SPI_Data_Size := HAL.SPI.Data_Size_8b;
      Dly_SPCK            : Delay_Type := 0;
      Dly_Btw_Transfers   : Delay_Type := 0;
      Baud                : UInt32 := 1;
   end record;

   procedure Configure (This : in out SPI_Port;
                        Cfg  : Configuration);

   --  Enable SPI.
   procedure Enable (This : in out SPI_Port)
     with Inline;

   function Enabled (This : SPI_Port) return Boolean;

   --  Disable SPI
   procedure Disable (This : in out SPI_Port)
     with Inline;

   --  Reset SPI and set it to Slave mode
   procedure Reset (This : in out SPI_Port)
     with Inline;

   --  Set SPI to Master or Slave mode
   procedure Set_Mode (This : in out SPI_Port;
                       Md   : Mode);

   --  Get SPI work mode.
   function Get_Mode (This : SPI_Port) return Mode;

   --  Set Peripheral Select mode.
   --  Peripheral Chip Select can be controlled by SPI_TDR or SPI_MR.
   procedure Set_Variable_Peripheral_Select_Mode (This   : in out SPI_Port;
                                                  Enable : Boolean);

   --  Get Peripheral Select mode.
   function Get_Variable_Peripheral_Select_Mode (This : SPI_Port)
                                                 return Boolean;

   procedure Set_Peripheral_Select_Decode (This   : in out SPI_Port;
                                           Enable : Boolean);

   function Get_Peripheral_Select_Decode (This : SPI_Port)
                                          return Boolean;

   procedure Set_Fault_Detect_Mode (This   : in out SPI_Port;
                                    Enable : Boolean);

   function Get_Fault_Detect_Mode (This : SPI_Port) return Boolean;

   procedure Set_Tx_On_Rx_Empty (This   : in out SPI_Port;
                                 Enable : Boolean);

   function Get_Tx_In_Rx_Empty (This : SPI_Port) return Boolean;

   procedure Set_Delay_Between_Chip_Select (This  : in out SPI_Port;
                                            Value : Delay_Type);

   procedure Set_Loopback (This   : in out SPI_Port;
                           Enable : Boolean);

   procedure Set_Clock_Polarity (This : in out SPI_Port;
                                 Pol  : Clock_Polarity);

   procedure Set_Clock_Phase (This  : in out SPI_Port;
                              Phase : Clock_Phase);

   procedure Configure_Cs_Behavior (This     : in out SPI_Port;
                                    Behavior : Cs_Behavior);

   procedure Set_Bits_Per_Transfer (This : in out SPI_Port;
                                    Size : HAL.SPI.SPI_Data_Size);

   procedure Set_Transfer_Delay (This                    : in out SPI_Port;
                                 Delay_Before_SPCK       : Delay_Type;
                                 Delay_Between_Transfers : Delay_Type);

   procedure Set_Baudrate (This     : in out SPI_Port;
                           Bitrate  : UInt32)
     with Pre => Bitrate /= 0;

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout);

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout);

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout);

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout);

   procedure Enable_Clock (This : SPI_Port);
   procedure Disable_Clock (This : SPI_Port);

private

   use SAM_SVD;

   WPKEY : constant := 16#535049#;

   type Internal_SPI_Port is new SAM_SVD.SPI.SPI_Peripheral;

   type SPI_Port (Periph : not null access Internal_SPI_Port;
                  CS     : Chip_Select) is
   limited new HAL.SPI.SPI_Port with null record;

   --  Issue a LASTXFER command.
   --  The next transfer is the last transfer and after that CS is de-asserted.
   procedure Set_Last_Xfer (This : in out SPI_Port)
     with Inline;

   procedure Set_Peripheral_Chip_Select_Value (This  : in out SPI_Port;
                                               Value : UInt4);

   function Get_PCS (NPCS : Chip_Select) return UInt4 is
      ((2 ** Natural (NPCS)) - 1);

   function Is_Tx_Empty (This : SPI_Port) return Boolean is
     (This.Periph.SPI_SR.TXEMPTY);

   function Is_Tx_Ready (This : SPI_Port) return Boolean is
     (This.Periph.SPI_SR.TDRE);

   function Is_Rx_Full (This : SPI_Port) return Boolean is
     (This.Periph.SPI_SR.RDRF);

   function Is_Rx_Ready (This : SPI_Port) return Boolean is
     (Is_Tx_Empty (This) and then Is_Rx_Full (This));

   function Calculate_Baud_Divider (Baud : Natural;
                                    Mck  : Natural)
                                    return Divider_Type;

   function Div_Ceil (Lhs, Rhs : Natural) return Natural;

   function Poll_For_TX_Ready (This    : SPI_Port;
                               Timeout : Natural)
                               return Boolean;

   function Poll_For_RX_Ready (This    : SPI_Port;
                               Timeout : Natural)
                               return Boolean;

   procedure Unlock_Write_Protection (This : in out SPI_Port);
   procedure Lock_Write_Protection (This : in out SPI_Port);

end SAM.SPI;
