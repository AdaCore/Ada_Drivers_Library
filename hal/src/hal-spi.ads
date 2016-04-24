package HAL.SPI is
   type SPI_Status is
     (Ok,
      Err_Error,
      Err_Timeout,
      Busy);

   type SPI_Data_Size is
     (Data_Size_8b,
      Data_Size_16b);

   type SPI_Data_8b is array (Natural range <>) of Byte;
   type SPI_Data_16b is array (Natural range <>) of Short;

   type SPI_Port is limited interface;
   type SPI_Port_Ref is not null access all SPI_Port'Class;

   function Data_Size (Port : SPI_Port) return SPI_Data_Size is abstract;

   procedure Transmit
     (Port    : in out SPI_Port;
      Data    : SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (Port) = Data_Size_8b;

   procedure Transmit
     (Port    : in out SPI_Port;
      Data    : SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (Port) = Data_Size_16b;

   procedure Receive
     (Port    : in out SPI_Port;
      Data    : out SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (Port) = Data_Size_8b;

   procedure Receive
     (Port    : in out SPI_Port;
      Data    : out SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (Port) = Data_Size_16b;

end HAL.SPI;
