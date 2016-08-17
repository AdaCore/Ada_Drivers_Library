package HAL.UART is

   type UART_Status is
     (Ok,
      Err_Error,
      Err_Timeout,
      Busy);

   type UART_Data_Size is
     (Data_Size_8b,
      Data_Size_9b);

   type UART_Data_8b is array (Natural range <>) of Byte;

   type UART_Data_9b is array (Natural range <>) of UInt9;

   type UART_Port is limited interface;

   type UART_Port_Ref is access all UART_Port'Class;

   function Data_Size (Port : UART_Port) return UART_Data_Size is abstract;

   procedure Transmit
     (This    : in out UART_Port;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_8b;

   procedure Transmit
     (This    : in out UART_Port;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_9b;

   procedure Receive
     (This    : in out UART_Port;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_8b;

   procedure Receive
     (This    : in out UART_Port;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_9b;

end HAL.UART;
