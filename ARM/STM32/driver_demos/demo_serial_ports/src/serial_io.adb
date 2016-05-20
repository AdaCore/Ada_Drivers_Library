with STM32.Device; use STM32.Device;

package body Serial_IO is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Peripheral (Device : access Peripheral_Descriptor) is
      Configuration : GPIO_Port_Configuration;
      Device_Pins   : constant GPIO_Points := Device.Rx_Pin & Device.Tx_Pin;
   begin
      Enable_Clock (Device_Pins);
      Enable_Clock (Device.Transceiver.all);

      Configuration.Mode        := Mode_AF;
      Configuration.Speed       := Speed_50MHz;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors   := Pull_Up;

      Configure_IO (Device_Pins, Configuration);

      Configure_Alternate_Function (Device_Pins, Device.Transceiver_AF);
   end Initialize_Peripheral;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Device    : access Peripheral_Descriptor;
      Baud_Rate : Baud_Rates;
      Parity    : Parities     := No_Parity;
      Data_Bits : Word_Lengths := Word_Length_8;
      End_Bits  : Stop_Bits    := Stopbits_1;
      Control   : Flow_Control := No_Flow_Control)
   is
   begin
      Disable (Device.Transceiver.all);

      Set_Baud_Rate    (Device.Transceiver.all, Baud_Rate);
      Set_Mode         (Device.Transceiver.all, Tx_Rx_Mode);
      Set_Stop_Bits    (Device.Transceiver.all, End_Bits);
      Set_Word_Length  (Device.Transceiver.all, Data_Bits);
      Set_Parity       (Device.Transceiver.all, Parity);
      Set_Flow_Control (Device.Transceiver.all, Control);

      Enable (Device.Transceiver.all);
   end Configure;

end Serial_IO;
