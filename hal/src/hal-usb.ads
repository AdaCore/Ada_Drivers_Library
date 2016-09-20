package HAL.USB is

   type Speed is (USB_Low_Speed,
                  USB_Full_Speed,
                  USB_High_Speed);

   type Endpoint_Kind is (USB_EP_Control,
                          USB_EP_Isochronous,
                          USB_EP_Bulk,
                          USB_EP_Interrupt)
     with Size => 2;

   type Endpoint_Direction is (USB_EP_Dir_Out, USB_EP_Dir_In)
     with Size => 1;

   subtype Endpoint_Number is UInt4;

   type Endpoint_Address is record
      Number    : Endpoint_Number;
      Direction : Endpoint_Direction;
   end record with Size => 8;

private
   for Endpoint_Kind use
     (USB_EP_Control     => 2#00#,
      USB_EP_Isochronous => 2#01#,
      USB_EP_Bulk        => 2#10#,
      USB_EP_Interrupt   => 2#11#);

   for Endpoint_Direction use
     (USB_EP_Dir_Out => 0,
      USB_EP_Dir_In  => 1);

   for Endpoint_Address use record
      Number    at 0 range 0 .. 3;
      Direction at 0 range 7 .. 7;
   end record;

end HAL.USB;
