package HAL.USB.Device is

   subtype Device_Address is Byte;
   
   type Device_Event_Kind is (USBD_Event_VBus_On,
                              USBD_Event_VBus_Off,
                              USBD_Event_Reset,
                              USBD_Event_High_Speed,
                              USBD_Event_Suspend,
                              USBD_Event_Resume);
   
   type Device_EP_Event_Kind is (USBD_EP_Event_Setup,
                                 USBD_EP_Event_Out,
                                 USBD_EP_Event_In);
   
   type USB_Device_Controller is limited interface;
   
   procedure Initialize (This : in out USB_Device_Controller) is abstract;
   
   procedure Remote_Wakeup (This : in out USB_Device_Controller) is abstract;
   
   procedure Set_Address (This : in out USB_Device_Controller;
                          Addr : Device_Address) is abstract;
   
   
   
end HAL.USB.Device;
