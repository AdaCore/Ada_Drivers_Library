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

   type Device_EP_Event is record
      Event : Device_EP_Event_Kind;
      Endpoint_Addr : Endpoint_Address;
   end record;

   type Device_State is record
      VBus_Flag : Boolean;
      Speed     : USB.Speed;
      Active    : Boolean;
   end record;

   type USB_Device_Controller is limited interface;

   procedure Initialize (This : in out USB_Device_Controller) is abstract;

   procedure Uninitialize (This : in out USB_Device_Controller) is abstract;

   procedure Connect (This : in out USB_Device_Controller) is abstract;

   procedure Disconnect (This : in out USB_Device_Controller) is abstract;

   function Get_State (This : in out USB_Device_Controller) return Device_State is abstract;

   procedure Remote_Wakeup (This : in out USB_Device_Controller) is abstract;

   procedure Set_Address (This : in out USB_Device_Controller;
                          Addr : Device_Address) is abstract;

   function Read_Setup_Packet (This : in out USB_Device_Controller)
                               return Byte_Array is abstract;

   procedure Configure_Endpoint (This            : in out USB_Device_Controller;
                                 EP_Addr         : Endpoint_Address;
                                 Max_Packet_Size : Short) is abstract;

   procedure Unconfigure_Endpoint (This    : in out USB_Device_Controller;
                                   EP_Addr : Endpoint_Address) is abstract;

   procedure Stall_Endpoint (This    : in out USB_Device_Controller;
                             EP_Addr : Endpoint_Address;
                             Stall   : Boolean := True) is abstract;

   procedure Transmit (This    : in out USB_Device_Controller;
                       EP_Addr : Endpoint_Address;
                       Data    : Byte_Array) is abstract
     with Pre'Class => EP_Addr.Direction = USB_EP_Dir_Out;

   procedure Receive (This    : in out USB_Device_Controller;
                      EP_Addr : Endpoint_Address;
                      Data    : out Byte_Array) is abstract
     with Pre'Class => EP_Addr.Direction = USB_EP_Dir_In;

   function Get_Frame_Number  (This : in out USB_Device_Controller)
                               return Short is abstract;

   procedure Signal_Event  (This : in out USB_Device_Controller;
                            Evt  : Device_Event_Kind) is abstract;

   procedure Signal_Event  (This : in out USB_Device_Controller;
                            Ep   : Endpoint_Address;
                            Evt  : Device_EP_Event_Kind) is abstract;

end HAL.USB.Device;
