with HAL.USB; use HAL.USB;

package STM32.USB is

   type USB_Kind is (USB_Host, USB_Device);

   type USB_Phy_Interface is (USB_PHY_ULPI, USB_PHY_EMBEDDED);
   
   procedure Driver_Init (Kind              : USB_Kind;
                          Phy_Interface     : USB_Phy_Interface;
                          Speed             : HAL.USB.Speed;
                          SOF_Output_Enable : Boolean);
   
   procedure Driver_Reset;

end STM32.USB;
