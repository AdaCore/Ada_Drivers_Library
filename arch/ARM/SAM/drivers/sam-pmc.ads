package SAM.PMC is

   procedure Configure_Peripheral (ID     : Natural;
                                   Enable : Boolean);

   procedure Enable_Peripheral (ID : Natural);

   procedure Disable_Peripheral (ID : Natural);

   procedure Enable_Peripheral_Clock (ID : Natural);
   procedure Disable_Peripheral_Clock (ID : Natural);

   function Get_Peripheral_Clock (ID : Natural) return UInt32;

   type Peripheral_Clock_Array is array (0 .. 7) of UInt32;

   type System_Clock_Map is record
      MAINCK    : UInt32 := 0;  --  Main CLock
      SLCK      : UInt32 := 0;  --  Slow Clock
      PLLACK    : UInt32 := 0;  --  PLLA Clock
      UPLLCK    : UInt32 := 0;  --  UPLL Clock
      UPLLCKDIV : UInt32 := 0;  --  UPLL after divider Clock
      MCK       : UInt32 := 0;  --  Master Clock
      SysTick   : UInt32 := 0;  --  SysTick External Clock
      HCLK      : UInt32 := 0;  --  Processor Clock
      FCLK      : UInt32 := 0;  --  Free Running Clock
      USBCLK    : UInt32 := 0;  --  USB FS Clock
      USBHS     : UInt32 := 0;  --  USB HS Clock
      PCK       : Peripheral_Clock_Array := (others => 0);
   end record;

   function System_Clocks return System_Clock_Map;

private

   Slow_RC_Osc : constant := 32_768;
   Crystal_Osc_32k768 : constant := 32_768;

   Crystal_Osc_Main : constant := 12_000_000;

   UPLLCK_480MHz  : constant := 480_000_000;

   type GCLK_Source is
     (SLCK, MAINCK, UPLLCKDIV, PLLACK, MCK);

   type GCLK_Divider is range 1 .. 256;

   function Calc_PLLACK_Div return UInt32;
   function Calc_FCLK (In_Clk : UInt32) return UInt32;
   function Calc_USB_FS (Clocks : System_Clock_Map) return UInt32;
   function Calc_PCKx (X      : Natural;
                       Clocks : System_Clock_Map)
                       return UInt32;

--   procedure Set_GCLK (PID : UInt7;
--                       Src : GCLK_Source;
--                       Div : GCLK_Divider);

   function Get_GCLK (PID : UInt7) return UInt32;

end SAM.PMC;
