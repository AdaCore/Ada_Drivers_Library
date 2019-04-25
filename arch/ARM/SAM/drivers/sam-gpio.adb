with SAM; use SAM;
with SAM_SVD; use SAM_SVD;
with SAM_SVD.PIO; use SAM_SVD.PIO;

with SAM.PMC; use SAM.PMC;

package body SAM.GPIO is

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode
   is
   begin
      case Pin_IO_Mode (This) is
         when Mode_Out => return HAL.GPIO.Output;
         when Mode_In  => return HAL.GPIO.Input;
         when others   => return HAL.GPIO.Unknown;
      end case;
   end Mode;

   -----------------
   -- Pin_IO_Mode --
   -----------------

   function Pin_IO_Mode (This : GPIO_Point) return Pin_IO_Modes
   is
      Ret : Pin_IO_Modes;
   begin
      if This.Periph.PIO_PSR.Arr (This.Pin) then
         --  peripheral is inactive
         if This.Periph.PIO_OSR.Arr (This.Pin) then
            --  output
            Ret := Mode_Out;
         else
            --  input
            Ret := Mode_In;
         end if;
      else
         --  peripheral is active
         Ret := Mode_AF;
      end if;
      return Ret;
   end Pin_IO_Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   function Set_Mode (This : in out GPIO_Point;
                      Mode : HAL.GPIO.GPIO_Config_Mode)
                      return Boolean
   is
   begin
      case Mode is
         when HAL.GPIO.Output =>
            This.Periph.PIO_OER.Arr (This.Pin) := True;
         when HAL.GPIO.Input =>
            This.Periph.PIO_ODR.Arr (This.Pin) := True;
--         when others =>
--            return False;
      end case;
      return True;
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor
   is
   begin
      if This.Periph.PIO_PUSR.Arr (This.Pin) then
         return HAL.GPIO.Pull_Up;
      elsif This.Periph.PIO_PPDSR.Arr (This.Pin) then
         return HAL.GPIO.Pull_Down;
      else
         return HAL.GPIO.Floating;
      end if;
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   function Set_Pull_Resistor (This : in out GPIO_Point;
                               Pull : HAL.GPIO.GPIO_Pull_Resistor)
                               return Boolean
   is
   begin
      case Pull is
         when HAL.GPIO.Floating =>
            This.Periph.PIO_PUDR.Arr (This.Pin) := True;
            This.Periph.PIO_PPDDR.Arr (This.Pin) := True;
         when HAL.GPIO.Pull_Up =>
            This.Periph.PIO_PUER.Arr (This.Pin) := True;
         when HAL.GPIO.Pull_Down =>
            This.Periph.PIO_PPDER.Arr (This.Pin) := True;
      end case;
      return True;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean
   is
   begin
      return (This.Periph.PIO_PDSR.Arr (This.Pin));
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point)
   is
   begin
      This.Periph.PIO_SODR.Arr (This.Pin) := True;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point)
   is
   begin
      This.Periph.PIO_CODR.Arr (This.Pin) := True;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point)
   is
   begin
      if This.Periph.PIO_ODSR.Arr (This.Pin) then
         This.Clear;
      else
         This.Set;
      end if;
   end Toggle;

   -----------
   -- Drive --
   -----------

   procedure Drive (This : in out GPIO_Point; Condition : Boolean)
   is
   begin
      if Condition then
         This.Set;
      else
         This.Clear;
      end if;
   end Drive;

   ------------
   -- Locked --
   ------------

   function Locked (This : GPIO_Point) return Boolean
   is
   begin
      return (This.Periph.PIO_LOCKSR.Arr (This.Pin));
   end Locked;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO (This   : in out GPIO_Point;
                           Config : GPIO_Port_Configuration)
   is
   begin
      --  Disable interrupts
      This.Periph.PIO_IDR.Arr (This.Pin) := True;

      --  The PIO input logic requires the peripheral clock
      if Config.Mode = Mode_In then
         SAM.PMC.Configure_Peripheral (ID     => Group_ID (GPIO => This),
                                       Enable => True);
      end if;

      --  Enable pull-up resistors as requested
      --  Enable pull-down resistors as requested
      if not Set_Pull_Resistor (This => This,
                                Pull => Config.Resistors)
      then
         raise GPIO_Exception;
      end if;

      case Config.Mode is
         when Mode_In =>
            This.Periph.PIO_PER.Arr (This.Pin) := True;
            if not Set_Mode (This => This,
                             Mode => HAL.GPIO.Input)
            then
               raise GPIO_Exception;
            end if;

            case Config.Filter_Type is
               when None =>
                  This.Periph.PIO_IFSCDR.Arr (This.Pin) := True;
                  This.Periph.PIO_IFDR.Arr (This.Pin) := True;
               when Glitch =>
                  This.Periph.PIO_IFSCDR.Arr (This.Pin) := True;
                  This.Periph.PIO_IFER.Arr (This.Pin) := True;
               when Debounce =>
                  This.Periph.PIO_IFSCER.Arr (This.Pin) := True;
                  This.Periph.PIO_SCDR.DIV := Config.Filter_Time;
            end case;
            This.Periph.PIO_SCHMITT.Arr (This.Pin) := Config.Schmitt_Trigger;
         when Mode_Out =>
            This.Periph.PIO_PER.Arr (This.Pin) := True;
            if not Set_Mode (This => This,
                             Mode => HAL.GPIO.Output)
            then
               raise GPIO_Exception;
            end if;

            case Config.Output_Type is
               when Push_Pull =>
                  This.Periph.PIO_MDDR.Arr (This.Pin) := True;
               when Open_Drain =>
                  This.Periph.PIO_MDER.Arr (This.Pin) := True;
            end case;
         when Mode_AF =>
            case Config.AF_Output_Type is
               when Push_Pull =>
                  This.Periph.PIO_MDDR.Arr (This.Pin) := True;
               when Open_Drain =>
                  This.Periph.PIO_MDER.Arr (This.Pin) := True;
            end case;
            case Config.AF is
               when Periph_A =>
                  This.Periph.PIO_ABCDSR (0).Arr (This.Pin) := False;
                  This.Periph.PIO_ABCDSR (1).Arr (This.Pin) := False;
               when Periph_B =>
                  This.Periph.PIO_ABCDSR (0).Arr (This.Pin) := True;
                  This.Periph.PIO_ABCDSR (1).Arr (This.Pin) := False;
               when Periph_C =>
                  This.Periph.PIO_ABCDSR (0).Arr (This.Pin) := False;
                  This.Periph.PIO_ABCDSR (1).Arr (This.Pin) := True;
               when Periph_D =>
                  This.Periph.PIO_ABCDSR (0).Arr (This.Pin) := True;
                  This.Periph.PIO_ABCDSR (1).Arr (This.Pin) := True;
            end case;
            This.Periph.PIO_PDR.Arr (This.Pin) := True;
      end case;
   end Configure_IO;

   procedure Enable_Clock (Port : GPIO_Port)
   is
   begin
      SAM.PMC.Enable_Peripheral_Clock (ID => Group_ID (Port => Port));
   end Enable_Clock;

   procedure Enable_Clock (Point : GPIO_Point)
   is
   begin
      SAM.PMC.Enable_Peripheral_Clock (ID => Group_ID (GPIO => Point));
   end Enable_Clock;

   procedure Disable_Clock (Port : GPIO_Port)
   is
   begin
      SAM.PMC.Disable_Peripheral_Clock (ID => Group_ID (Port => Port));
   end Disable_Clock;

   procedure Disable_Clock (Point : GPIO_Point)
   is
   begin
      SAM.PMC.Disable_Peripheral_Clock (ID => Group_ID (GPIO => Point));
   end Disable_Clock;

   procedure Enable_Interrupt (This : GPIO_Point;
                               Trigger : Interrupt_Trigger_Type)
   is
   begin
      This.Periph.PIO_IER.Arr (This.Pin) := True;
      case Trigger is
         when Rising_Edge =>
            This.Periph.PIO_AIMER.Arr (This.Pin) := True;
            This.Periph.PIO_ESR.Arr (This.Pin) := True;
            This.Periph.PIO_REHLSR.Arr (This.Pin) := True;
         when Falling_Edge =>
            This.Periph.PIO_AIMER.Arr (This.Pin) := True;
            This.Periph.PIO_ESR.Arr (This.Pin) := True;
            This.Periph.PIO_FELLSR.Arr (This.Pin) := True;
         when Low =>
            This.Periph.PIO_AIMER.Arr (This.Pin) := True;
            This.Periph.PIO_LSR.Arr (This.Pin) := True;
            This.Periph.PIO_FELLSR.Arr (This.Pin) := True;
         when High =>
            This.Periph.PIO_AIMER.Arr (This.Pin) := True;
            This.Periph.PIO_LSR.Arr (This.Pin) := True;
            This.Periph.PIO_REHLSR.Arr (This.Pin) := True;
         when Any_Edge =>
            This.Periph.PIO_AIMDR.Arr (This.Pin) := True;
      end case;
   end Enable_Interrupt;

   procedure Disable_Interrupt (This : GPIO_Point)
   is
   begin
      This.Periph.PIO_IDR.Arr (This.Pin) := True;
   end Disable_Interrupt;

   function Read_Interrupt_Status (This : GPIO_Point) return Boolean
   is
   begin
      return This.Periph.PIO_ISR.Arr (This.Pin);
   end Read_Interrupt_Status;

end SAM.GPIO;
