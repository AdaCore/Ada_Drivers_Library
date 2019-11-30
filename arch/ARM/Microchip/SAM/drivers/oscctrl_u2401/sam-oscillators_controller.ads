with HAL;

package SAM.Oscillators_Controller is

   type DFLL_Operating_Mode is (Open_Loop_Mode, Closed_Loop_Mode);

   procedure Configure_DFLL
     (On_Demand_Control     : Boolean;
      Run_On_Standby        : Boolean;
      USB_Clock_Recovery    : Boolean;
      Wait_Lock             : Boolean;
      Bypass_Coarse_Lock    : Boolean;
      Quick_Lock_Disable    : Boolean;
      Chill_Cycle_Disable   : Boolean;
      Lose_Lock_After_Wake  : Boolean;
      Stable_DFLL_Frequency : Boolean;
      Operating_Mode        : DFLL_Operating_Mode;
      Coarse_Maximum_Step   : HAL.UInt6;
      Fine_Maximum_Step     : HAL.UInt8;
      Multiply_Factor       : HAL.UInt16);
   --  See the reference manual of your device for more information on the
   --  configuration of DFLL.

   type DPLL_Reference_Clock is (GCLK_Ref, XOSC32K_Ref, XOSC0_Ref, XOSC1_Ref);

   type DPLL_Lock_Time is
     (
      Default,    -- No time-out. Automatic lock
      Lock_800Us, -- Time-out if no lock within 800us
      Lock_900Us, -- Time-out if no lock within 900us
      Lock_1Ms,   -- Time-out if no lock within 1ms
      Lock_1P1Ms) -- Time-out if no lock within 1.1ms
     with Size => 3;

   procedure Configure_DPLL
     (DPLL                    : HAL.Bit;
      On_Demand_Control       : Boolean;
      Run_On_Standby          : Boolean;
      Loop_Divider_Fractional : HAL.UInt5;
      Loop_Divider_Integer    : HAL.UInt13;
      Clock_Divider           : HAL.UInt11;
      DCO_Filter_Enable       : Boolean;
      Sigma_Delta_DCO_Filter  : HAL.UInt3;
      Lock_Bypass             : Boolean;
      Lock_Time               : DPLL_Lock_Time;
      Reference_Clock         : DPLL_Reference_Clock;
      Wakeup_Fast             : Boolean;
      Prop_Integral_Filter    : HAL.UInt4);
   --  See the reference manual of your device for more information on the
   --  configuration of DPLLs.

private
   for DPLL_Lock_Time use
     (Default    => 0,
      Lock_800Us => 4,
      Lock_900Us => 5,
      Lock_1Ms   => 6,
      Lock_1P1Ms => 7);
end SAM.Oscillators_Controller;
