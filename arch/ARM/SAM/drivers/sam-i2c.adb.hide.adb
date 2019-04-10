with SAM.Device;

with SAM_SVD.TWIHS; use SAM_SVD.TWIHS;

package body SAM.I2C is

   I2C_FAST_MODE_SPEED  : constant := 400_000;
   LOW_LEVEL_TIME_LIMIT : constant := 384_000;
   TWIHS_CLK_DIVIDER    : constant := 2;
   TWIHS_CLK_CALC_ARGU  : constant := 3;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This : in out I2C_Port;
      Conf : I2C_Configuration)
   is
      Status : SAM_SVD.TWIHS.TWIHS_TWIHS_SR_Register;

      Cl_Div, Ch_Div : UInt32;

      Ckdiv : UInt32 := 0;
   begin

      --  disable twihs interrupts
      This.Periph.TWIHS_IDR := (TXCOMP => True,
                                RXRDY  => True,
                                TXRDY  => True,
                                SVACC  => True,
                                GACC   => True,
                                OVRE   => True,
                                UNRE   => True,
                                NACK   => True,
                                ARBLST => True,
                                SCL_WS => True,
                                EOSACC => True,
                                MCACK  => True,
                                TOUT   => True,
                                PECERR => True,
                                SMBDAM => True,
                                SMBHHM => True,
                                others => <>);

      --  dummy status register read
      --  Status := This.Periph.TWIHS_SR;

      --  reset peripheral
      This.Reset;

      --  enable master mode
      This.Periph.TWIHS_CR.MSDIS := True;
      This.Periph.TWIHS_CR.SVDIS := True;

      This.Periph.TWIHS_CR.MSEN := True;

      --  set clock speed

      --  fast mode can only be used in slave mode
      if Conf.Clock_Speed > I2C_FAST_MODE_SPEED then
         raise Program_Error with "Unsupported Clock Speed with configuration";
      end if;

      if Conf.Clock_Speed > LOW_LEVEL_TIME_LIMIT then
         Cl_Div := SAM.Device.System_Clocks.MCK /
           (LOW_LEVEL_TIME_LIMIT * TWIHS_CLK_DIVIDER) - TWIHS_CLK_CALC_ARGU;
         Ch_Div := SAM.Device.System_Clocks.MCK /
           ((Conf.Clock_Speed +
            (Conf.Clock_Speed - LOW_LEVEL_TIME_LIMIT)) *
              TWIHS_CLK_DIVIDER) - TWIHS_CLK_CALC_ARGU;

         while Cl_Div > UInt32 (TWIHS_TWIHS_CWGR_CLDIV_Field'Last) and
           Ckdiv < UInt32 (TWIHS_TWIHS_CWGR_CKDIV_Field'Last) loop
            Ckdiv := Ckdiv + 1;
            Cl_Div := Cl_Div / TWIHS_CLK_DIVIDER;
         end loop;

         while Ch_Div > UInt32 (TWIHS_TWIHS_CWGR_CHDIV_Field'Last) and
           Ckdiv < UInt32 (TWIHS_TWIHS_CWGR_CKDIV_Field'Last) loop
            Ckdiv := Ckdiv + 1;
            Ch_Div := Ch_Div / TWIHS_CLK_DIVIDER;
         end loop;
      else
         Cl_Div := SAM.Device.System_Clocks.MCK /
           (Conf.Clock_Speed * TWIHS_CLK_DIVIDER) - TWIHS_CLK_CALC_ARGU;

         while Cl_Div > UInt32 (TWIHS_TWIHS_CWGR_CLDIV_Field'Last) and
           Ckdiv < UInt32 (TWIHS_TWIHS_CWGR_CKDIV_Field'Last) loop
            Ckdiv := Ckdiv + 1;
            Cl_Div := Cl_Div / TWIHS_CLK_DIVIDER;
         end loop;

         Ch_Div := Cl_Div;
      end if;

      This.Periph.TWIHS_CWGR.CLDIV := UInt8 (Cl_Div);
      This.Periph.TWIHS_CWGR.CHDIV := UInt8 (Ch_Div);
      This.Periph.TWIHS_CWGR.CKDIV := UInt3 (Ckdiv);

   end Configure;

   ---------------------
   -- Master_Transmit --
   ---------------------

   overriding procedure Master_Transmit
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Master_Transmit unimplemented");
      raise Program_Error with "Unimplemented procedure Master_Transmit";
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   overriding procedure Master_Receive
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : out HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      This.Periph.TWIHS_MMR.MREAD := 1;
      This.Periph.TWIHS_MMR.IADRSZ := 1;

      case This.Config.Addressing_Mode is
         when Addressing_Mode_7bit =>
            This.Periph.TWIHS_MMR.DADR := Addr;
         when Addressing_Mode_10bit =>
            This.Periph.TWIHS_MMR.DADR :=
      end case;
   end Master_Receive;

   ---------------
   -- Mem_Write --
   ---------------

   overriding procedure Mem_Write
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Mem_Write unimplemented");
      raise Program_Error with "Unimplemented procedure Mem_Write";
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   overriding procedure Mem_Read
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : out HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Mem_Read unimplemented");
      raise Program_Error with "Unimplemented procedure Mem_Read";
   end Mem_Read;

   procedure Reset (This : in out I2C_Port)
   is
   begin
      This.Periph.TWIHS_CR.SWRST := 1;
   end Reset;

end SAM.I2C;
