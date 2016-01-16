with STM32_SVD.Interrupts;

separate (STM32.LTDC)
package body Init is

   pragma Warnings (Off, "* is not referenced");
   type LCD_Polarity is
     (Polarity_Active_Low,
      Polarity_Active_High) with Size => 1;
   type LCD_PC_Polarity is
     (Input_Pixel_Clock,
      Inverted_Input_Pixel_Clock) with Size => 1;
   pragma Warnings (On, "* is not referenced");

   function To_Bit is new Ada.Unchecked_Conversion (LCD_Polarity, Bit);
   function To_Bit is new Ada.Unchecked_Conversion (LCD_PC_Polarity, Bit);

   --  Extracted from STM32F429x.LTDC
   type Layer_Type is record
      LCR        : L1CR_Register; --  Layerx Control Register
      LWHPCR     : L1WHPCR_Register; --  Layerx Window Horizontal Position Configuration Register
      LWVPCR     : L1WVPCR_Register; --  Layerx Window Vertical Position Configuration Register
      LCKCR      : L1CKCR_Register; --  Layerx Color Keying Configuration Register
      LPFCR      : L1PFCR_Register; --  Layerx Pixel Format Configuration Register
      LCACR      : L1CACR_Register; --  Layerx Constant Alpha Configuration Register
      LDCCR      : L1DCCR_Register; --  Layerx Default Color Configuration Register
      LBFCR      : L1BFCR_Register; --  Layerx Blending Factors Configuration Register
      Reserved_0 : Word;
      Reserved_1 : Word;
      LCFBAR     : Word; --  Layerx Color Frame Buffer Address Register
      LCFBLR     : L1CFBLR_Register; --  Layerx Color Frame Buffer Length Register
      LCFBLNR    : L1CFBLNR_Register; --  Layerx ColorFrame Buffer Line Number Register
      Reserved_2 : Word;
      Reserved_3 : Word;
      Reserved_4 : Word;
      LCLUTWR    : L1CLUTWR_Register; --  Layerx CLUT Write Register
   end record with Volatile;

   for Layer_Type use record
      LCR        at  0 range 0 .. 31;
      LWHPCR     at  4 range 0 .. 31;
      LWVPCR     at  8 range 0 .. 31;
      LCKCR      at 12 range 0 .. 31;
      LPFCR      at 16 range 0 .. 31;
      LCACR      at 20 range 0 .. 31;
      LDCCR      at 24 range 0 .. 31;
      LBFCR      at 28 range 0 .. 31;
      Reserved_0 at 32 range 0 .. 31;
      Reserved_1 at 36 range 0 .. 31;
      LCFBAR     at 40 range 0 .. 31;
      LCFBLR     at 44 range 0 .. 31;
      LCFBLNR    at 48 range 0 .. 31;
      Reserved_2 at 52 range 0 .. 31;
      Reserved_3 at 56 range 0 .. 31;
      Reserved_4 at 60 range 0 .. 31;
      LCLUTWR    at 64 range 0 .. 31;
   end record;
   type Layer_Access is access all Layer_Type;

   G_Layer1_Reg : aliased Layer_Type
     with Import, Address => LTDC_Periph.L1CR'Address;
   G_Layer2_Reg : aliased Layer_Type
     with Import, Address => LTDC_Periph.L2CR'Address;

   protected Sync is
      --  A call to Prepare_Wait_VBR must be followed by a call to
      --  Wait. Wait until register reload.
      procedure Prepare_Wait_VBR;

      --  Wait for an interrupt.
      entry Wait;

      procedure Interrupt;
      pragma Attach_Handler
        (Interrupt, STM32_SVD.Interrupts.LCD_TFT_Interrupt);
   private
      Triggered : Boolean := False;
   end Sync;

   protected body Sync is
      ----------------------
      -- Prepare_Wait_VBR --
      ----------------------

      procedure Prepare_Wait_VBR is
      begin
         Triggered := False;
         LTDC_Periph.IER.RRIE := 1;
      end Prepare_Wait_VBR;

      ----------
      -- Wait --
      ----------

      entry Wait when Triggered is
      begin
         null;
      end Wait;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt
      is
      begin
         LTDC_Periph.IER :=
           (RRIE => 0, TERRIE => 0, FUIE => 0, LIE => 0, others => <>);
         LTDC_Periph.ICR.CRRIF := 1;
         Triggered := True;
      end Interrupt;
   end Sync;

   ---------------
   -- Get_Layer --
   ---------------

   function Get_Layer (Layer : LCD_Layer) return Layer_Access is
   begin
      if Layer = Layer1 then
         return G_Layer1_Reg'Access;
      else
         return G_Layer2_Reg'Access;
      end if;
   end Get_Layer;

   --------------------
   -- Set_Layer_CFBA --
   --------------------

   procedure Set_Layer_CFBA (Layer : LCD_Layer;
                             FBA   : Frame_Buffer_Access)
   is
      L : constant Layer_Access := Get_Layer (Layer);
      function To_Word is new Ada.Unchecked_Conversion
        (Frame_Buffer_Access, Word);
   begin
         L.LCFBAR := To_Word (FBA);
   end Set_Layer_CFBA;

   --------------------
   -- Get_Layer_CFBA --
   --------------------

   function Get_Layer_CFBA
     (Layer : LCD_Layer) return Frame_Buffer_Access
   is
      L : constant Layer_Access := Get_Layer (Layer);
      function To_FBA is new Ada.Unchecked_Conversion
        (Word, Frame_Buffer_Access);
   begin
      return To_FBA (L.LCFBAR);
   end Get_Layer_CFBA;

   ---------------------
   -- Set_Layer_State --
   ---------------------

   procedure Set_Layer_State
     (Layer : LCD_Layer;
      State : Boolean)
   is
      L : constant Layer_Access := Get_Layer (Layer);
   begin
      if State and then Frame_Buffer_Array (Layer) = Null_Address then
         Frame_Buffer_Array (Layer) := STM32.SDRAM.Reserve
           (Word (LCD_Width * LCD_Height * Pixel_Size (Current_Pixel_Fmt)));
         Set_Layer_CFBA (Layer, Frame_Buffer_Array (Layer));
         Reload_Config (Immediate => True);
      end if;

      L.LCR.LEN := (if state then 1 else 0);
   end Set_Layer_State;

   -------------------
   -- Reload_Config --
   -------------------

   procedure Reload_Config (Immediate : Boolean) is
   begin
      if Immediate then
         LTDC_Periph.SRCR.IMR := 1;
         loop
            exit when LTDC_Periph.SRCR.IMR = 0;
         end loop;
      else
         LTDC_Periph.SRCR.VBR := 1;
         if True then
            Sync.Prepare_Wait_VBR;
            Sync.Wait;
         else
            --  Polling
            loop
               exit when LTDC_Periph.SRCR.VBR = 0;
            end loop;
         end if;
      end if;
   end Reload_Config;

   ---------------
   -- LTDC_Init --
   ---------------

   procedure LTDC_Init
   is
      DivR_Val : PLLSAI_DivR;

   begin
      RCC_Periph.APB2ENR.LTDCEN := 1;

      LTDC_Periph.GCR.VSPOL := To_Bit (Polarity_Active_Low);
      LTDC_Periph.GCR.HSPOL := To_Bit (Polarity_Active_Low);
      LTDC_Periph.GCR.DEPOL := To_Bit (Polarity_Active_Low);
      LTDC_Periph.GCR.PCPOL := To_Bit (Input_Pixel_Clock);

      if DivR = 2 then
         DivR_Val := PLLSAI_DIV2;
      elsif DivR = 4 then
         DivR_Val := PLLSAI_DIV4;
      elsif DivR = 8 then
         DivR_Val := PLLSAI_DIV8;
      elsif DivR = 16 then
         DivR_Val := PLLSAI_DIV16;
      else
         raise Constraint_Error with "Invalid DivR value: 2, 4, 8, 16 allowed";
      end if;

      Disable_PLLSAI;
      Set_PLLSAI_Factors
        (LCD  => PLLSAI_R,
         VCO  => PLLSAI_N,
         DivR => DivR_Val);
      Enable_PLLSAI;

      --  Synchronization size
      LTDC_Periph.SSCR :=
        (HSW => SSCR_HSW_Field (LCD_HSync - 1),
         VSH => SSCR_VSH_Field (LCD_VSync - 1),
         others => <>);

      --  Accumulated Back Porch
      LTDC_Periph.BPCR :=
        (AHBP => BPCR_AHBP_Field (LCD_HSync + LCD_HBP - 1),
         AVBP => BPCR_AVBP_Field (LCD_VSync + LCD_VBP - 1),
         others => <>);

      --  Accumulated Active Width/Height
      LTDC_Periph.AWCR :=
        (AAW => AWCR_AAW_Field (LCD_HSync + LCD_HBP + LCD_Width - 1),
         AAH => AWCR_AAH_FIeld (LCD_VSync + LCD_VBP + LCD_Height - 1),
         others => <>);

      --  VTotal Width/Height
      LTDC_Periph.TWCR :=
        (TOTALW =>
           TWCR_TOTALW_Field (LCD_HSync + LCD_HBP + LCD_Width + LCD_HFP - 1),
         TOTALH =>
           TWCR_TOTALH_Field (LCD_VSync + LCD_VBP + LCD_Height + LCD_VFP - 1),
         others => <>);

      --  Background color to black
      LTDC_Periph.BCCR.BC := 0;

      Reload_Config (True);

      LTDC_Periph.GCR.LTDCEN := 1;
   end LTDC_Init;

   -----------------
   -- Layers_Init --
   -----------------

   procedure Layer_Init
     (Layer    : LCD_Layer;
      Config   : Pixel_Format;
      BF1, BF2 : UInt3)
   is
      L    : constant Layer_Access := Get_Layer (Layer);
      CFBL : L1CFBLR_Register := L.LCFBLR;
   begin
      --  Horizontal start and stop = sync + Back Porch
      L.LWHPCR :=
        (WHSTPOS => UInt12 (LCD_HSync + LCD_HBP),
         WHSPPOS => UInt12 (LCD_HSync + LCD_HBP + LCD_Width - 1),
         others  => <>);

      --  Vertical start and stop
      L.LWVPCR :=
        (WVSTPOS => UInt11 (LCD_VSync + LCD_VBP),
         WVSPPOS => UInt11 (LCD_VSync + LCD_VBP + LCD_Height - 1),
         others  => <>);

      L.LPFCR.PF := Pixel_Format'Enum_Rep (Config);

      L.LCACR.CONSTA := 255;

      L.LDCCR := (others => 0);

      L.LBFCR.BF1 := BF1;
      L.LBFCR.BF2 := BF2;

      CFBL.CFBLL := UInt13 (LCD_Width * Pixel_Size (Config)) + 3;
      CFBL.CFBP := UInt13 (LCD_Width * Pixel_Size (Config));
      L.LCFBLR := CFBL;

      L.LCFBLNR.CFBLNBR := UInt11 (LCD_Height);

      Set_Layer_CFBA (Layer, Frame_Buffer_Array (Layer));

      Reload_Config (True);
   end Layer_Init;

end Init;