------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------
with STM32.GPIO;
with STM32.Device;
with STM32_SVD.RCC;
with STM32_SVD.SYSCFG;
with STM32_SVD.Ethernet; use STM32_SVD.Ethernet;
with STM32.SDRAM;
with Ada.Real_Time;
with Ada.Interrupts.Names;
with Ada.Unchecked_Conversion;

package body STM32.Eth is

   type Rx_Desc_Range is mod 16;
   type Rx_Desc_Array is array (Rx_Desc_Range) of Rx_Desc_Type;
   type Rx_Desc_Arr_Ptr is access Rx_Desc_Array;

   type Rx_Buffer is array (Natural range 0 .. 1023) of Unsigned_8;
   type Rx_Buffer_Array is array (Rx_Desc_Range) of Rx_Buffer;
   type Rx_Buffer_Arr_Ptr is access Rx_Buffer_Array;

   Rx_Descs : Rx_Desc_Arr_Ptr;
   Rx_Buffs : Rx_Buffer_Arr_Ptr;

   procedure Init_Rx_Desc (I : Rx_Desc_Range);

   ---------------------
   -- Initialize_RMII --
   ---------------------

   procedure Initialize_RMII
   is
      use STM32.GPIO;
      use STM32.Device;
      use STM32_SVD.RCC;
   begin
      --  Enable GPIO clocks

      Enable_Clock (GPIO_A);
      Enable_Clock (GPIO_C);
      Enable_Clock (GPIO_G);

      --  Enable SYSCFG clock
      RCC_Periph.APB2ENR.SYSCFGEN := True;

      --  Select RMII (before enabling the clocks)
      STM32_SVD.SYSCFG.SYSCFG_Periph.PMC.MII_RMII_SEL := True;

      Configure_Alternate_Function (PA1,  GPIO_AF_ETH_11); -- RMII_REF_CLK
      Configure_Alternate_Function (PA2,  GPIO_AF_ETH_11); -- RMII_MDIO
      Configure_Alternate_Function (PA7,  GPIO_AF_ETH_11); -- RMII_CRS_DV
      Configure_Alternate_Function (PC1,  GPIO_AF_ETH_11); -- RMII_MDC
      Configure_Alternate_Function (PC4,  GPIO_AF_ETH_11); -- RMII_RXD0
      Configure_Alternate_Function (PC5,  GPIO_AF_ETH_11); -- RMII_RXD1
      Configure_Alternate_Function (PG2,  GPIO_AF_ETH_11); -- RMII_RXER
      Configure_Alternate_Function (PG11, GPIO_AF_ETH_11); -- RMII_TX_EN
      Configure_Alternate_Function (PG13, GPIO_AF_ETH_11); -- RMII_TXD0
      Configure_Alternate_Function (PG14, GPIO_AF_ETH_11); -- RMII_TXD1
      Configure_IO (PA1, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PA2, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PA7, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PC1, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PC4, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PC5, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PG2, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PG11, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PG13, (Mode_AF, Push_Pull, Speed_100MHz, Floating));
      Configure_IO (PG14, (Mode_AF, Push_Pull, Speed_100MHz, Floating));

      --  Enable clocks
      RCC_Periph.AHB1ENR.ETHMACEN := True;
      RCC_Periph.AHB1ENR.ETHMACTXEN := True;
      RCC_Periph.AHB1ENR.ETHMACRXEN := True;
      RCC_Periph.AHB1ENR.ETHMACPTPEN := True;

      --  Reset
      RCC_Periph.AHB1RSTR.ETHMACRST := True;
      RCC_Periph.AHB1RSTR.ETHMACRST := False;

      --  Software reset
      Ethernet_DMA_Periph.DMABMR.SR := True;
      while Ethernet_DMA_Periph.DMABMR.SR loop
         null;
      end loop;
   end Initialize_RMII;

   --------------
   -- Read_MMI --
   --------------

   procedure Read_MMI (Reg : UInt5; Val : out UInt16)
   is
      use Ada.Real_Time;
      Pa : constant UInt5 := 0;
      Cr : UInt3;
   begin
      case STM32.Device.System_Clock_Frequencies.HCLK is
         when 20e6 .. 35e6 - 1   => Cr := 2#010#;
         when 35e6 .. 60e6 - 1   => Cr := 2#011#;
         when 60e6 .. 100e6 - 1  => Cr := 2#000#;
         when 100e6 .. 150e6 - 1 => Cr := 2#001#;
         when 150e6 .. 216e6     => Cr := 2#100#;
         when others => raise Constraint_Error;
      end case;

      Ethernet_MAC_Periph.MACMIIAR :=
        (PA => Pa,
         MR => Reg,
         CR => Cr,
         MW => False,
         MB => True,
         others => <>);
      loop
         exit when not Ethernet_MAC_Periph.MACMIIAR.MB;
         delay until Clock + Milliseconds (1);
      end loop;

      Val := Ethernet_MAC_Periph.MACMIIDR.TD;
   end Read_MMI;

   ------------------
   -- Init_Rx_Desc --
   ------------------

   procedure Init_Rx_Desc (I : Rx_Desc_Range)
   is
      function W is new Ada.Unchecked_Conversion
        (Address, UInt32);
      Last : constant Boolean := I = Rx_Desc_Range'Last;
   begin
      Rx_Descs (I) :=
        (Rdes0 => (Own => 1, others => <>),
         Rdes1 => (Dic => 0, Rbs2 => 0,
                   Rer => (if Last then 1 else 0),
                   Rch => 1, Rbs => Rx_Buffer'Length,
                   others => <>),
         Rdes2 => W (Rx_Buffs (I)'Address),
         Rdes3 => W (Rx_Descs (I + 1)'Address));
   end Init_Rx_Desc;

   procedure Init_Mac is
      function To_Rx_Desc_Arr_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Rx_Desc_Arr_Ptr);
      function To_Rx_Buffer_Arr_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Rx_Buffer_Arr_Ptr);
      function W is new Ada.Unchecked_Conversion
        (Address, UInt32);
      Desc_Addr : Address;
   begin
      --  FIXME: check speed, full duplex
      Ethernet_MAC_Periph.MACCR :=
        (CSTF => True,
         WD   => False,
         JD   => False,
         IFG  => 2#100#,
         CSD  => False,
         FES  => True,
         ROD  => True,
         LM   => False,
         DM   => True,
         IPCO => False,
         RD   => False,
         APCS => True,
         BL   => 2#10#,
         DC   => True,
         TE   => False,
         RE   => False,
         others => <>);
      Ethernet_MAC_Periph.MACFFR :=
        (RA => True, others => <>);
      Ethernet_MAC_Periph.MACHTHR := 0;
      Ethernet_MAC_Periph.MACHTLR := 0;
      Ethernet_MAC_Periph.MACFCR :=
        (PT   => 0,
         ZQPD => False,
         PLT  => 0,
         UPFD => False,
         RFCE => True,
         TFCE => True,
         FCB  => False,
         others => <>);
      Ethernet_MAC_Periph.MACVLANTR :=
        (VLANTC => False,
         VLANTI => 0,
         others => <>);
      Ethernet_MAC_Periph.MACPMTCSR :=
        (WFFRPR => False,
         GU     => False,
         WFR    => False,
         MPR    => False,
         WFE    => False,
         MPE    => False,
         PD     => False,
         others => <>);

      Desc_Addr := STM32.SDRAM.Reserve (Amount => Rx_Desc_Array'Size / 8);
      Rx_Descs := To_Rx_Desc_Arr_Ptr (Desc_Addr);
      Ethernet_DMA_Periph.DMARDLAR := W (Desc_Addr);

      Desc_Addr := STM32.SDRAM.Reserve (Amount => Rx_Buffer_Array'Size / 8);
      Rx_Buffs := To_Rx_Buffer_Arr_Ptr (Desc_Addr);

      for I in Rx_Desc_Range loop
         Init_Rx_Desc (I);
      end loop;

      Ethernet_DMA_Periph.DMABMR :=
        (SR   => False,
         DA   => False,
         DSL  => 0,
         EDFE => False,
         PBL  => 4,
         RTPR => 0,
         FB   => True,
         RDP  => 4,
         USP  => True,
         FPM  => False,
         AAB  => False,
         MB   => False,
         others => <>);
   end Init_Mac;

   protected Sync is
      entry Wait_Packet;
      procedure Start_Rx;

      procedure Interrupt;
      pragma Attach_Handler (Interrupt, Ada.Interrupts.Names.ETH_Interrupt);
   private
      Rx_Pkt : Boolean := False;

      --  First descriptor of the last received packet.
      Last_Rx : Rx_Desc_Range;

      --  Descriptor for the next packet to be received.
      Next_Rx : Rx_Desc_Range;
   end Sync;

   protected body Sync is
      procedure Start_Rx is
      begin
         --  Make as if last packet received was in last descriptor.
         Last_Rx := Rx_Desc_Range'Last;
         Next_Rx := Rx_Desc_Range'First;
         Rx_Descs (Last_Rx).Rdes0.Own := 0;
         Rx_Descs (Last_Rx).Rdes0.Ls := 1;

         --  Assume the RxDMA is ok.
         Ethernet_MAC_Periph.MACCR.RE := True;
         Ethernet_DMA_Periph.DMAIER.RIE := True;
         Ethernet_DMA_Periph.DMAIER.NISE := True;
         Ethernet_DMA_Periph.DMAOMR.SR := True;
         Ethernet_DMA_Periph.DMARPDR := 1;
      end Start_Rx;

      entry Wait_Packet when Rx_Pkt is
      begin
         --  Set OWN to last rx descs.
         loop
            --  The previous packet is owned by the software.
            pragma Assert (Rx_Descs (Last_Rx).Rdes0.Own = 0);

            --  Refill desc.
            Init_Rx_Desc (Last_Rx);
            Last_Rx := Last_Rx + 1;
            exit when Last_Rx = Next_Rx;
         end loop;

         --  As we got an interrupt, the next descriptor should be for us.
         pragma Assert (Rx_Descs (Last_Rx).Rdes0.Own = 0);
         Last_Rx := Next_Rx;

         --  Find Next Rx.
         loop
            exit when Rx_Descs (Next_Rx).Rdes0.Ls = 1;
            Next_Rx := Next_Rx + 1;
         end loop;
         Next_Rx := Next_Rx + 1;

         if Rx_Descs (Next_Rx).Rdes0.Own = 1 then
            --  Have to wait if no packets after the current one.
            Rx_Pkt := False;
         end if;
      end Wait_Packet;

      procedure Interrupt is
      begin
         Ethernet_DMA_Periph.DMASR.RS := True;
         Rx_Pkt := True;
      end Interrupt;
   end Sync;

   procedure Start_Rx is
   begin
      Sync.Start_Rx;
   end Start_Rx;

   procedure Wait_Packet is
   begin
      Sync.Wait_Packet;
   end Wait_Packet;
end STM32.Eth;
