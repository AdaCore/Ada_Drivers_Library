------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with SAM_SVD.DMAC; use SAM_SVD.DMAC;

with HAL; use HAL;

package body SAM.DMAC is

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Descriptors : not null Descriptor_Section_Access;
      Write_Back  : not null Descriptor_Section_Access)
   is
   begin
      --  Enable all priority levels
      DMAC_Periph.CTRL.LVLEN.Arr := (others => True);

      DMAC_Periph.BASEADDR := Descriptors.all'Address;
      DMAC_Periph.WRBADDR  := Write_Back.all'Address;

      DMAC_Periph.CTRL.DMAENABLE := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      DMAC_Periph.CTRL.DMAENABLE := False;
   end Disable;

   -------------
   -- Pending --
   -------------

   function Pending (Id : Channel_Id) return Boolean
   is (DMAC_Periph.Channels (Integer (Id)).CHSTATUS.PEND);

   ----------
   -- Busy --
   ----------

   function Busy (Id : Channel_Id) return Boolean
   is (DMAC_Periph.Channels (Integer (Id)).CHSTATUS.BUSY);

   -----------------
   -- Fetch_Error --
   -----------------

   function Fetch_Error (Id : Channel_Id) return Boolean
   is (DMAC_Periph.Channels (Integer (Id)).CHSTATUS.FERR);

   ---------------
   -- CRC_Error --
   ---------------

   function CRC_Error (Id : Channel_Id) return Boolean
   is (DMAC_Periph.Channels (Integer (Id)).CHSTATUS.CRCERR);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Id          : Channel_Id;
      Trig_Src    : Trigger_Source;
      Trig_Action : Trigger_Action;
      Priority    : Priority_Level;
      Burst_Len   : Burst_Length;
      Threshold   : FIFO_Threshold_Kind)
   is
      Chan : Channel_Record renames DMAC_Periph.Channels (Integer (Id));
   begin
      Chan.CHCTRLA.BURSTLEN :=
        CHCTRLA0_BURSTLENSelect'Enum_Val (Burst_Len - 1);

      Chan.CHCTRLA.TRIGSRC := UInt7 (Trig_Src);

      Chan.CHCTRLA.TRIGACT :=
        CHCTRLA0_TRIGACTSelect'Enum_Val (Trigger_Action'Enum_Rep (Trig_Action));

      Chan.CHCTRLA.THRESHOLD :=
        CHCTRLA0_THRESHOLDSelect'Enum_Val (FIFO_Threshold_Kind'Enum_Rep (Threshold));

      Chan.CHPRILVL.PRILVL := CHPRILVL0_PRILVLSelect'Enum_Val (Priority);
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable (Id : Channel_Id) is
   begin
      DMAC_Periph.Channels (Integer (Id)).CHCTRLA.ENABLE := True;
   end Enable;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (Id : Channel_Id) is
   begin
      DMAC_Periph.Channels (Integer (Id)).CHCTRLB.CMD := Suspend;
   end Suspend;

   ------------
   -- Resume --
   ------------

   procedure Resume (Id : Channel_Id) is
   begin
      DMAC_Periph.Channels (Integer (Id)).CHCTRLB.CMD := Resume;
   end Resume;

   ----------------------
   -- Software_Trigger --
   ----------------------

   procedure Software_Trigger (Id : Channel_Id) is
   begin
      DMAC_Periph.SWTRIGCTRL.Val :=
        DMAC_Periph.SWTRIGCTRL.Val or Shift_Left (UInt32 (1), Integer (Id));
   end Software_Trigger;

   ------------
   -- Enable --
   ------------

   procedure Enable (Id : Channel_Id; Int : Interrupt_Kind) is
   begin
      case Int is
         when Suspend =>
            DMAC_Periph.Channels (Integer (Id)).CHINTENSET.SUSP := True;
         when Transfer_Complete =>
            DMAC_Periph.Channels (Integer (Id)).CHINTENSET.TCMPL := True;
         when Transfer_Error =>
            DMAC_Periph.Channels (Integer (Id)).CHINTENSET.TERR := True;
      end case;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Id : Channel_Id; Int : Interrupt_Kind) is
   begin
      case Int is
         when Suspend =>
            DMAC_Periph.Channels (Integer (Id)).CHINTENCLR.SUSP := True;
         when Transfer_Complete =>
            DMAC_Periph.Channels (Integer (Id)).CHINTENCLR.TCMPL := True;
         when Transfer_Error =>
            DMAC_Periph.Channels (Integer (Id)).CHINTENCLR.TERR := True;
      end case;
   end Disable;

   -----------
   -- Clear --
   -----------

   procedure Clear (Id : Channel_Id; Int : Interrupt_Kind) is
   begin
      case Int is
         when Suspend =>
            DMAC_Periph.Channels (Integer (Id)).CHINTFLAG :=
              (SUSP => True, TCMPL => False, TERR => False, others => 0);
         when Transfer_Complete =>
            DMAC_Periph.Channels (Integer (Id)).CHINTFLAG :=
              (SUSP => False, TCMPL => True, TERR => False, others => 0);
         when Transfer_Error =>
            DMAC_Periph.Channels (Integer (Id)).CHINTFLAG :=
              (SUSP => False, TCMPL => False, TERR => True, others => 0);
      end case;
   end Clear;

   ---------
   -- Set --
   ---------

   function Set (Id : Channel_Id; Int : Interrupt_Kind) return Boolean is
   begin
      case Int is
         when Suspend =>
            return DMAC_Periph.Channels (Integer (Id)).CHINTFLAG.SUSP;
         when Transfer_Complete =>
            return DMAC_Periph.Channels (Integer (Id)).CHINTFLAG.TCMPL;
         when Transfer_Error =>
            return DMAC_Periph.Channels (Integer (Id)).CHINTFLAG.TERR;
      end case;
   end Set;

end SAM.DMAC;
