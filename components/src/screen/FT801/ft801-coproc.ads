package FT801.Coproc is

   OPT_3D        : constant := 0;
   OPT_RGB565    : constant := 0;
   OPT_MONO      : constant := 1;
   OPT_NODL      : constant := 2;
   OPT_FLAT      : constant := 256;
   OPT_SIGNED    : constant := 256;
   OPT_CENTERX   : constant := 512;
   OPT_CENTERY   : constant := 1024;
   OPT_CENTER    : constant := 1536;
   OPT_RIGHTX    : constant := 2048;
   OPT_NOBACK    : constant := 4096;
   OPT_NOTICKS   : constant := 8192;
   OPT_NOHM      : constant := 16384;
   OPT_NOPOINTER : constant := 16384;
   OPT_NOSECS    : constant := 32768;
   OPT_NOHANDS   : constant := 49152;

   type Coproc_List is array (Natural range <>) of UInt32;

   procedure Send_Coproc_Cmds (This : in out FT801_Device;
                               Cmds : Coproc_List);

   CMD_DLSTART   : constant UInt32 := 16#FFFF_FF00#;
   CMD_DLSWAP    : constant UInt32 := 16#FFFF_FF01#;
   CMD_COLDSTART : constant UInt32 := 16#FFFF_FF32#;
   CMD_LOGO      : constant UInt32 := 16#FFFF_FF31#;

   type CMD_INTERRUPT (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Cmd : UInt32 := 16#FFFF_FF02#;
            MS  : UInt32;
         when True =>
            Arr : Coproc_List (1 .. 2);
      end case;
   end record
     with Unchecked_Union, Size => 64;

   for CMD_INTERRUPT use record
      Cmd at 0 range 0 .. 31;
      MS at 0 range 32 .. 63;
      Arr at 0 range 0 .. 63;
   end record;

   type CMD_MEMCPY (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Cmd : UInt32 := 16#FFFF_FF1D#;
            Dst : UInt32;
            Src : UInt32;
            Num : UInt32;
         when True =>
            Arr : Coproc_List (1 .. 4);
      end case;
   end record
     with Unchecked_Union, Size => 128;

   for CMD_MEMCPY use record
      Cmd at 0 range 0 .. 31;
      Dst at 4 range 0 .. 31;
      Src at 8 range 0 .. 31;
      Num at 12 range 0 .. 31;
      Arr at 0 range 0 .. 127;
   end record;

   type CMD_MEMZERO (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Cmd : UInt32 := 16#FFFF_FF1C#;
            Ptr : UInt32;
            Num : UInt32;         when True =>
            Arr : Coproc_List (1 .. 3);
      end case;
   end record
     with Unchecked_Union, Size => 96;

   for CMD_MEMZERO use record
      Cmd at 0 range 0 .. 31;
      Ptr at 4 range 0 .. 31;
      Num at 8 range 0 .. 31;
      Arr at 0 range 0 .. 95;
   end record;

   type CMD_MEMSET (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Cmd : UInt32 := 16#FFFF_FF1B#;
            Ptr : UInt32;
            Value : UInt32;
            Num : UInt32;
         when True =>
            Arr : Coproc_List (1 .. 4);
      end case;
   end record
     with Unchecked_Union, Size => 128;

   for CMD_MEMSET use record
      Cmd at 0 range 0 .. 31;
      Ptr at 4 range 0 .. 31;
      Value at 8 range 0 .. 31;
      Num at 12 range 0 .. 31;
      Arr at 0 range 0 .. 127;
   end record;

   function Fault_Occured (This : FT801_Device) return Boolean;

   procedure Recover_Fault (This : FT801_Device);


private

   function Compute_Free_Space (This : FT801_Device) return Fifo_Pointer_Type;



end FT801.Coproc;
