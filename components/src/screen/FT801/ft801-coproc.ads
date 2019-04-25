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

   type Fifo_Pointer is range 0 .. 4095;

   type Cmd_Fifo is record
      Pointer : Fifo_Pointer := 0;
   end record;

   procedure Cmd_Start (This : in out FT801_Device);

   procedure Cmd_End (This : in out FT801_Device);

   CMD_DLSTART   : constant UInt32 := 16#FFFF_FF00#;
   CMD_DLSWAP    : constant UInt32 := 16#FFFF_FF01#;
   CMD_COLDSTART : constant UInt32 := 16#FFFF_FF32#;

   type CMD_INTERRUPT (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Cmd : UInt32 := 16#FFFF_FF02#;
            MS  : UInt32;
         when True =>
            Arr : UInt8_Array (1 .. 8);
      end case;
   end record
     with Unchecked_Union, Size => 64;

   for CMD_INTERRUPT use record
      Cmd at 0 range 0 .. 31;
      MS at 0 range 32 .. 63;
      Arr at 0 range 0 .. 63;
   end record;






end FT801.Coproc;
