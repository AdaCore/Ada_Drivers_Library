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
   
   procedure Cmd_Start;
   
   procedure Cmd_End;
   
   
end FT801.Coproc;
