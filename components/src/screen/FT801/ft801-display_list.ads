package FT801.Display_List is

   type Cmd_List is array (Natural range <>) of UInt32;

   procedure Send_Cmd_List (This : in out FT801_Device;
                            Cmds : Cmd_List)
     with Pre => Cmds'Length <= Dl_Pointer_Type'Last / 4;

   type Graphics_Primitives is
     (BITMAPS,
      POINTS,
      LINES,
      LINE_STRIP,
      EDGE_STRIP_R,
      EDGE_STRIP_L,
      EDGE_STRIP_A,
      EDGE_STRIP_B,
      RECTS)
     with Size => 4;

   for Graphics_Primitives use
     (BITMAPS      => 1,
      POINTS       => 2,
      LINES        => 3,
      LINE_STRIP   => 4,
      EDGE_STRIP_R => 5,
      EDGE_STRIP_L => 6,
      EDGE_STRIP_A => 7,
      EDGE_STRIP_B => 8,
      RECTS        => 9);

   type Cmd_Begin (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd  : UInt8 := 16#1F#;
            RSVD : UInt16 := 0;
            Prim : Graphics_Primitives;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Cmd_Begin use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 4 .. 23;
      Prim at 0 range 0 .. 3;
      Val at 0 range 0 .. 31;
   end record;

   type Bitmap_Layout (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd        : UInt8 := 16#07#;
            Format     : Graphics_Bitmap_Format;
            Linestride : UInt10;
            Height     : UInt9;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Bitmap_Layout use record
      Cmd at 0 range 24 .. 31;
      Format at 0 range 19 .. 23;
      Linestride at 0 range 9 .. 18;
      Height at 0 range 0 .. 8;
      Val at 0 range 0 .. 31;
   end record;

   type Bitmap_Size_Filter_Field is
     (NEAREST, BILINEAR)
     with Size => 1;

   for Bitmap_Size_Filter_Field use
     (NEAREST => 0,
      BILINEAR => 1);

   type Bitmap_Size_Wrap_Field is
     (BORDER, REPEAT)
     with Size => 1;

   for Bitmap_Size_Wrap_Field use
     (BORDER => 0,
      REPEAT => 1);

   type Bitmap_Size (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#08#;
            RSVD : UInt3 := 0;
            Filter : Bitmap_Size_Filter_Field;
            Wrapx  : Bitmap_Size_Wrap_Field;
            WrapY  : Bitmap_Size_Wrap_Field;
            Width  : UInt9;
            Height : UInt9;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Bitmap_Size use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 21 .. 23;
      Filter at 0 range 20 .. 20;
      Wrapx at 0 range 19 .. 19;
      Wrapy at 0 range 18 .. 18;
      Width at 0 range 9 .. 17;
      Height at 0 range 0 .. 8;
      Val at 0 range 0 .. 31;
   end record;

   type Bitmap_Source (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#01#;
            RSVD : UInt4 := 0;
            Addr : UInt20;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Bitmap_Source use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 20 .. 23;
      Addr at 0 range 0 .. 19;
      Val at 0 range 0 .. 31;
   end record;

   type Clear (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#26#;
            RSVD : UInt21 := 0;
            Color    : Boolean;
            Stencil    : Boolean;
            Tag    : Boolean;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Clear use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 3 .. 23;
      Color at 0 range 2 .. 2;
      Stencil at 0 range 1 .. 1;
      Tag at 0 range 0 .. 0;
      Val at 0 range 0 .. 31;
   end record;

   type ClearRGB (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd   : UInt8 := 16#02#;
            Red   : UInt8;
            Blue  : UInt8;
            Green : UInt8;
         when True =>
            Val        : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for ClearRGB use record
      Cmd at 0 range 24 .. 31;
      Red at 0 range 16 .. 23;
      Blue at 0 range 8 .. 15;
      Green at 0 range 0 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type Display (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 0;
            RSVD : UInt24 := 0;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Display use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 0 .. 23;
      Val at 0 range 0 .. 31;
   end record;

   type Cmd_End (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#21#;
            RSVD : UInt24 := 0;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Cmd_End use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 0 .. 23;
      Val at 0 range 0 .. 31;
   end record;

   type Vertex2ii (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt2 := 2#10#;
            X   : UInt9;
            Y   : UInt9;
            Handle : UInt5;
            Cell : UInt7;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Vertex2ii use record
      Cmd at 0 range 30 .. 31;
      X at 0 range 21 .. 29;
      Y at 0 range 12 .. 20;
      Handle at 0 range 7 .. 11;
      Cell at 0 range 0 .. 6;
      Val at 0 range 0 .. 31;
   end record;

   type Vertex2f (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt2 := 2#01#;
            X   : UInt15;
            Y   : UInt15;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Vertex2f use record
      Cmd at 0 range 30 .. 31;
      X at 0 range 15 .. 29;
      Y at 0 range 0 .. 14;
      Val at 0 range 0 .. 31;
   end record;

   type ColorRGB (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#04#;
            Red : UInt8;
            Blue : UInt8;
            Green : UInt8;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for ColorRGB use record
      Cmd at 0 range 24 .. 31;
      Red at 0 range 16 .. 23;
      Blue at 0 range 8 .. 15;
      Green at 0 range 0 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type Point_Size (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#0D#;
            RSVD : UInt7 := 0;
            Size : UInt17;
         when True =>
            Val  : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Point_Size use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 17 .. 23;
      Size at 0 range 0 .. 16;
      Val at 0 range 0 .. 31;
   end record;

   type Save_Context (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#22#;
            RSVD : UInt24 := 16#00#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Save_Context use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 0 .. 23;
      Val at 0 range 0 .. 31;
   end record;

   type Restore_Context (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#23#;
            RSVD : UInt24 := 16#00#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for Restore_Context use record
      Cmd at 0 range 24 .. 31;
      RSVD at 0 range 0 .. 23;
      Val at 0 range 0 .. 31;
   end record;

private

   procedure Poll_For_Ready (This : FT801_Device);

end FT801.Display_List;
