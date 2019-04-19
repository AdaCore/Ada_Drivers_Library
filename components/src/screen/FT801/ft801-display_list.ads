package FT801.Display_List is
   
   type Cmd_List is array (Natural range <>) of UInt32;
   
   procedure Send_Cmd_List (This : FT801_Device;
                            Cmds : Cmd_List);
   
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
  
   type Bitmap_Size (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            Cmd : UInt8 := 16#08#;
            RSVD : UInt3 := 0;
            Filter : Boolean;
            Wrapx  : Boolean;
            WrapY  : Boolean;
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

end FT801.Display_List;
