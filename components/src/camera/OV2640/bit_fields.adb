package body Bit_Fields is
   type Convert_8 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Value : Byte;
         when True =>
            Bits : Bit_Field (0 .. 7);
      end case;
   end record with Unchecked_Union, Size => 8;

   for Convert_8 use record
      Value  at 0 range 0 .. 7;
      Bits   at 0 range 0 .. 7;
   end record;

   type Convert_16 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Value : UInt16;
         when True =>
            Bits : Bit_Field (0 .. 15);
      end case;
   end record with Unchecked_Union, Size => 16;

   for Convert_16 use record
      Value  at 0 range 0 .. 15;
      Bits   at 0 range 0 .. 15;
   end record;

   type Convert_32 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Value : UInt32;
         when True =>
            Bits : Bit_Field (0 .. 31);
      end case;
   end record with Unchecked_Union, Size => 32;

   for Convert_32 use record
      Value  at 0 range 0 .. 31;
      Bits   at 0 range 0 .. 31;
   end record;


   -------------
   -- To_Word --
   -------------

   function To_Word (Bits : Bit_Field) return UInt32 is
      Tmp : Convert_32;
   begin
      Tmp.Bits := Bits;
      return Tmp.Value;
   end To_Word;

   --------------
   -- To_UInt16 --
   --------------

   function To_UInt16 (Bits : Bit_Field) return UInt16 is
      Tmp : Convert_16;
   begin
      Tmp.Bits := Bits;
      return Tmp.Value;
   end To_UInt16;

   -------------
   -- To_Byte --
   -------------

   function To_Byte (Bits : Bit_Field) return Byte is
      Tmp : Convert_8;
   begin
      Tmp.Bits := Bits;
      return Tmp.Value;
   end To_Byte;

   ------------------
   -- To_Bit_Field --
   ------------------

   function To_Bit_Field (Value : UInt32) return Bit_Field is
      Tmp : Convert_32;
   begin
      Tmp.Value := Value;
      return Tmp.Bits;
   end To_Bit_Field;

   ------------------
   -- To_Bit_Field --
   ------------------

   function To_Bit_Field (Value : UInt16) return Bit_Field is
      Tmp : Convert_16;
   begin
      Tmp.Value := Value;
      return Tmp.Bits;
   end To_Bit_Field;

   ------------------
   -- To_Bit_Field --
   ------------------

   function To_Bit_Field (Value : Byte) return Bit_Field is
      Tmp : Convert_8;
   begin
      Tmp.Value := Value;
      return Tmp.Bits;
   end To_Bit_Field;
end Bit_Fields;
