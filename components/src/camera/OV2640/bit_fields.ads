with Interfaces.Bit_Types; use Interfaces.Bit_Types;

package Bit_Fields is
   type Bit_Field is array (Natural range <>) of Bit
     with Component_Size => 1;

   function To_Word (Bits : Bit_Field) return Word
     with Pre => Bits'First = 0 and then Bits'Last = 31;
   function To_Short (Bits : Bit_Field) return Short
     with Pre => Bits'First = 0 and then Bits'Last = 15;
   function To_Byte (Bits : Bit_Field) return Byte
     with Pre => Bits'First = 0 and then Bits'Last = 7;
   function To_Bit_Field (Value : Word) return Bit_Field
     with Post => To_Bit_Field'Result'First = 0
     and then
       To_Bit_Field'Result'Last = 31;

   function To_Bit_Field (Value : Short) return Bit_Field
     with Post => To_Bit_Field'Result'First = 0
     and then
       To_Bit_Field'Result'Last = 15;
   function To_Bit_Field (Value : Byte) return Bit_Field
     with Post => To_Bit_Field'Result'First = 0
     and then
       To_Bit_Field'Result'Last = 7;
end Bit_Fields;
