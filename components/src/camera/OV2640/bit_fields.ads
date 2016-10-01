with HAL; use HAL;

package Bit_Fields is
   type Bit_Field is array (Natural range <>) of Bit
     with Component_Size => 1;

   function To_Word (Bits : Bit_Field) return UInt32;
   function To_UInt16 (Bits : Bit_Field) return UInt16;
   function To_Byte (Bits : Bit_Field) return Byte;

   function To_Bit_Field (Value : UInt32) return Bit_Field
     with Post => To_Bit_Field'Result'First = 0
     and then
       To_Bit_Field'Result'Last = 31;
   function To_Bit_Field (Value : UInt16) return Bit_Field
     with Post => To_Bit_Field'Result'First = 0
     and then
       To_Bit_Field'Result'Last = 15;
   function To_Bit_Field (Value : Byte) return Bit_Field
     with Post => To_Bit_Field'Result'First = 0
     and then
       To_Bit_Field'Result'Last = 7;
end Bit_Fields;
