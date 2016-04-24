with Interfaces;

package HAL is
   pragma Pure;

   subtype Word is Interfaces.Unsigned_32;
   subtype Short is Interfaces.Unsigned_16;
   subtype Byte is Interfaces.Unsigned_8;

   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;

   type Byte_Array is array (Natural range <>) of Byte;
   type Short_Array is array (Natural range <>) of Short;
   type Word_Array is array (Natural range <>) of Word;
end HAL;
