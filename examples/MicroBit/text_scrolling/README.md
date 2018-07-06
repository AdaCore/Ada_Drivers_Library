Text Scrolling Example
======================

In this example we will see how to display text on the LED matrix of the
micro:bit.

Code
====

To display text on the LED matrix, we will use the procedure `Display` of the
`MicroBit.Display` package.


```ada
   procedure Display (Str : String)
     with Pre => Str'Length <= Scroll_Text_Max_Length;
```

Arguments:

 - Str : The text to be displayed on the LED matrix

Precondition:

The procedure `Display` has a precondition that the length of the text cannot
be more than `Scroll_Text_Max_Length` (128 characters).

Here is the code:
```ada
with MicroBit.Display;

procedure Main is
begin

   loop
      MicroBit.Display.Display ("Make with Ada!  ");
   end loop;
end Main;
```
