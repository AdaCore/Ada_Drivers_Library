package Sudoku is
   type sudoku_ar_t is array (Integer range 0 .. 80) of Integer range 0 .. 9;
   FINISH_EXCEPTION : exception;
   function checkValidity (val : Integer; x : Integer; y : Integer;  sudoku_ar : in  sudoku_ar_t) return Boolean;
   function placeNumber (pos : Integer; sudoku_ar : in out sudoku_ar_t) return Boolean;
   function solve (sudoku_ar : in out sudoku_ar_t) return Boolean;
   procedure test;
end Sudoku;
