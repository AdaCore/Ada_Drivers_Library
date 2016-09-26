
package Grid is
   type Size is new Integer range 0 .. 3;

   type Grid_T is array (Size, Size) of Integer;

   type Direction_E is (Left, Up, Right, Down);

   type CGrid is tagged record
      Grid  : Grid_T;
      Score : Natural;
   end record;


   type Cell_Position_T is record
      X : Size;
      Y : Size;
   end record;
   type Moving_Tile_T is record
      Current : Cell_Position_T;
      Previous : Cell_Position_T;
   end record;
   type Trace_Grid_T is array (Size, Size) of Cell_Position_T;

   procedure Init (G : in out CGrid);

   function Get
     (G : in CGrid;
      X : Size;
      Y : Size) return Integer;

   procedure Set
     (G     : in out CGrid;
      X     : Size;
      Y     : Size;
      Value : Integer);

   procedure Move
     (G         : in out CGrid;
      Direction : Direction_E);

   function Move
     (G         : in out CGrid;
      Direction : Direction_E) return Trace_Grid_T;

   function Can_Move
     (G         : in out CGrid;
      Direction : Direction_E) return Boolean;

private
   type Row_T is array (Size) of Integer;
   type Row_Trace_T is array (Size) of Size;

   type Rows_T is array (Size) of Row_T;

   type Rows_Traces_T is array (Size) of Row_Trace_T;

   procedure Compact_Row (Row   : in out Row_T;
                          Score : in out Natural);

   function Compact_Row (Row   : in out Row_T;
                         Score : in out Natural) return Row_Trace_T;

   procedure Compact_Rows (Rows  : in out Rows_T;
                           Score : in out Natural);

   function Compact_Rows (Rows  : in out Rows_T;
                          Score : in out Natural) return Rows_Traces_T;

   function Grid_To_Rows
     (G         : in CGrid;
      Direction : Direction_E)  return Rows_T;

   procedure Rows_To_Grid
     (G         : in out CGrid;
      Rows      : Rows_T;
      Direction : Direction_E);

   function Traces_To_Move_Grid
     (Traces    : Rows_Traces_T;
      Direction : Direction_E) return Trace_Grid_T;

end Grid;
