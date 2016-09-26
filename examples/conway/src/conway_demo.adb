
--  The file declares the main procedure for the demonstration.

with Conway_Driver;        pragma Unreferenced (Conway_Driver);
--  The Conway_Driver package contains the task that actually controls the app.
--  Although it is not referenced directly in the main procedure, we need it
--  in the closure of the context clauses so that it will be included in the
--  executable.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with Ada.Real_Time;
with System;

procedure Conway_Demo is
   pragma Priority (System.Priority'First);
begin
   delay until Ada.Real_Time.Time_Last;
end Conway_Demo;
