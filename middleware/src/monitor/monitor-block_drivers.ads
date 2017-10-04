------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with HAL;               use HAL;
with HAL.Block_Drivers; use HAL.Block_Drivers;

package Monitor.Block_Drivers is

   type Put_Line_Procedure is access procedure (Str : String);

   type Block_Driver_Monitor
     (Driver_Under_Monitoring : not null Any_Block_Driver;
      Put_Line                : not null Put_Line_Procedure)
   is new Block_Driver with private;

   overriding
   function Read
     (This         : in out Block_Driver_Monitor;
      Block_Number : UInt64;
      Data         : out Block)
     return Boolean;

   overriding
   function Write
     (This         : in out Block_Driver_Monitor;
      Block_Number : UInt64;
      Data         : Block)
     return Boolean;

   procedure Enable (This : in out Block_Driver_Monitor);
   --  Enable monitor's output (default)

   procedure Disable (This : in out Block_Driver_Monitor);
   --  Disable monitor's output

private

   type Block_Driver_Monitor
     (Driver_Under_Monitoring : not null Any_Block_Driver;
      Put_Line                : not null Put_Line_Procedure)
   is new Block_Driver with record
      Enabled : Boolean := True;
   end record;
end Monitor.Block_Drivers;
