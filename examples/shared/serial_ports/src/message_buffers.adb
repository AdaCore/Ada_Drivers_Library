------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

package body Message_Buffers is

   -------------
   -- Content --
   -------------

   function Content (This : Message) return String is
   begin
      return This.Content (1 .. This.Length);
   end Content;

   ----------------
   -- Content_At --
   ----------------

   function Content_At (This : Message;  Index : Positive) return Character is
   begin
      return This.Content (Index);
   end Content_At;

   ------------
   -- Length --
   ------------

   function Length (This : Message) return Natural is
   begin
      return This.Length;
   end Length;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Message) is
   begin
      This.Length := 0;
   end Clear;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Message;  Value : Character) is
   begin
      This.Length := This.Length + 1;
      This.Content (This.Length) := Value;
   end Append;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Message;  To : String) is
   begin
      This.Content (1 .. To'Length) := To;
      This.Length := To'Length;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Message;  To : Character) is
   begin
      This.Content (1) := To;
      This.Length := 1;
   end Set;

   --------------------
   -- Set_Terminator --
   --------------------

   procedure Set_Terminator (This : in out Message;  To : Character) is
   begin
      This.Terminator := To;
   end Set_Terminator;

   ----------------
   -- Terminator --
   ----------------

   function Terminator (This : Message) return Character is
   begin
      return This.Terminator;
   end Terminator;

   ---------------------------------
   -- Await_Transmission_Complete --
   ---------------------------------

   procedure Await_Transmission_Complete (This : in out Message) is
   begin
      Suspend_Until_True (This.Transmission_Complete);
   end Await_Transmission_Complete;

   ------------------------------
   -- Await_Reception_Complete --
   ------------------------------

   procedure Await_Reception_Complete (This : in out Message) is
   begin
      Suspend_Until_True (This.Reception_Complete);
   end Await_Reception_Complete;

   ----------------------------------
   -- Signal_Transmission_Complete --
   ----------------------------------

   procedure Signal_Transmission_Complete (This : in out Message) is
   begin
      Set_True (This.Transmission_Complete);
   end Signal_Transmission_Complete;

   -------------------------------
   -- Signal_Reception_Complete --
   -------------------------------

   procedure Signal_Reception_Complete (This : in out Message) is
   begin
      Set_True (This.Reception_Complete);
   end Signal_Reception_Complete;

   ----------------
   -- Note_Error --
   ----------------

   procedure Note_Error (This : in out Message; Condition : Error_Conditions) is
   begin
      This.Error_Status := This.Error_Status or Condition;
   end Note_Error;

   ---------------------
   -- Errors_Detected --
   ---------------------

   function Errors_Detected (This : Message) return Error_Conditions is
   begin
      return This.Error_Status;
   end Errors_Detected;

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors (This : in out Message) is
   begin
      This.Error_Status := No_Error_Detected;
   end Clear_Errors;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (This : Message; Condition : Error_Conditions) return Boolean is
   begin
      return (This.Error_Status and Condition) /= 0;
   end Has_Error;

end Message_Buffers;
