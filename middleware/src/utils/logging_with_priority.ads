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

generic
   type Priorities is (<>);
   --  Logging priority type. Higher value means higher priority.

   Maximum_Message_Length     : Positive;
   --  Maximum number of Characters in a message. Messages longer than this
   --  limit will be rejected.

   Maximum_Number_Of_Messages : Positive;
   --  Maximum number of messages in the queue. When the queue is filled the
   --  lower priority will be discared.

package Logging_With_Priority is

   procedure Log_Line (Str : String; Prio : Priorities);
   --  Add message in the logging queue. The messages are sorted with FIFO
   --  within priorities.
   --
   --  The message will be rejected right away if:
   --     - The message is longer than Maximum_Message_Length
   --     - The queue if full of messages with higher or equal priority
   --
   --  The message might be reject later if the queue is full and a message
   --  with higher priority is inserted.

   procedure Pop (Str    : out String;
                  Length : out Natural;
                  Prio   : out Priorities)
     with Pre => Str'Length = Maximum_Message_Length;
   --  Remove the top priority message from the queue. Length will be zero when
   --  there's no message in the queue.

   function Full return Boolean;
   --  Return True if the message queue is full

   function Empty return Boolean;
   --  Return True if the message queue is empty

end Logging_With_Priority;
