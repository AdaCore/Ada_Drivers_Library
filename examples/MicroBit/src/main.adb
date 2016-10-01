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

with nrf51.GPIO;   use nrf51.GPIO;
with nrf51.Device; use nrf51.Device;

procedure Main is
   Conf : GPIO_Configuration;
begin
   Conf.Mode      := Mode_Out;
   Conf.Resistors := Pull_Down;

   --  Column
   P04.Configure_IO (Conf);
   P05.Configure_IO (Conf);
   P06.Configure_IO (Conf);
   P07.Configure_IO (Conf);
   P08.Configure_IO (Conf);
   P09.Configure_IO (Conf);
   P10.Configure_IO (Conf);
   P11.Configure_IO (Conf);
   P12.Configure_IO (Conf);

   --  Row
   P13.Configure_IO (Conf);
   P14.Configure_IO (Conf);
   P15.Configure_IO (Conf);


   --  Column sink current
   P04.Clear;
   P05.Clear;
   P06.Clear;
   P07.Clear;
   P08.Clear;
   P09.Clear;
   P10.Clear;
   P11.Clear;
   P12.Clear;

   --  Blinky!!!

   loop
      --  Row source current
      P13.Set;
      P14.Set;
      P15.Set;
      for Cnt in 1 .. 100_000 loop
         null;
      end loop;
      P13.Clear;
      P14.Clear;
      P15.Clear;
      for Cnt in 1 .. 100_000 loop
         null;
      end loop;
   end loop;
end Main;
