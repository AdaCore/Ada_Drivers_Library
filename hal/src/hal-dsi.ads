------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

--  Display Serial Interface

package HAL.DSI is

   subtype DSI_Virtual_Channel_ID is UInt2;

   type DSI_Data is array (Positive range <>) of UInt8;

   type DSI_Pkt_Data_Type is
     (DCS_Short_Pkt_Write_P0, --  DCS Short write, no parameter
      DCS_Short_Pkt_Write_P1, --  DCS Short write, one parameter
      Gen_Short_Pkt_Write_P0, --  Generic Short write, no parameter
      Gen_Short_Pkt_Write_P1, --  Generic Short write, one parameter
      Gen_Short_Pkt_Write_P2, --  Generic Short write, two parameters
      DCS_Long_Pkt_Write,     --  DCS Long write
      Gen_Long_Pkt_Write,     --  Generic Long write
      DCS_Short_Pkt_Read,     --  DCS Short read
      Gen_Short_Pkg_Read_P0,  --  Gen read, no parameter
      Gen_Short_Pkg_Read_P1,  --  Gen read, one parameter
      Gen_Short_Pkg_Read_P2); --  Gen read, two parameter

   subtype DSI_Short_Write_Packet_Data_Type is DSI_Pkt_Data_Type range
     DCS_Short_Pkt_Write_P0 .. Gen_Short_Pkt_Write_P2;

   subtype DSI_Long_Write_Packet_Data_Type is DSI_Pkt_Data_Type range
     DCS_Long_Pkt_Write .. Gen_Long_Pkt_Write;

   type DSI_Port is limited interface;

   type Any_DSI_Port is access all DSI_Port'Class;

   procedure DSI_Short_Write
     (Port       : in out DSI_Port;
      Channel_ID : DSI_Virtual_Channel_ID;
      Mode       : DSI_Short_Write_Packet_Data_Type;
      Param1     : UInt8;
      Param2     : UInt8) is abstract;

   procedure DSI_Long_Write
     (Port       : in out DSI_Port;
      Channel_Id : DSI_Virtual_Channel_ID;
      Mode       : DSI_Long_Write_Packet_Data_Type;
      Param1     : UInt8;
      Parameters : DSI_Data) is abstract;

end HAL.DSI;
