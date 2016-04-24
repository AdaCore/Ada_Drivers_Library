--  Display Serial Interface

package HAL.DSI is

   subtype DSI_Virtual_Channel_ID is UInt2;

   type DSI_Data is array (Positive range <>) of Byte;

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
   type DSI_Port_Ref is not null access all DSI_Port'Class;

   procedure DSI_Short_Write
     (Port       : in out DSI_Port;
      Channel_ID : DSI_Virtual_Channel_ID;
      Mode       : DSI_Short_Write_Packet_Data_Type;
      Param1     : Byte;
      Param2     : Byte) is abstract;

   procedure DSI_Long_Write
     (Port       : in out DSI_Port;
      Channel_Id : DSI_Virtual_Channel_ID;
      Mode       : DSI_Long_Write_Packet_Data_Type;
      Param1     : Byte;
      Parameters : DSI_Data) is abstract;

end HAL.DSI;
