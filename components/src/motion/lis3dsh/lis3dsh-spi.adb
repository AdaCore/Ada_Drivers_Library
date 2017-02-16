------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    lis3dsh.c                                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file provides a set of functions needed to manage the    --
--            LIS3DSH MEMS Accelerometer.                                   --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This private package provides an I/O  implementation for the LIS3DSH
--  accelerometer chip.

package body LIS3DSH.SPI is

   --------------
   -- IO_Write --
   --------------

   overriding
   procedure IO_Write
     (This      : in out Three_Axis_Accelerometer_SPI;
      Value     : UInt8;
      WriteAddr : Register_Address)
   is
      Status : SPI_Status;
   begin
      This.Chip_Select.Clear;
      This.Port.Transmit (SPI_Data_8b'(UInt8 (WriteAddr or SPI_Write_Flag),
                          Value),
                          Status);
      This.Chip_Select.Set;

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end IO_Write;

   -------------
   -- IO_Read --
   -------------

   overriding
   procedure IO_Read
     (This     : Three_Axis_Accelerometer_SPI;
      Value    : out UInt8;
      ReadAddr : Register_Address)
   is
      Data : SPI_Data_8b (1 .. 1);
      Status : SPI_Status;
   begin
      This.Chip_Select.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => UInt8 (ReadAddr or SPI_Read_Flag)),
                          Status);
      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      This.Port.Receive (Data, Status);

      This.Chip_Select.Set;

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
      Value := Data (1);
   end IO_Read;

end LIS3DSH.SPI;
