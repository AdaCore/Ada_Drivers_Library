------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

--  This package allows allocating/deallocating memory from the GPU memory.
--  Such memory is particularly useful to achieve non-cached or coherent memory
--  operations between the GPU and the ARM.
package RPi.Firmware.GPU_Memory is

   type Memory_Flag is new UInt32;
   type Memory_Handle is new UInt32;

   Invalid_Memory_Handle : constant Memory_Handle := 0;

   Mem_Flag_Normal            : constant Memory_Flag;
   --  normal allocating alias. Don't use from ARM

   Mem_Flag_Discardable       : constant Memory_Flag;
   --  can be resized to 0 at any time. Use for cached data

   Mem_Flag_Direct            : constant Memory_Flag;
   --  0xC alias uncached

   Mem_Flag_Coherent          : constant Memory_Flag;
   --  0x8 alias. Non-allocating in L2 but coherent

   Mem_Flag_L1_Non_Allocating : constant Memory_Flag;
   --  Allocating in L2

   Mem_Flag_Zero              : constant Memory_Flag;
   --  Initialise buffer to all zeros

   Mem_Flag_No_Init           : constant Memory_Flag;
   --  Don't initialize (default is initialise to ones)

   Mem_Flag_Hint_Permalock    : constant Memory_Flag;
   --  Likely to be locked for long periods of time.

   procedure Memory_Allocate
     (Size      : UInt32;
      Alignment : UInt32;
      Flags     : Memory_Flag;
      Handle    : out Memory_Handle);
   --  Allocates contiguous memory on the GPU. size and alignment are in bytes.

   procedure Memory_Lock
     (Handle : Memory_Handle;
      Addr   : out BUS_Address);
   --  Lock buffer in place, and return a bus address. Must be done before
   --  memory can be accessed

   procedure Memory_Unlock
     (Handle : Memory_Handle;
      Status : out Boolean);
   --  Unlock buffer. It retains contents, but may move. Needs to be locked
   --  before next use.
   --  Status set to True upon success.

   procedure Memory_Release
     (Handle : Memory_Handle;
      Status : out Boolean);
   --  Free the memory buffer.
   --  Status set to True upon success.

private

   Mem_Flag_Normal         : constant Memory_Flag := 2#000_0000#;
   --  normal allocating alias. Don't use from ARM

   Mem_Flag_Discardable    : constant Memory_Flag := 2#000_0001#;
   --  can be resized to 0 at any time. Use for cached data

   Mem_Flag_Direct         : constant Memory_Flag := 2#000_0100#;
   --  0xC alias uncached

   Mem_Flag_Coherent       : constant Memory_Flag := 2#000_1000#;
   --  0x8 alias. Non-allocating in L2 but coherent

   Mem_Flag_L1_Non_Allocating : constant Memory_Flag :=
                                  Mem_Flag_Direct or Mem_Flag_Coherent;
   --  Allocating in L2

   Mem_Flag_Zero           : constant Memory_Flag := 2#001_0000#;
   --  Initialise buffer to all zeros

   Mem_Flag_No_Init        : constant Memory_Flag := 2#010_0000#;
   --  Don't initialize (default is initialise to ones)

   Mem_Flag_Hint_Permalock : constant Memory_Flag := 2#100_0000#;
   --  Likely to be locked for long periods of time.

end RPi.Firmware.GPU_Memory;
