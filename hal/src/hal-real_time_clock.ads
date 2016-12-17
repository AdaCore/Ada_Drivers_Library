------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

package HAL.Real_Time_Clock is

   type RTC_Hour is mod 24;
   type RTC_Minute is mod 60;
   type RTC_Second is mod 60;

   type RTC_Time is record
      Hour : RTC_Hour;
      Min  : RTC_Minute;
      Sec  : RTC_Second;
   end record;

   type RTC_Month is (January, February, March, April, May, June, July, August,
                      September, October, November, December);
   type RTC_Day_Of_Week is (Monday, Tuesday, Wednesday, Thursday, Friday,
                            Saturday, Sunday);
   type RTC_Day is range 1 .. 31;
   type RTC_Year is range 00 .. 99;

   type RTC_Date is record
      Day_Of_Week : RTC_Day_Of_Week;
      Day         : RTC_Day;
      Month       : RTC_Month;
      Year        : RTC_Year;
   end record;

   for RTC_Day_Of_Week use (Monday    => 1,
                            Tuesday   => 2,
                            Wednesday => 3,
                            Thursday  => 4,
                            Friday    => 5,
                            Saturday  => 6,
                            Sunday    => 7);

   for RTC_Month use (January   => 1,
                      February  => 2,
                      March     => 3,
                      April     => 4,
                      May       => 5,
                      June      => 6,
                      July      => 7,
                      August    => 8,
                      September => 9,
                      October   => 10,
                      November  => 11,
                      December  => 12);

   type RTC_Device is limited interface;
   type Any_RTC_Device is access all RTC_Device'Class;

   procedure Set (This : in out RTC_Device;
                  Time : RTC_Time;
                  Date : RTC_Date) is abstract;

   procedure Get (This : in out RTC_Device;
                  Time : out RTC_Time;
                  Date : out RTC_Date) is abstract;
   --  This procedure ensures coherent time and date values

   function Get_Time (This : RTC_Device)
                      return HAL.Real_Time_Clock.RTC_Time
                      is abstract;

   function Get_Date (This : RTC_Device)
                      return HAL.Real_Time_Clock.RTC_Date
                      is abstract;

end HAL.Real_Time_Clock;
