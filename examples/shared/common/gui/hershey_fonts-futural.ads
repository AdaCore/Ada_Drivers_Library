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

--  This file accompanied the original distribution of the Hershey fonts,
--  via the Usenet :
--
--  This distribution is made possible through the collective encouragement of
--  the Usenet Font Consortium, a mailing list that sprang to life to get this
--  accomplished and that will now most likely disappear into the mists of
--  time... Thanks are especially due to Jim Hurt, who provided the packed
--  font data for the distribution, along with a lot of other help.
--
--  This file describes the Hershey Fonts in general, along with a description
--  of the other files in this distribution and a simple re-distribution
--  restriction.
--
--  USE RESTRICTION: This distribution of the Hershey Fonts may be used by
--  anyone for any purpose, commercial or otherwise, providing that:
--  1. The following acknowledgements must be distributed with the font data:
--     - The Hershey Fonts were originally created by Dr. A. V. Hershey while
--       working at the U. S.  National Bureau of Standards.
--     - The format of the Font data in this distribution was originally
--       created by:
--       James Hurt
--       Cognition, Inc.
--       900 Technology Park Drive
--       Billerica, MA 01821
--       (mit-eddie!ci-dandelion!hurt)
--  2. The font data in this distribution may be converted into any other
--     format *EXCEPT* the format distributed by the U.S. NTIS (which
--     organization holds the rights to the distribution and use of the font
--     data in that particular format). Not that anybody would really *want*
--     to use their format... each point is described in eight UInt8s as
--     "xxx yyy:", where xxx and yyy are the coordinate values as ASCII
--     numbers.
--
--  *PLEASE* be reassured: The legal implications of NTIS' attempt to control
--  a particular form of the Hershey Fonts *are* troubling. HOWEVER: We have
--  been endlessly and repeatedly assured by NTIS that they do not care what
--  we do with our version of the font data, they do not want to know about
--  it, they understand that we are distributing this information all over the
--  world, etc etc etc... but because it isn't in their *exact* distribution
--  format, they just don't care!!! So go ahead and use the data with a
--  clear conscience! (If you feel bad about it, take a smaller deduction
--  for something on your taxes next week...)

package Hershey_Fonts.FuturaL is

   pragma Warnings (Off);

   Desc : aliased constant String :=
            "12345  1JZ" &
            "12345  9MWRFRT RRYQZR[SZRY" &
            "12345  6JZNFNM RVFVM" &
            "12345 12H]SBLb RYBRb RLOZO RKUYU" &
            "12345 27H\PBP_ RTBT_ RYIWGTFPFMGKIKKLMMNOOUQWRXSYUYXWZT[P[MZKX" &
            "12345 32F^[FI[ RNFPHPJOLMMKMIKIIJGLFNFPGSHVHYG[F RWTUUTWTYV[X[Z" &
            "Z[X[VYTWT" &
            "12345 35E_\O\N[MZMYNXPVUTXRZP[L[JZIYHWHUISJRQNRMSKSIRGPFNGMIMKN" &
            "NPQUXWZY[[[\Z\Y" &
            "12345  8MWRHQGRFSGSIRKQL" &
            "12345 11KYVBTDRGPKOPOTPYR]T`Vb" &
            "12345 11KYNBPDRGTKUPUTTYR]P`Nb" &
            "12345  9JZRLRX RMOWU RWOMU" &
            "12345  6E_RIR[ RIR[R" &
            "12345  8NVSWRXQWRVSWSYQ[" &
            "12345  3E_IR[R" &
            "12345  6NVRVQWRXSWRV" &
            "12345  3G][BIb" &
            "12345 18H\QFNGLJKOKRLWNZQ[S[VZXWYRYOXJVGSFQF" &
            "12345  5H\NJPISFS[" &
            "12345 15H\LKLJMHNGPFTFVGWHXJXLWNUQK[Y[" &
            "12345 16H\MFXFRNUNWOXPYSYUXXVZS[P[MZLYKW" &
            "12345  7H\UFKTZT RUFU[" &
            "12345 18H\WFMFLOMNPMSMVNXPYSYUXXVZS[P[MZLYKW" &
            "12345 24H\XIWGTFRFOGMJLOLTMXOZR[S[VZXXYUYTXQVOSNRNOOMQLT" &
            "12345  6H\YFO[ RKFYF" &
            "12345 30H\PFMGLILKMMONSOVPXRYTYWXYWZT[P[MZLYKWKTLRNPQOUNWMXKXIW" &
            "GTFPF" &
            "12345 24H\XMWPURRSQSNRLPKMKLLINGQFRFUGWIXMXRWWUZR[P[MZLX" &
            "12345 12NVROQPRQSPRO RRVQWRXSWRV" &
            "12345 14NVROQPRQSPRO RSWRXQWRVSWSYQ[" &
            "12345  4F^ZIJRZ[" &
            "12345  6E_IO[O RIU[U" &
            "12345  4F^JIZRJ[" &
            "12345 21I[LKLJMHNGPFTFVGWHXJXLWNVORQRT RRYQZR[SZRY" &
            "12345 56E`WNVLTKQKOLNMMPMSNUPVSVUUVS RQKOMNPNSOUPV RWKVSVUXVZV\" &
            "T]Q]O\L[JYHWGTFQFNGLHJJILHOHRIUJWLYNZQ[T[WZYYZX RXKWSWUXV" &
            "12345  9I[RFJ[ RRFZ[ RMTWT" &
            "12345 24G\KFK[ RKFTFWGXHYJYLXNWOTP RKPTPWQXRYTYWXYWZT[K[" &
            "12345 19H]ZKYIWGUFQFOGMILKKNKSLVMXOZQ[U[WZYXZV" &
            "12345 16G\KFK[ RKFRFUGWIXKYNYSXVWXUZR[K[" &
            "12345 12H[LFL[ RLFYF RLPTP RL[Y[" &
            "12345  9HZLFL[ RLFYF RLPTP" &
            "12345 23H]ZKYIWGUFQFOGMILKKNKSLVMXOZQ[U[WZYXZVZS RUSZS" &
            "12345  9G]KFK[ RYFY[ RKPYP" &
            "12345  3NVRFR[" &
            "12345 11JZVFVVUYTZR[P[NZMYLVLT" &
            "12345  9G\KFK[ RYFKT RPOY[" &
            "12345  6HYLFL[ RL[X[" &
            "12345 12F^JFJ[ RJFR[ RZFR[ RZFZ[" &
            "12345  9G]KFK[ RKFY[ RYFY[" &
            "12345 22G]PFNGLIKKJNJSKVLXNZP[T[VZXXYVZSZNYKXIVGTFPF" &
            "12345 14G\KFK[ RKFTFWGXHYJYMXOWPTQKQ" &
            "12345 25G]PFNGLIKKJNJSKVLXNZP[T[VZXXYVZSZNYKXIVGTFPF RSWY]" &
            "12345 17G\KFK[ RKFTFWGXHYJYLXNWOTPKP RRPY[" &
            "12345 21H\YIWGTFPFMGKIKKLMMNOOUQWRXSYUYXWZT[P[MZKX" &
            "12345  6JZRFR[ RKFYF" &
            "12345 11G]KFKULXNZQ[S[VZXXYUYF" &
            "12345  6I[JFR[ RZFR[" &
            "12345 12F^HFM[ RRFM[ RRFW[ R\FW[" &
            "12345  6H\KFY[ RYFK[" &
            "12345  7I[JFRPR[ RZFRP" &
            "12345  9H\YFK[ RKFYF RK[Y[" &
            "12345 12KYOBOb RPBPb ROBVB RObVb" &
            "12345  3KYKFY^" &
            "12345 12KYTBTb RUBUb RNBUB RNbUb" &
            "12345  6JZRDJR RRDZR" &
            "12345  3I[Ib[b" &
            "12345  8NVSKQMQORPSORNQO" &
            "12345 18I\XMX[ RXPVNTMQMONMPLSLUMXOZQ[T[VZXX" &
            "12345 18H[LFL[ RLPNNPMSMUNWPXSXUWXUZS[P[NZLX" &
            "12345 15I[XPVNTMQMONMPLSLUMXOZQ[T[VZXX" &
            "12345 18I\XFX[ RXPVNTMQMONMPLSLUMXOZQ[T[VZXX" &
            "12345 18I[LSXSXQWOVNTMQMONMPLSLUMXOZQ[T[VZXX" &
            "12345  9MYWFUFSGRJR[ ROMVM" &
            "12345 23I\XMX]W`VaTbQbOa RXPVNTMQMONMPLSLUMXOZQ[T[VZXX" &
            "12345 11I\MFM[ RMQPNRMUMWNXQX[" &
            "12345  9NVQFRGSFREQF RRMR[" &
            "12345 12MWRFSGTFSERF RSMS^RaPbNb" &
            "12345  9IZMFM[ RWMMW RQSX[" &
            "12345  3NVRFR[" &
            "12345 19CaGMG[ RGQJNLMOMQNRQR[ RRQUNWMZM\N]Q][" &
            "12345 11I\MMM[ RMQPNRMUMWNXQX[" &
            "12345 18I\QMONMPLSLUMXOZQ[T[VZXXYUYSXPVNTMQM" &
            "12345 18H[LMLb RLPNNPMSMUNWPXSXUWXUZS[P[NZLX" &
            "12345 18I\XMXb RXPVNTMQMONMPLSLUMXOZQ[T[VZXX" &
            "12345  9KXOMO[ ROSPPRNTMWM" &
            "12345 18J[XPWNTMQMNNMPNRPSUTWUXWXXWZT[Q[NZMX" &
            "12345  9MYRFRWSZU[W[ ROMVM" &
            "12345 11I\MMMWNZP[S[UZXW RXMX[" &
            "12345  6JZLMR[ RXMR[" &
            "12345 12G]JMN[ RRMN[ RRMV[ RZMV[" &
            "12345  6J[MMX[ RXMM[" &
            "12345 10JZLMR[ RXMR[P_NaLbKb" &
            "12345  9J[XMM[ RMMXM RM[X[" &
            "12345 40KYTBRCQDPFPHQJRKSMSOQQ RRCQEQGRISJTLTNSPORSTTVTXSZR[Q]Q" &
            "_Ra RQSSUSWRYQZP\P^Q`RaTb" &
            "12345  3NVRBRb" &
            "12345 40KYPBRCSDTFTHSJRKQMQOSQ RRCSESGRIQJPLPNQPURQTPVPXQZR[S]S" &
            "_Ra RSSQUQWRYSZT\T^S`RaPb" &
            "12345 24F^IUISJPLONOPPTSVTXTZS[Q RISJQLPNPPQTTVUXUZT[Q[O" &
            "12345 35JZJFJ[K[KFLFL[M[MFNFN[O[OFPFP[Q[QFRFR[S[SFTFT[U[UFVFV[W" &
            "[WFXFX[Y[YFZFZ[";

   Font : Font_Desc := Desc'Access;

   pragma Warnings (On);

end Hershey_Fonts.FuturaL;
