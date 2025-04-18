/*=======================================================================================*/
/*  CHERI RISCV Sail Model                                                               */
/*                                                                                       */
/*  This CHERI Sail RISC-V architecture model here, comprising all files and             */
/*  directories except for the snapshots of the Lem and Sail libraries in the            */
/*  prover_snapshots directory (which include copies of their licenses), is subject      */
/*  to the BSD two-clause licence below.                                                 */
/*                                                                                       */
/*  Copyright (c) 2017-2021                                                              */
/*    Alasdair Armstrong                                                                 */
/*    Thomas Bauereiss                                                                   */
/*    Brian Campbell                                                                     */
/*    Jessica Clarke                                                                     */
/*    Nathaniel Wesley Filardo (contributions prior to July 2020, thereafter Microsoft)  */
/*    Alexandre Joannou                                                                  */
/*    Microsoft                                                                          */
/*    Prashanth Mundkur                                                                  */
/*    Robert Norton-Wright (contributions prior to March 2020, thereafter Microsoft)     */
/*    Alexander Richardson                                                               */
/*    Peter Rugg                                                                         */
/*    Peter Sewell                                                                       */
/*                                                                                       */
/*  All rights reserved.                                                                 */
/*                                                                                       */
/*  This software was developed by SRI International and the University of               */
/*  Cambridge Computer Laboratory (Department of Computer Science and                    */
/*  Technology) under DARPA/AFRL contract FA8650-18-C-7809 ("CIFV"), and                 */
/*  under DARPA contract HR0011-18-C-0016 ("ECATS") as part of the DARPA                 */
/*  SSITH research programme.                                                            */
/*                                                                                       */
/*  This software was developed within the Rigorous Engineering of                       */
/*  Mainstream Systems (REMS) project, partly funded by EPSRC grant                      */
/*  EP/K008528/1, at the Universities of Cambridge and Edinburgh.                        */
/*                                                                                       */
/*  This project has received funding from the European Research Council                 */
/*  (ERC) under the European Union’s Horizon 2020 research and innovation                */
/*  programme (grant agreement 789108, ELVER).                                           */
/*                                                                                       */
/*  Redistribution and use in source and binary forms, with or without                   */
/*  modification, are permitted provided that the following conditions                   */
/*  are met:                                                                             */
/*  1. Redistributions of source code must retain the above copyright                    */
/*     notice, this list of conditions and the following disclaimer.                     */
/*  2. Redistributions in binary form must reproduce the above copyright                 */
/*     notice, this list of conditions and the following disclaimer in                   */
/*     the documentation and/or other materials provided with the                        */
/*     distribution.                                                                     */
/*                                                                                       */
/*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''                   */
/*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED                    */
/*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                      */
/*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR                  */
/*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                         */
/*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                     */
/*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                     */
/*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND                  */
/*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,                   */
/*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT                   */
/*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF                   */
/*  SUCH DAMAGE.                                                                         */
/*=======================================================================================*/

/* CHERI specific helpers */

val MEMr_tag = "read_tag_bool"  : bits(64) -> bool
val MEMw_tag = "write_tag_bool" : (bits(64) , bool) -> unit

val MAX : forall 'n, 'n >= 0 . atom('n) -> atom(2 ^ 'n - 1)
function MAX(n) = pow2(n) - 1

/*!
 * align_down(n, bv) returns the given bit vector, bv,  aligned down to a power
 * of two by clearing the least significant n bits.
 */
val align_down : forall 'n 'm, 'n >= 1 & 'm > 'n . (int('n), bits('m)) -> bits('m)
function align_down(n, bv) = [bv with ('n - 1) .. 0 = zeros()]

/* width of capability in bytes (excluding tag) */
type cap_size : Int = 8
let  cap_size = sizeof(cap_size)
type log2_cap_size : Int = 3
let  log2_cap_size = sizeof(log2_cap_size)
type CapBits = bits(8 * cap_size)
/*! Width of compressed perms field in bits */
type cap_cperms_width : Int = 6

/*! Width of architectural otype field in bits */
type cap_otype_width : Int = 4
let  cap_otype_width = sizeof(cap_otype_width)
/*! Width of compressed otype field in bits */
type cap_cotype_width : Int = 3
/*! Width of T and B field in bits */
type cap_mantissa_width : Int = 9
let  cap_mantissa_width = sizeof(cap_mantissa_width)
/*! Width of expanded exponent field in bits */
type cap_E_width : Int = 5
let  cap_E_width = sizeof(cap_E_width)
/*! Width of compressed exponent field in bits */
type cap_cE_width : Int = 4
type cap_addr_width : Int = xlen
let  cap_addr_width = sizeof(cap_addr_width)
/*! Cap length width is one larger than address space width in order to 
 represent maximum top / length of 2**xlen. */
type cap_len_width : Int = cap_addr_width + 1
let  cap_len_width = sizeof(cap_len_width)

/*!
 * There is one revocation bit for every 8 bytes.
 */
type log2_revocation_granule_size : Int = 3
let  log2_revocation_granule_size = sizeof(log2_revocation_granule_size)

/*! THIS represents capabilities as stored in memory. */
struct EncCapability = {
  cmode      : bits(1),
  cperms     : bits(cap_cperms_width),
  cotype     : bits(cap_cotype_width),
  cE         : bits(cap_cE_width),
  T          : bits(cap_mantissa_width),
  B          : bits(cap_mantissa_width),
  address    : bits(cap_addr_width)
}

function capBitsToEncCapability(c) : CapBits -> EncCapability = struct {
  cmode      = c[63..63],
  cperms     = c[62..57],
  cotype     = c[56..54],
  cE         = c[53..50],
  T          = c[49..41],
  B          = c[40..32],
  address    = c[31..0]
}

function encCapToBits(cap) : EncCapability -> CapBits =
  cap.cmode @
  cap.cperms @
  cap.cotype @
  cap.cE @
  cap.T @
  cap.B @
  cap.address

let cap_max_addr = MAX(cap_addr_width)
let cap_max_otype = MAX(cap_otype_width)

/*! Width of the architectural perms returned by CGetPerms. */
type cap_perms_width : Int = 12
let  cap_perms_width = sizeof(cap_perms_width)

type CapAddrBits  = bits(cap_addr_width)
type CapAddrInt   = range(0, (2 ^ cap_addr_width) - 1)
type CapLenBits   = bits(cap_len_width)
type CapLen       = range(0, (2 ^ cap_len_width) - 1)
type CapPermsBits = bits(cap_perms_width)

/*! Exponent chosen to permit representing the entire address space. */
type cap_max_E : Int = 24
let cap_max_E = sizeof(cap_max_E)
let cap_max_E_bits = to_bits(cap_E_width, sizeof(cap_max_E))
/* Value for T field that represents max value of top (2**32) when used with cap_max_E. */
let cap_reset_T = 0b1 @ zeros(cap_mantissa_width - 1)

/*!
 * THIS represents a partially decompressed capability. The
 * permissions, E and otype are expanded from their compressed format by
 * [encCapabilityToCapability] and compressed by [capToEncCap].
 */
struct Capability = {
  tag                    : bool,
  perm_user0             : bool,
  permit_seal            : bool,
  permit_unseal          : bool,
  permit_execute         : bool,
  access_system_regs     : bool,
  permit_load_store_cap  : bool,
  permit_load            : bool,
  permit_store_local_cap : bool,
  permit_load_mutable    : bool,
  permit_store           : bool,
  permit_load_global     : bool,
  global                 : bool,
  cmode                  : bits(1),
  E                      : bits(cap_E_width),
  B                      : bits(cap_mantissa_width),
  T                      : bits(cap_mantissa_width),
  otype                  : bits(cap_otype_width),
  address                : bits(cap_addr_width)
}
