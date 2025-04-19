/*=======================================================================================*/
/*  TRUISA RISCV Sail Model                                                              */
/*                                                                                       */
/*  This Sail TRUISA RISC-V architecture model, comprising all files and                 */
/*  directories except where otherwise noted is subject the BSD                          */
/*  two-clause license in the LICENSE file.                                              */
/*                                                                                       */
/*  SPDX-License-Identifier: BSD-2-Clause                                                */
/*                                                                                       */
/*  Copyright (c) 2019-2025                                                              */
/*    Dong Tong, Peking University                                                       */
/*                                                                                       */
/*  All rights reserved.                                                                 */
/*=======================================================================================*/

type descr_addr_width : Int = 24
let  descr_addr_width = sizeof(descr_addr_width)

type DescrAddrBits  = bits(descr_addr_width)
type DescrAddrInt   = range(0, (2 ^ descr_addr_width) - 1)

/*! THIS represents descriptor-mode capabilities as stored in memory. */
struct EncDescrCapability = {
  cmode      : bits(1), /* 0, EncCapability; 1, EncDescrCapability */
  cperms     : bits(cap_cperms_width),
  cotype     : bits(cap_cotype_width),
  dmode      : bits(1), /* 0, EnDescrCapability; 1, EncDescriptor */
  E          : bits(cap_E_width),
  reserved16 : bits(16),
  address    : bits(cap_addr_width)
}

/*! THIS represents descriptor as stored in memory. */
struct EncDescriptor = {
  cmode      : bits(1), /* 0, EncCapability; 1, EncDescrCapability */
  cperms     : bits(cap_cperms_width),
  cotype     : bits(cap_cotype_width),
  dmode      : bits(1), /* 0, EnDescrCapability; 1, EncDescriptor */
  E          : bits(cap_E_width),
  base       : bits(descr_addr_width),
  bound      : bits(descr_addr_width)
}

register MDESCRC   : Capability
register MDDLBC    : Capability

struct Descriptor = {
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
  dmode                  : bits(1),
  otype                  : bits(cap_otype_width),
  E                      : bits(cap_E_width),
  base                   : bits(descr_addr_width),
  bound                  : bits(descr_addr_width)
}

function capToDescriptor(cap) : Capability -> Descriptor = {
  let temp : bits(50) = cap.T @ cap.B @ cap.address;
  return struct {
    tag                    = cap.tag,
    perm_user0             = cap.perm_user0            ,
    permit_seal            = cap.permit_seal           ,
    permit_unseal          = cap.permit_unseal         ,
    permit_execute         = cap.permit_execute        ,
    access_system_regs     = cap.access_system_regs    ,
    permit_load_store_cap  = cap.permit_load_store_cap ,
    permit_load            = cap.permit_load           ,
    permit_store_local_cap = cap.permit_store_local_cap,
    permit_load_mutable    = cap.permit_load_mutable   ,
    permit_store           = cap.permit_store          ,
    permit_load_global     = cap.permit_load_global    ,
    global                 = cap.global                ,
    cmode                  = cap.cmode,
    dmode                  = cap.dmode,
    otype                  = cap.otype,
    E                      = cap.E,
    base                   = temp[47..24],
    bound                  = temp[23..0]
  }
}

/* The null descriptor is defined as all zeros. */
let null_descr : Descriptor = struct {
  tag                    = false,
  perm_user0             = false,
  permit_seal            = false,
  permit_unseal          = false,
  permit_execute         = false,
  access_system_regs     = false,
  permit_load_store_cap  = false,
  permit_load            = false,
  permit_store_local_cap = false,
  permit_load_mutable    = false,
  permit_store           = false,
  permit_load_global     = false,
  global                 = false,
  cmode                  = zeros(),
  dmode                  = ones(),
  E                      = zeros(),
  otype                  = to_bits(cap_otype_width, otype_unsealed),
  base                   = zeros(),
  bound                  = zeros()
}

function capBitsToEncDescrCapability(c) : CapBits -> EncDescrCapability = struct {
  cmode      = c[63..63],
  cperms     = c[62..57],
  cotype     = c[56..54],
  dmode      = c[53..53],
  E          = c[52..48],
  reserved16 = c[47..32],
  address    = c[31..0]
}

function encDescrCapToBits(cap) : EncDescrCapability -> CapBits =
  cap.cmode @
  cap.cperms @
  cap.cotype @
  cap.dmode @
  cap.E @
  cap.reserved16 @
  cap.address

/*! Partially decompress a capability from bits to a [Capability] struct.
  Permissions, otype and exponent are decompressed, but the bounds are left
  in the form of B and T fields. */
function encDescrCapabilityToCapability(t,c) : (bool, EncDescrCapability) -> Capability = {
  var perm_user0             : bool = false;
  var permit_seal            : bool = false;
  var permit_unseal          : bool = false;
  var permit_execute         : bool = false;
  var access_system_regs     : bool = false;
  var permit_load_store_cap  : bool = false;
  var permit_load            : bool = false;
  var permit_store_local_cap : bool = false;
  var permit_load_mutable    : bool = false;
  var permit_store           : bool = false;
  var permit_load_global     : bool = false;
  var global                 : bool = bit_to_bool(c.cperms[5]);
  var isExe : bool = false;
  match c.cperms[4..0] {
    0b11 @ [SL, LM, LG] => {
      /* mem-rw-cap format */
      permit_load = true;
      permit_load_store_cap = true;
      permit_store = true;
      permit_store_local_cap = bit_to_bool(SL);
      permit_load_mutable = bit_to_bool(LM);
      permit_load_global = bit_to_bool(LG);
    },
    0b101 @ [LM, LG] => {
      /* mem-ro-cap format */
      permit_load = true;
      permit_load_store_cap = true;
      permit_load_mutable = bit_to_bool(LM);
      permit_load_global = bit_to_bool(LG);
    },
    0b10000 => {
      /* mem-wo-cap */
      permit_store = true;
      permit_load_store_cap = true;
    },
    0b100 @ [LD, SD] => {
      /* mem-data */
      permit_load = bit_to_bool(LD);
      permit_store = bit_to_bool(SD);
    },
    0b01 @ [SR, LM, LG] => {
      /* Executable format */
      isExe = true;
      permit_execute = true;
      permit_load = true;
      permit_load_store_cap = true;
      access_system_regs  = bit_to_bool(SR);
      permit_load_mutable = bit_to_bool(LM);
      permit_load_global  = bit_to_bool(LG);
    },
    0b00 @ [U0, SE, US] => {
      /* Sealing format */
      perm_user0    = bit_to_bool(U0);
      permit_seal   = bit_to_bool(SE);
      permit_unseal = bit_to_bool(US);
    }
  };
  /* The otype of executable caps is mapped to 1-7 and others to 9-15. Unsealed
    is always 0. */
  let otype = (if isExe | c.cotype == 0b000 then 0b0 else 0b1) @ c.cotype;
  return struct {
    tag                    = t,
    perm_user0             = perm_user0            ,
    permit_seal            = permit_seal           ,
    permit_unseal          = permit_unseal         ,
    permit_execute         = permit_execute        ,
    access_system_regs     = access_system_regs    ,
    permit_load_store_cap  = permit_load_store_cap ,
    permit_load            = permit_load           ,
    permit_store_local_cap = permit_store_local_cap,
    permit_load_mutable    = permit_load_mutable   ,
    permit_store           = permit_store          ,
    permit_load_global     = permit_load_global    ,
    global                 = global                ,
    cmode                  = c.cmode,
    dmode                  = zeros(),
    E                      = c.E,
    B                      = zeros(),
    T                      = zeros(),
    otype                  = otype,
    address                = c.address
  }
}

function isDescrCapability(cap) : Capability -> bool = {
  bit_to_bool(cap.cmode[0])
}

function calcDescrE(base, bound) : (DescrAddrBits, DescrAddrBits) -> bits(cap_E_width) = {
  let E : range(0, 24) = 24 - count_leading_zeros( base ^ bound );
  to_bits(cap_E_width, E)
}

function calcCentroid(expo, addr) : (bits(cap_E_width), CapAddrBits) -> CapAddrBits = {
  var e = unsigned(expo);
  let mask : CapAddrBits = ones(sizeof(cap_addr_width)) << e;
  let tail_len : CapLenBits = (to_bits(sizeof(cap_len_width), 1) << e) >> 1;
  let centroid : CapAddrBits = addr & mask | tail_len[cap_addr_width - 1 .. 0 ];
  centroid
}

function calcCapCentroid(cap) : Capability -> CapAddrBits = {
  let e = cap.E;
  let a : CapAddrBits = cap.address;
  calcCentroid(e, a)
}

function calcDescrCentroid(descr) : Descriptor -> CapAddrBits = {
  let a : CapAddrBits = sign_extend(descr.base);
  let e = calcDescrE(descr.base, descr.bound);
  calcCentroid(e, a)
}

function getDescriptor(expon, addr) : (bits(cap_E_width), CapAddrBits) -> (bool, Descriptor) = {
  let descr_mddlbc: Descriptor = capToDescriptor(MDDLBC);
  let descr_mdescrc : Descriptor = capToDescriptor(MDESCRC);

  if calcCentroid(expon, addr) == calcDescrCentroid(descr_mddlbc) then
    (true, descr_mddlbc)       // 返回MDDLBC寄存器中的描述符
  else if calcCentroid(expon, addr) == calcDescrCentroid(descr_mdescrc) then
    (true, descr_mdescrc)      // 返回MDESCRC寄存器中的描述符
  else
    (false, null_descr)       // 无匹配时返回NULL描述符
}

/*!
 * Returns the decoded base and top in descriptor of the given Capability.
 */
function getDescrCapBoundsBits(c) : Capability -> (bool, CapAddrBits, CapLenBits) = {
  let (descrmiss: bool, d : Descriptor) = getDescriptor(c.E, c.address);
  let base : CapAddrBits = sign_extend(d.base);
  let top : CapLenBits  = sign_extend((0b0 @ d.bound) + 1);
  (descrmiss, base, top)
}

function getDescrBounds(cap) : Capability -> (bool, CapAddrInt, CapLen) =
  let (descrmiss: bool, base : CapAddrBits, top : CapLenBits) = getDescrCapBoundsBits(cap) in
  (descrmiss, unsigned(base), unsigned(top))

function hasValidDescriptor(cap) : Capability -> bool =
  let (descrmiss: bool, _, _ ) = getDescrCapBoundsBits(cap) in
  descrmiss