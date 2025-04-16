type descr_addr_width : Int = cap_max_E
let  descr_addr_width = sizeof(descr_addr_width)

type DescrAddrBits  = bits(descr_addr_width)
type DescrAddrInt   = range(0, (2 ^ descr_addr_width) - 1)

/*! THIS represents descriptor as stored in memory. */
struct Descriptor = {
  cmode      : bits(1),
  cperms     : bits(cap_cperms_width),
  cotype     : bits(cap_cotype_width),
  reserved1  : bits(1),
  E          : bits(cap_E_width),
  base       : bits(descr_addr_width),
  bound      : bits(descr_addr_width)
}

/*! THIS represents descriptor-mode capabilities as stored in memory. */
struct DescrCapability = {
  cmode      : bits(1),
  cperms     : bits(cap_cperms_width),
  cotype     : bits(cap_cotype_width),
  reserved1  : bits(1),
  E          : bits(cap_E_width),
  reserved16 : bits(16),
  address    : bits(cap_addr_width)
}

function capBitsToDescrCapability(c) : CapBits -> DescrCapability = struct {
  cmode      = c[63..63],
  cperms     = c[62..57],
  cotype     = c[56..54],
  reserved1  = c[53..53],
  E          = c[52..48],
  reserved16 = c[47..32],
  address    = c[31..0]
}

function descrCapToBits(cap) : DescrCapability -> CapBits =
  cap.cmode @
  cap.cperms @
  cap.cotype @
  cap.reserved1 @
  cap.E @
  cap.reserved16 @
  cap.address

/*! Partially decompress a capability from bits to a [Capability] struct.
  Permissions, otype and exponent are decompressed, but the bounds are left
  in the form of B and T fields. */
function descrCapabilityToCapability(t,c) : (bool, DescrCapability) -> Capability = {
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
    E                      = c.E,
    B                      = zeros(),
    T                      = zeros(),
    otype                  = otype,
    address                = c.address
  }
}

function isDescrCapability(cap) : Capability -> bool = {
  bit_to_bool(c.mode)
}

function calcDescrE(base, bound) : DescrAddrBits, DescrAddrBits -> bits(cap_E_width) = {
  let E : range(0, 24) = 24 - count_leading_zeros( base ^ bound );
  to_bits(cap_E_width, E)
}

fucntion calCentroid(e, a) : (bits(cap_E_width), CapAddrBits) -> CapAddrBits = {
  let mask : CapAddrBits = tobits((0b1 << e) - 1);
  let centroid : CapAddrBits = ( a & mask ) | ( 1 << (e-1) );
  centroid
}

function calcCapCentroid(cap) : DescrCapbility -> xlenbits = {
  let e = cap.E;
  let a : CapAddrBits = cap.address;
  calCentroid(e, a)
}

function calcDescrCentroid(descr) : Descriptor -> xlenbits = {
  let a : CapAddrBits = descr.base;
  let e = calcDescrE(descr.base, descr.bound);
  calCentroid(e, a)
}

val getDescriptor(e, a) : (bits(cap_E_width), CapAddrBits) -> (bool, Descriptor)
function getDescriptor(input_desc) = {
  let desc_mddlbc = MDDLBC;
  let desc_mdescrc = MDESCRC;

  let match_mddlbc = 
    calcCentroid(e, a) == calcDescrCentroid(descr_mddlbc);

  let match_mdescrc = 
    calcCentroid(e, a) == calcDescrCentroid(decrc_mdescrc);

  if match_mddlbc then
    (true, desc_mddlbc)       // 返回MDDLBC寄存器中的描述符
  else if match_mdescrc then
    (true, desc_mdescrc)      // 返回MDESCRC寄存器中的描述符
  else
    (false, zeros())       // 无匹配时返回NULL描述符
}

/*!
 * Returns the decoded base and top in descriptor of the given Capability.
 */
function getDescrCapBoundsBits(c) : Capability -> (bool, CapAddrBits, CapLenBits) = {
  let E = unsigned(c.E);
  let a : CapAddrBits = c.address;
  let (descrmiss: bool, d : Descriptor) = getDescriptor(E, a);
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