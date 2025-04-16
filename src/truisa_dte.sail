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

/* TRUISA specific helpers */

let DLBNumEntries = 8
type DLBIndexT = (bits(3))
let DLBIndexMax : DLBIndexT = 0b111

struct DLBEntry = {
cmode      : bits(1),
cperms     : bits(cap_cperms_width),
cotype     : bits(cap_cotype_width),
reserved1  : bits(1),
E          : bits(cap_E_width),
base       : bits(24),
bound      : bits(24)
}

register DLBIndex : DLBIndexT
register DLBFIFO : DLBIndexT

reigster DLBEntry00 ： DLBEntry
reigster DLBEntry01 ： DLBEntry
reigster DLBEntry02 ： DLBEntry
reigster DLBEntry03 ： DLBEntry
reigster DLBEntry04 ： DLBEntry
reigster DLBEntry05 ： DLBEntry
reigster DLBEntry06 ： DLBEntry
reigster DLBEntry07 ： DLBEntry

let DLBEntries : vector(DLBNumEntries, dec, register(DLBEntry)) = {
    ref DLBEntry07,
    ref DLBEntry06,
    ref DLBEntry05,
    ref DLBEntry04,
    ref DLBEntry03,
    ref DLBEntry02,
    ref DLBEntry01,
    ref DLBEntry00
}