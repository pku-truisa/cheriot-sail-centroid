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

let DLB_num_entries = 8
type DLB_index_width = 3
let DLB_index_width = sizeof(DLB_index_width)
let DLBIndexMax : DLB_index_width = 0b111

type DLBIndexT = bits(DLB_index_width)

register DLBIndex : DLBIndexT
register DLBFIFO : DLBIndexT

reigster DLBEntry00 ：Descriptor
reigster DLBEntry01 ：Descriptor
reigster DLBEntry02 ：Descriptor
reigster DLBEntry03 ：Descriptor
reigster DLBEntry04 ：Descriptor
reigster DLBEntry05 ：Descriptor
reigster DLBEntry06 ：Descriptor
reigster DLBEntry07 ：Descriptor

let DLBEntries : vector(DLBNumEntries, dec, register(Descriptor)) = {
    ref DLBEntry07,
    ref DLBEntry06,
    ref DLBEntry05,
    ref DLBEntry04,
    ref DLBEntry03,
    ref DLBEntry02,
    ref DLBEntry01,
    ref DLBEntry00
}