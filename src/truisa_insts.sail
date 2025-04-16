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

/*
 *  Descritptor Lookaside Buffer instruction
 */

/* ****************************************************************** */
union clause ast = TFENCE_VMA : (regidx, regidx)

mapping clause encdec = TFENCE_VMA(rs1, rs2)
  <-> 0b0001001 @ rs2 @ cs1 @ 0b000 @ 0b00000 @ 0b0001111

function clause execute TFENCE_VMA(rs1, rs2) = {
  let addr : option(xlenbits) = if rs1 == 0b00000 then None() else Some(X(rs1));
  let asid : option(xlenbits) = if rs2 == 0b00000 then None() else Some(X(rs2));
  flush_DLB(asid, addr); 
  RETIRE_SUCCESS
}

mapping clause assembly = TFENCE_VMA(rs1, rs2)
  <-> "tfence.vma" ^ spc() ^ reg_name(rs1) ^ sep() ^ reg_name(rs2)

/* ****************************************************************** */
union clause ast = TDLBRW : unit

mapping clause encdec = TDLBRW
  <-> 0b0001010 @ 0b00000 @0bcs1 @0b000 @0b00000 @0b0001111

function clause execute (TDLBRW) = {
  DLBWriteEntry(OLBFIFO);
}

function clause assembly (TDLBINSERT()) = "tdlbrw"

/* ****************************************************************** */
union clause ast = TSETDESCRIPTOR : (regidx, regidx, regidx)

mapping clause encdec = TSETDESCRIPTOR(cd, rs1, rs2)
  <-> 0b0000100 @ rs2 @ rs1 @ 0b011 @ cd @ 0b0001111

function clause execute (TSETDESCRIPTOR(cd, rs1, rs2)) = {
  let base : option(xlenbits) = Some(X(rs1));
  let bound : option(xlenbits) = Some(X(rs2));
  C(cd) = SetDescriptor(base, bound);
  RETIRE_SUCCESS
}

mapping clause assembly = TSETDESCRIPTOR(cd, rs1, rs2)
  <-> "tsetdescr" ^ spc() ^ cap_reg_name(cd) ^ sep() ^ reg_name(rs1) ^ sep() ^ reg_name(rs2)
