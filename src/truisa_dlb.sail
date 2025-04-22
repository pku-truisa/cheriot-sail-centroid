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

/* idealized generic DLB entry to model fence.vm and also speed up simulation. */

val match_DLB_Entry : (bits(cap_E_width), CapAddrBits, Descriptor) > bool
function match_DLBEntry(expon, addr, entry) =
  let centroidIn  = calcCentroid(expon, addr) in
  let centroidEntry = calcDescrCentroid(entry) in
  ( 
    centroidIn == CentroidEntry
  )

val lookup_DLB : (bits(cap_E_width), CapAddrBits) -> option(Descriptor)
function lookup_DLB(expon, addr) = {
  foreach (idx from 0 to DLBNumEntries - 1) {
    if(match_DLB_Entry(expoin, addr, *DLBEntries[idx])) then
      return Some(to_bits(DLB_index_width, idx))
    };
    None()
}

val flush_DLB : (bits(1), bits(5), CapAddrBits) -> unit
function flush_DLB(full, expon, addr) = {
  if full then {
    foreach (idx from 0 to DLB_num_entries - 1) {
      DLBEntries[idx] = None()
    }
  } else {
    let idx = lookup_DLB(expon, addr)
    match (idx) {
      None() => (),
      Some(i) => {
        Match(DLBEntries[i]) {
          None() => (),
          Some(e) => e = None()
        }
      }
    }
  }
}

function write_DLB(idx : DLB_num_entries, ent : Descritpor) -> unit =
  DLBEntries[idx] = Some(ent)