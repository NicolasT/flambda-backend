(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* CR mshinwell: This file needs to be code reviewed *)

module String = Misc.Stdlib.String

let log_verbose = Dissector_log.log_verbose

(* Each IGOT entry is 8 bytes (one 64-bit address) *)
let entry_size = 8

(* Delimiter for synthetic symbol names - unlikely to appear in normal
   symbols *)
let delimiter = "\xf0\x9f\x90\x8d" (* Unicode snake emoji U+1F40D in UTF-8 *)

module Entry = struct
  type t =
    { index : int;
      original_symbol : string;
      igot_symbol : string
    }

  let index e = e.index

  let original_symbol e = e.original_symbol

  let igot_symbol e = e.igot_symbol

  let offset e = e.index * entry_size
end

type t =
  { entries : Entry.t list;
    num_entries : int;
    section_data : bytes
  }

let igot_symbol_name ~prefix ~symbol =
  "igot" ^ delimiter ^ prefix ^ delimiter ^ symbol

let build ~prefix ~plt_symbols ~got_only_symbols =
  (* Iterate plt_symbols then got_only_symbols, deduplicating across both.
     Avoids allocating a concatenated input list. *)
  let seen = String.Tbl.create 256 in
  let index = ref 0 in
  let acc = ref [] in
  let add original_symbol =
    if not (String.Tbl.mem seen original_symbol)
    then begin
      String.Tbl.add seen original_symbol ();
      let i = !index in
      incr index;
      let igot_symbol = igot_symbol_name ~prefix ~symbol:original_symbol in
      log_verbose "  IGOT entry %d: %s -> %s" i original_symbol igot_symbol;
      acc := { Entry.index = i; original_symbol; igot_symbol } :: !acc
    end
  in
  List.iter add plt_symbols;
  List.iter add got_only_symbols;
  let entries = List.rev !acc in
  let num_entries = !index in
  (* Section data is zero-initialized *)
  let section_data = Bytes.make (num_entries * entry_size) '\x00' in
  { entries; num_entries; section_data }

let entries t = t.entries

let num_entries t = t.num_entries

let section_data t = t.section_data

let section_size t = Bytes.length t.section_data
