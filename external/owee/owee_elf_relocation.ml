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

(* Section index constants *)
let shn_undef = 0
let shn_loreserve = 0xff00
let shn_xindex = 0xffff

module Section_index = struct
  type t = int

  let of_int t = t
  let to_int t = t

  let undef = shn_undef
  let xindex = shn_xindex

  let is_undef t = t = shn_undef
  let is_defined t = t <> shn_undef
  let needs_extended t = t >= shn_loreserve
end

(* x86-64 relocation types *)
module Reloc_type = struct
  (* Using int rather than int64: relocation types are ELF Elf64_Word values
     (32-bit), so they always fit in an OCaml int.  This avoids boxing. *)
  type t = int

  let equal (a : int) (b : int) = a = b
  let to_int t = t
  let of_int t = t
  let to_int64 t = Int64.of_int t
  let of_int64 t = Int64.to_int t

  let plt32 = 4
  let rex_gotpcrelx = 42
  let r64 = 1
  let pc32 = 2

  let name t =
    if t = plt32 then "PLT32"
    else if t = rex_gotpcrelx then "REX_GOTPCRELX"
    else if t = pc32 then "PC32"
    else if t = r64 then "64"
    else Printf.sprintf "type=%d" t
end

(* Size of an Elf64_Rela entry in bytes *)
let rela_entry_size = 24

(* Size of an Elf64_Sym entry in bytes *)
let sym_entry_size = 24

type rela_entry =
  { r_offset : int64;
    r_sym : int;
    r_type : Reloc_type.t;
    r_addend : int64
  }

(* Extract symbol index from r_info (upper 32 bits).
   Uses Int64.to_int + native lsr to avoid allocating an intermediate int64.
   Safe on 64-bit platforms (required by the dissector) where OCaml int is 63
   bits so the full 64-bit r_info fits after Int64.to_int. *)
let r_sym_of_info r_info = Int64.to_int r_info lsr 32

(* Extract relocation type from r_info (lower 32 bits).
   Uses Int64.to_int + native land to avoid allocating an intermediate int64. *)
let r_type_of_info r_info = Int64.to_int r_info land 0xFFFFFFFF

let iter_rela_entries ~rela_body ~f =
  let n = Owee_buf.size rela_body in
  if n mod rela_entry_size <> 0
  then
    Owee_buf.invalid_formatf
      "RELA section size %d is not a multiple of entry size %d" n
      rela_entry_size;
  let num_entries = n / rela_entry_size in
  (* Read fields directly from the bigarray without cursor or int64 boxing.
     Each RELA entry is 24 bytes: r_offset (0-7), r_info (8-15), r_addend
     (16-23). All fields in little-endian byte order.

     r_offset is returned as a native int: safe on 64-bit platforms (required
     by the dissector) since section offsets are well under 2^62.

     r_info is split into r_type (lower 32 bits) and r_sym (upper 32 bits),
     both returned as native ints. r_addend is not passed to [f] since the
     dissector never needs it. *)
  for i = 0 to num_entries - 1 do
    let p = i * rela_entry_size in
    let r_offset =
      Bigarray.Array1.unsafe_get rela_body p
      lor (Bigarray.Array1.unsafe_get rela_body (p + 1) lsl 8)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 2) lsl 16)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 3) lsl 24)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 4) lsl 32)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 5) lsl 40)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 6) lsl 48)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 7) lsl 56)
    in
    let r_type =
      Bigarray.Array1.unsafe_get rela_body (p + 8)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 9) lsl 8)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 10) lsl 16)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 11) lsl 24)
    in
    let r_sym =
      Bigarray.Array1.unsafe_get rela_body (p + 12)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 13) lsl 8)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 14) lsl 16)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 15) lsl 24)
    in
    (* r_addend: bytes 16-23 little-endian. Read as native int; addends are
       small (typically 0 or -4) and fit in 63 bits on 64-bit platforms. *)
    let r_addend =
      Bigarray.Array1.unsafe_get rela_body (p + 16)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 17) lsl 8)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 18) lsl 16)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 19) lsl 24)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 20) lsl 32)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 21) lsl 40)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 22) lsl 48)
      lor (Bigarray.Array1.unsafe_get rela_body (p + 23) lsl 56)
    in
    f ~r_offset ~r_sym ~r_type ~r_addend
  done

(* Elf64_Sym layout:
   st_name  (4 bytes, offset 0)  - index into string table
   st_info  (1 byte,  offset 4)  - type and binding
   st_other (1 byte,  offset 5)  - visibility
   st_shndx (2 bytes, offset 6)  - section header index
   st_value (8 bytes, offset 8)  - value
   st_size  (8 bytes, offset 16) - size *)

(* Reads the name of a symbol only if it is undefined (st_shndx = SHN_UNDEF).
   Combines the shndx check and name lookup in a single symbol entry access,
   avoiding two separate passes over the same symbol entry.
   Returns [None] if the symbol is defined, the index is out of bounds, or the
   name cannot be read. *)
let read_undef_symbol_name ~symtab_body ~strtab_body ~sym_index =
  let sym_offset = sym_index * sym_entry_size in
  if sym_offset + sym_entry_size > Owee_buf.size symtab_body
  then None
  else
    (* st_shndx is at offset 6 within the symbol entry, 2 bytes LE *)
    let shndx =
      Bigarray.Array1.unsafe_get symtab_body (sym_offset + 6) lor
      (Bigarray.Array1.unsafe_get symtab_body (sym_offset + 7) lsl 8)
    in
    if shndx <> shn_undef
    then None
    else
      (* st_name is at offset 0 within the symbol entry, 4 bytes LE *)
      let st_name =
        Bigarray.Array1.unsafe_get symtab_body sym_offset lor
        (Bigarray.Array1.unsafe_get symtab_body (sym_offset + 1) lsl 8) lor
        (Bigarray.Array1.unsafe_get symtab_body (sym_offset + 2) lsl 16) lor
        (Bigarray.Array1.unsafe_get symtab_body (sym_offset + 3) lsl 24)
      in
      if st_name >= Owee_buf.size strtab_body
      then None
      else
        let cursor = Owee_buf.cursor strtab_body ~at:st_name in
        Owee_buf.Read.zero_string cursor ()

let read_symbol_name ~symtab_body ~strtab_body ~sym_index =
  let sym_offset = sym_index * sym_entry_size in
  if sym_offset >= Owee_buf.size symtab_body
  then None
  else
    let cursor = Owee_buf.cursor symtab_body ~at:sym_offset in
    let st_name = Owee_buf.Read.u32 cursor in
    (* Read null-terminated string from strtab *)
    if st_name >= Owee_buf.size strtab_body
    then None
    else
      let cursor = Owee_buf.cursor strtab_body ~at:st_name in
      Owee_buf.Read.zero_string cursor ()

let read_symbol_shndx ~symtab_body ~sym_index =
  let sym_offset = sym_index * sym_entry_size in
  if sym_offset + sym_entry_size > Owee_buf.size symtab_body
  then None
  else
    (* st_shndx is at offset 6 within the symbol entry, 2 bytes little-endian.
       Read directly from the bigarray to avoid cursor record allocation. *)
    let p = sym_offset + 6 in
    Some (Section_index.of_int
      (Bigarray.Array1.unsafe_get symtab_body p lor
       (Bigarray.Array1.unsafe_get symtab_body (p + 1) lsl 8)))

(* Construct r_info from symbol index and relocation type.
   Safe on 64-bit platforms (required by the dissector) where OCaml int is 63
   bits: sym is a 32-bit ELF symbol index so sym < 2^31, meaning sym lsl 32
   fits in 63 bits. Saves three int64 allocations versus the naive version. *)
let make_r_info ~sym ~typ =
  Int64.of_int ((sym lsl 32) lor typ)

let write_rela_entry ~cursor entry =
  Owee_buf.Write.u64 cursor entry.r_offset;
  Owee_buf.Write.u64 cursor (make_r_info ~sym:entry.r_sym ~typ:entry.r_type);
  Owee_buf.Write.u64 cursor entry.r_addend

(* Symbol binding attributes *)
module Symbol_binding = struct
  type t = int

  let to_int t = t

  let local = 0
  let global = 1
  let weak = 2
end

(* Symbol type attributes *)
module Symbol_type = struct
  type t = int

  let to_int t = t

  let notype = 0
  let object_ = 1
  let func = 2
  let section = 3
  let file = 4
end

(* Symbol visibility attributes (stored in st_other) *)
module Symbol_visibility = struct
  type t = int

  let to_int t = t

  let default = 0
  let internal = 1
  let hidden = 2
  let protected = 3
end

let make_st_info ~binding ~typ =
  (Symbol_binding.to_int binding lsl 4) lor Symbol_type.to_int typ

type sym_entry =
  { st_name : int;
    st_info : int;
    st_other : int;
    st_shndx : int;
    st_value : int64;
    st_size : int64
  }

let write_sym_entry ~cursor entry =
  Owee_buf.Write.u32 cursor entry.st_name;
  Owee_buf.Write.u8 cursor entry.st_info;
  Owee_buf.Write.u8 cursor entry.st_other;
  Owee_buf.Write.u16 cursor entry.st_shndx;
  Owee_buf.Write.u64 cursor entry.st_value;
  Owee_buf.Write.u64 cursor entry.st_size
