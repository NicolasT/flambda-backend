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

module Elf = Compiler_owee.Owee_elf
module Rela = Compiler_owee.Owee_elf_relocation
module Strtab = Compiler_owee.Owee_elf_string_table
module Buf = Compiler_owee.Owee_buf
module String = Misc.Stdlib.String

let log_verbose = Dissector_log.log_verbose

let align_up64 value alignment =
  let alignment = Int64.of_int alignment in
  let mask = Int64.sub alignment 1L in
  Int64.logand (Int64.add value mask) (Int64.lognot mask)

module Section_layout = struct
  type t =
    { offset : int64;
      size : int64
    }

  let offset l = l.offset

  let size l = l.size
end

module Layout = struct
  type t =
    { igot : Section_layout.t;
      rela_igot : Section_layout.t;
      iplt : Section_layout.t;
      rela_iplt : Section_layout.t;
      symtab : Section_layout.t;
      symtab_shndx : Section_layout.t option;
      strtab : Section_layout.t;
      shstrtab : Section_layout.t;
      section_headers_offset : int64;
      total_size : int64
    }

  let igot l = l.igot

  let rela_igot l = l.rela_igot

  let iplt l = l.iplt

  let rela_iplt l = l.rela_iplt

  let symtab l = l.symtab

  let symtab_shndx l = l.symtab_shndx

  let strtab l = l.strtab

  let shstrtab l = l.shstrtab

  let section_headers_offset l = l.section_headers_offset

  let total_size l = l.total_size
end

(* Rewritten relocation entries for a single .rela.text* section *)
module Rewritten_rela_section = struct
  type t =
    { section_offset : int64; (* Original file offset of this section *)
      entries : Rela.rela_entry array
    }

  let section_offset t = t.section_offset

  let entries t = t.entries
end

type t =
  { num_original_symbols : int;
    (* Pre-computed st_name offsets for synthetic symbols in the output strtab.
       These are absolute offsets (relative to start of the full output strtab),
       computed as original_strtab_size + position in synthetic_strtab_data. *)
    igot_st_names : int array;
    iplt_st_names : int array;
    (* Raw bytes to append after the original strtab to form the output
       strtab *)
    synthetic_strtab_data : bytes;
    (* For IGOT RELA writing: igot_orig_sym_indices.(i) is the output symtab
       index of the original symbol for IGOT entry i (0 = not found). *)
    igot_orig_sym_indices : int array;
    (* For IPLT RELA writing: iplt_igot_sym_indices.(j) is the output symtab
       index of the IGOT synthetic symbol for IPLT entry j. *)
    iplt_igot_sym_indices : int array;
    total_symbols : int;
    rewritten_rela_sections : Rewritten_rela_section.t list;
    shstrtab : Strtab.t;
    section_name_offsets : (int * string) String.Tbl.t;
    igot_name_offset : int;
    igot_name_str : string;
    rela_igot_name_offset : int;
    rela_igot_name_str : string;
    iplt_name_offset : int;
    iplt_name_str : string;
    rela_iplt_name_offset : int;
    rela_iplt_name_str : string;
    igot_idx : int;
    rela_igot_idx : int;
    iplt_idx : int;
    rela_iplt_idx : int;
    num_sections : int;
    symtab_idx : int;
    symtab_shndx_idx : int option;
    new_symtab_shndx_idx : int option;
    symtab_shndx_name_offset : int option;
    (* File offset of the end of the original section data — the point at which
       new sections begin. Pre-computed and stored to avoid recomputing in
       execute_plan. *)
    original_data_end : int64;
    layout : Layout.t
  }

let num_original_symbols t = t.num_original_symbols

let igot_st_names t = t.igot_st_names

let iplt_st_names t = t.iplt_st_names

let synthetic_strtab_data t = t.synthetic_strtab_data

let igot_orig_sym_indices t = t.igot_orig_sym_indices

let iplt_igot_sym_indices t = t.iplt_igot_sym_indices

let total_symbols t = t.total_symbols

let rewritten_rela_sections t = t.rewritten_rela_sections

let shstrtab t = t.shstrtab

let section_name_offsets t = t.section_name_offsets

let igot_name_offset t = t.igot_name_offset

let igot_name_str t = t.igot_name_str

let rela_igot_name_offset t = t.rela_igot_name_offset

let rela_igot_name_str t = t.rela_igot_name_str

let iplt_name_offset t = t.iplt_name_offset

let iplt_name_str t = t.iplt_name_str

let rela_iplt_name_offset t = t.rela_iplt_name_offset

let rela_iplt_name_str t = t.rela_iplt_name_str

let igot_idx t = t.igot_idx

let rela_igot_idx t = t.rela_igot_idx

let iplt_idx t = t.iplt_idx

let rela_iplt_idx t = t.rela_iplt_idx

let num_sections t = t.num_sections

let symtab_idx t = t.symtab_idx

let symtab_shndx_idx t = t.symtab_shndx_idx

let new_symtab_shndx_idx t = t.new_symtab_shndx_idx

let symtab_shndx_name_offset t = t.symtab_shndx_name_offset

let original_data_end t = t.original_data_end

let layout t = t.layout

(* DJB2a hash of bytes strtab_body[offset..null-terminator). Reads directly from
   the bigarray without allocating an OCaml string. *)
let djb2a_strtab strtab_body offset =
  let n = Buf.size strtab_body in
  let i = ref offset in
  let h = ref 5381 in
  while !i < n && Bigarray.Array1.unsafe_get strtab_body !i <> 0 do
    h := !h * 33 lxor Bigarray.Array1.unsafe_get strtab_body !i;
    incr i
  done;
  !h

(* DJB2a hash of an OCaml string. Produces the same value as [djb2a_strtab] for
   identical byte sequences. *)
let djb2a_string s =
  let n = String.length s in
  let h = ref 5381 in
  for i = 0 to n - 1 do
    h := !h * 33 lxor Char.code (String.unsafe_get s i)
  done;
  !h

(* True iff strtab_body[offset .. offset+len-1] == str and the next byte is 0.
   No allocation. Returns false if offset + String.length str >= Buf.size. *)
let strtab_name_equals strtab_body offset str =
  let slen = String.length str in
  if offset + slen >= Buf.size strtab_body
  then false
  else begin
    let i = ref 0 in
    while
      !i < slen
      && Bigarray.Array1.unsafe_get strtab_body (offset + !i)
         = Char.code (String.unsafe_get str !i)
    do
      incr i
    done;
    !i = slen && Bigarray.Array1.unsafe_get strtab_body (offset + slen) = 0
  end

(* Build all symbol maps needed for rewriting. Returns: - plt_index_map,
   got_index_map: int→int for rewrite_rela_section hot loop -
   igot_orig_sym_indices.(i): output symtab index of the original symbol for
   IGOT entry i (found via symtab scan; 0 = not found) -
   iplt_igot_sym_indices.(j): output symtab index of the IGOT synthetic symbol
   for IPLT entry j (= igot_base + igot_entry_idx; precomputed, no scan needed)
   - igot_st_names, iplt_st_names: absolute st_name offsets for synthetic
   symbols - synthetic_strtab_data: raw bytes to append to the original strtab

   No OCaml strings are allocated in the symtab scan hot loop: the K IGOT target
   names are hashed once up-front; each of the N symtab entries is matched by
   hashing its bytes directly from the strtab bigarray (no string created), with
   a byte-comparison fallback only on the rare hash-match case. *)
let build_all_symbol_maps ~symtab_body ~strtab_body ~igot_and_iplt ~num_original
    =
  let igot = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt = Build_igot_and_iplt.iplt igot_and_iplt in
  let igot_entries = Igot.entries igot in
  let iplt_entries = Iplt.entries iplt in
  let num_igot = Igot.num_entries igot in
  let num_iplt = Iplt.num_entries iplt in
  let igot_base = num_original in
  let iplt_base = num_original + num_igot in
  let original_strtab_size = Buf.size strtab_body in
  (* Hash table: djb2a(orig_name) -> list of (igot_entry_idx, orig_name_str). A
     list per bucket handles the rare case of hash collisions. The name strings
     are shared with IGOT entries — no extra allocation. *)
  let name_hash_to_igot : (int, (int * string) list) Hashtbl.t =
    Hashtbl.create (2 * num_igot)
  in
  List.iteri
    (fun i entry ->
      let orig_name = Igot.Entry.original_symbol entry in
      let h = djb2a_string orig_name in
      let existing =
        Option.value ~default:[] (Hashtbl.find_opt name_hash_to_igot h)
      in
      Hashtbl.replace name_hash_to_igot h ((i, orig_name) :: existing))
    igot_entries;
  (* Map original name -> IGOT entry index (used to build igot_to_iplt_idx and
     iplt_igot_sym_indices below; local to this function). *)
  let orig_name_to_igot_idx : int String.Tbl.t = String.Tbl.create num_igot in
  List.iteri
    (fun i entry ->
      String.Tbl.add orig_name_to_igot_idx (Igot.Entry.original_symbol entry) i)
    igot_entries;
  (* For each IGOT entry i: the index of the corresponding IPLT entry, or -1. *)
  let igot_to_iplt_idx = Array.make num_igot (-1) in
  List.iteri
    (fun j entry ->
      let orig_name = Iplt.Entry.original_symbol entry in
      match String.Tbl.find_opt orig_name_to_igot_idx orig_name with
      | Some igot_i ->
        if igot_to_iplt_idx.(igot_i) = -1 then igot_to_iplt_idx.(igot_i) <- j
      | None -> ())
    iplt_entries;
  (* iplt_igot_sym_indices.(j) = output symtab index of the IGOT synthetic
     symbol for IPLT entry j. Fully determined by the IGOT/IPLT structures — no
     scan. *)
  let iplt_igot_sym_indices = Array.make num_iplt 0 in
  List.iteri
    (fun j entry ->
      let orig_name = Iplt.Entry.original_symbol entry in
      match String.Tbl.find_opt orig_name_to_igot_idx orig_name with
      | Some igot_i -> iplt_igot_sym_indices.(j) <- igot_base + igot_i
      | None -> ())
    iplt_entries;
  (* igot_orig_sym_indices.(i) = output symtab index of the original symbol for
     IGOT entry i. Populated during the scan below. 0 = not found. *)
  let igot_orig_sym_indices = Array.make num_igot 0 in
  let plt_index_map = Hashtbl.create num_iplt in
  let got_index_map = Hashtbl.create num_igot in
  (* Single forward scan of symtab_body. For each symbol, compute the DJB2a hash
     of its name directly from the strtab bigarray — zero string allocations for
     the N-K non-matching symbols. Only on a hash match do we byte-compare
     against the pre-existing IGOT name string (also allocation-free). *)
  let n = Buf.size symtab_body / Rela.sym_entry_size in
  for sym_idx = 0 to n - 1 do
    let p = sym_idx * Rela.sym_entry_size in
    let st_name =
      Bigarray.Array1.unsafe_get symtab_body p
      lor (Bigarray.Array1.unsafe_get symtab_body (p + 1) lsl 8)
      lor (Bigarray.Array1.unsafe_get symtab_body (p + 2) lsl 16)
      lor (Bigarray.Array1.unsafe_get symtab_body (p + 3) lsl 24)
    in
    if st_name > 0
    then
      let h = djb2a_strtab strtab_body st_name in
      match Hashtbl.find_opt name_hash_to_igot h with
      | None -> ()
      | Some candidates ->
        List.iter
          (fun (igot_i, orig_name) ->
            if strtab_name_equals strtab_body st_name orig_name
            then begin
              if igot_orig_sym_indices.(igot_i) = 0
              then igot_orig_sym_indices.(igot_i) <- sym_idx;
              if not (Hashtbl.mem got_index_map sym_idx)
              then Hashtbl.add got_index_map sym_idx (igot_base + igot_i);
              match igot_to_iplt_idx.(igot_i) with
              | -1 -> ()
              | iplt_j ->
                if not (Hashtbl.mem plt_index_map sym_idx)
                then Hashtbl.add plt_index_map sym_idx (iplt_base + iplt_j)
            end)
          candidates
  done;
  (* Build synthetic strtab data and precompute absolute st_name offsets. *)
  let synthetic_buf = Buffer.create 64 in
  let igot_st_names = Array.make num_igot 0 in
  List.iteri
    (fun i entry ->
      let igot_sym = Igot.Entry.igot_symbol entry in
      let st_name = original_strtab_size + Buffer.length synthetic_buf in
      Buffer.add_string synthetic_buf igot_sym;
      Buffer.add_char synthetic_buf '\x00';
      igot_st_names.(i) <- st_name)
    igot_entries;
  let iplt_st_names = Array.make num_iplt 0 in
  List.iteri
    (fun i entry ->
      let iplt_sym = Iplt.Entry.iplt_symbol entry in
      let st_name = original_strtab_size + Buffer.length synthetic_buf in
      Buffer.add_string synthetic_buf iplt_sym;
      Buffer.add_char synthetic_buf '\x00';
      iplt_st_names.(i) <- st_name)
    iplt_entries;
  let synthetic_strtab_data = Buffer.to_bytes synthetic_buf in
  let total_symbols = iplt_base + num_iplt in
  ( plt_index_map,
    got_index_map,
    igot_orig_sym_indices,
    iplt_igot_sym_indices,
    igot_st_names,
    iplt_st_names,
    synthetic_strtab_data,
    total_symbols )

(* Rewrite a single .rela.text* section. Uses pre-computed integer-keyed maps
   (sym_index -> new_sym_index) so the hot loop performs only integer hashtable
   lookups with no string allocation.

   The section size is known ahead of time, so we pre-allocate the result array
   and fill it in one forward pass, avoiding both the reversed-list accumulator
   and the [List.rev] traversal. *)
let rewrite_rela_section ~rela_body ~plt_index_map ~got_index_map =
  let n = Buf.size rela_body / Rela.rela_entry_size in
  let placeholder : Rela.rela_entry =
    { r_offset = 0L;
      r_sym = 0;
      r_type = Rela.Reloc_type.of_int 0;
      r_addend = 0L
    }
  in
  let arr = Array.make n placeholder in
  let i = ref 0 in
  Rela.iter_rela_entries ~rela_body
    ~f:(fun ~r_offset ~r_sym ~r_type ~r_addend ->
      let r_offset = Int64.of_int r_offset in
      let r_addend = Int64.of_int r_addend in
      let new_entry : Rela.rela_entry =
        if Rela.Reloc_type.equal r_type Rela.Reloc_type.plt32
        then
          match Hashtbl.find_opt plt_index_map r_sym with
          | Some new_idx ->
            { r_offset;
              r_sym = new_idx;
              r_type = Rela.Reloc_type.pc32;
              r_addend
            }
          | None -> { r_offset; r_sym; r_type; r_addend }
        else if Rela.Reloc_type.equal r_type Rela.Reloc_type.rex_gotpcrelx
        then
          match Hashtbl.find_opt got_index_map r_sym with
          | Some new_idx ->
            { r_offset;
              r_sym = new_idx;
              r_type = Rela.Reloc_type.pc32;
              r_addend
            }
          | None -> { r_offset; r_sym; r_type; r_addend }
        else { r_offset; r_sym; r_type; r_addend }
      in
      arr.(!i) <- new_entry;
      incr i);
  arr

(* Each entry in SYMTAB_SHNDX is 4 bytes (Elf64_Word) *)
let symtab_shndx_entry_size = 4

(* Compute file layout. Since we rewrite .rela.text* sections in place, we don't
   allocate new space for them - only for the new IGOT/IPLT sections and the
   updated symtab/strtab/shstrtab. If the input has a SYMTAB_SHNDX section, we
   also allocate space for the extended version. *)
let compute_file_layout ~original_data_end ~igot_and_iplt ~total_symbols
    ~strtab_size ~shstrtab_size ~num_sections ~shentsize ~has_symtab_shndx =
  let current = ref original_data_end in
  let alloc alignment size =
    current := align_up64 !current alignment;
    let offset = !current in
    let size = Int64.of_int size in
    current := Int64.add offset size;
    { Section_layout.offset; size }
  in
  let igot_t = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt_t = Build_igot_and_iplt.iplt igot_and_iplt in
  let igot = alloc 16 (Igot.section_size igot_t) in
  let rela_igot = alloc 8 (Igot.num_entries igot_t * Rela.rela_entry_size) in
  let iplt = alloc 16 (Iplt.section_size iplt_t) in
  let rela_iplt = alloc 8 (Iplt.num_entries iplt_t * Rela.rela_entry_size) in
  let symtab = alloc 8 (total_symbols * Rela.sym_entry_size) in
  (* Allocate SYMTAB_SHNDX section if input has one *)
  let symtab_shndx =
    if has_symtab_shndx
    then Some (alloc 4 (total_symbols * symtab_shndx_entry_size))
    else None
  in
  let strtab = alloc 1 strtab_size in
  let shstrtab = alloc 1 shstrtab_size in
  let section_headers_offset = align_up64 !current 8 in
  let total_size =
    Int64.add section_headers_offset (Int64.of_int (num_sections * shentsize))
  in
  { Layout.igot;
    rela_igot;
    iplt;
    rela_iplt;
    symtab;
    symtab_shndx;
    strtab;
    shstrtab;
    section_headers_offset;
    total_size
  }

(* Sections that should be renamed for Large_code partitions *)
let sections_to_rename = [".text"; ".rodata"; ".data"; ".bss"]

(* Check if a section base name (without .rela prefix) needs renaming *)
let base_needs_rename base =
  List.exists
    (fun s -> String.equal base s || String.starts_with ~prefix:s base)
    sections_to_rename

(* Rename a section name based on partition kind.

   For Large_code partitions, .text -> .caml.p1.text, .rela.text ->
   .rela.caml.p1.text, etc. *)
let rename_section ~(partition_kind : Partition.kind) name =
  match partition_kind with
  | Main -> name
  | Large_code _ ->
    let prefix = Partition.section_prefix partition_kind in
    let is_rela = String.starts_with ~prefix:".rela" name in
    if is_rela
    then
      (* For .rela.* sections, check if the base (after .rela) needs renaming *)
      let base = String.sub name 5 (String.length name - 5) in
      if base_needs_rename base then ".rela" ^ prefix ^ base else name
    else if base_needs_rename name
    then prefix ^ name
    else name

(* [rela_text_sections] is a list of (section, body) pairs for all .rela.text*
   sections in the input file. Each section's relocations will be rewritten to
   use the synthetic IGOT/IPLT symbols. *)
let compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_sections
    ~partition_kind ~igot_and_iplt =
  log_verbose "forming rewrite plan for partition %s"
    (Partition.symbol_prefix partition_kind);
  let num_original = Buf.size symtab_body / Rela.sym_entry_size in
  let ( plt_index_map,
        got_index_map,
        igot_orig_sym_indices,
        iplt_igot_sym_indices,
        igot_st_names,
        iplt_st_names,
        synthetic_strtab_data,
        total_symbols ) =
    build_all_symbol_maps ~symtab_body ~strtab_body ~igot_and_iplt ~num_original
  in
  (* Rewrite all .rela.text* sections *)
  let rewritten_rela_sections =
    List.map
      (fun (section, rela_body) ->
        log_verbose "  rewriting section %s" section.Elf.sh_name_str;
        let entries =
          rewrite_rela_section ~rela_body ~plt_index_map ~got_index_map
        in
        { Rewritten_rela_section.section_offset = section.Elf.sh_offset;
          entries
        })
      rela_text_sections
  in
  let shstrtab = Strtab.create () in
  let section_name_offsets = String.Tbl.create 64 in
  Array.iter
    (fun (s : Elf.section) ->
      let renamed = rename_section ~partition_kind s.sh_name_str in
      let offset = Strtab.add shstrtab renamed in
      String.Tbl.add section_name_offsets s.sh_name_str (offset, renamed))
    sections;
  let igot_name_str = rename_section ~partition_kind ".data.igot" in
  let igot_name_offset = Strtab.add shstrtab igot_name_str in
  let rela_igot_name_str = rename_section ~partition_kind ".rela.data.igot" in
  let rela_igot_name_offset = Strtab.add shstrtab rela_igot_name_str in
  let iplt_name_str = rename_section ~partition_kind ".text.iplt" in
  let iplt_name_offset = Strtab.add shstrtab iplt_name_str in
  let rela_iplt_name_str = rename_section ~partition_kind ".rela.text.iplt" in
  let rela_iplt_name_offset = Strtab.add shstrtab rela_iplt_name_str in
  let num_original_sections = Array.length sections in
  let igot_idx = num_original_sections in
  let rela_igot_idx = num_original_sections + 1 in
  let iplt_idx = num_original_sections + 2 in
  let rela_iplt_idx = num_original_sections + 3 in
  let find_section_by_type sh_type =
    let rec loop i =
      if i >= Array.length sections
      then None
      else if Elf.Section_type.(equal (of_u32 sections.(i).Elf.sh_type) sh_type)
      then Some i
      else loop (i + 1)
    in
    loop 0
  in
  let symtab_idx =
    match find_section_by_type Elf.Section_type.sht_symtab with
    | Some i -> i
    | None -> 0
  in
  (* Find the SYMTAB_SHNDX section if present *)
  let symtab_shndx_idx =
    find_section_by_type Elf.Section_type.sht_symtab_shndx
  in
  (* We need SYMTAB_SHNDX if the input has one, OR if our new section indices
     are >= SHN_LORESERVE (65280). *)
  let needs_symtab_shndx =
    Option.is_some symtab_shndx_idx
    || Rela.Section_index.(needs_extended (of_int igot_idx))
  in
  (* If we need SYMTAB_SHNDX but the input doesn't have it, we need to create a
     new section. *)
  let need_new_symtab_shndx =
    needs_symtab_shndx && Option.is_none symtab_shndx_idx
  in
  let new_symtab_shndx_idx, symtab_shndx_name_offset, num_sections =
    if need_new_symtab_shndx
    then
      let idx = num_original_sections + 4 in
      let name_offset = Strtab.add shstrtab ".symtab_shndx" in
      Some idx, Some name_offset, num_original_sections + 5
    else None, None, num_original_sections + 4
  in
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) -> max acc (Int64.add s.sh_offset s.sh_size))
      0L sections
  in
  (* strtab size = original strtab + synthetic names appended to it *)
  let strtab_size = Buf.size strtab_body + Bytes.length synthetic_strtab_data in
  let layout =
    compute_file_layout ~original_data_end ~igot_and_iplt ~total_symbols
      ~strtab_size ~shstrtab_size:(Strtab.length shstrtab) ~num_sections
      ~shentsize:header.Elf.e_shentsize ~has_symtab_shndx:needs_symtab_shndx
  in
  { num_original_symbols = num_original;
    igot_st_names;
    iplt_st_names;
    synthetic_strtab_data;
    igot_orig_sym_indices;
    iplt_igot_sym_indices;
    total_symbols;
    rewritten_rela_sections;
    shstrtab;
    section_name_offsets;
    igot_name_offset;
    igot_name_str;
    rela_igot_name_offset;
    rela_igot_name_str;
    iplt_name_offset;
    iplt_name_str;
    rela_iplt_name_offset;
    rela_iplt_name_str;
    igot_idx;
    rela_igot_idx;
    iplt_idx;
    rela_iplt_idx;
    num_sections;
    symtab_idx;
    symtab_shndx_idx;
    new_symtab_shndx_idx;
    symtab_shndx_name_offset;
    original_data_end;
    layout
  }
