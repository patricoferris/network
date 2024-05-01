(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type buffer = bytes

(* Note:
 *
 * We try to maintain the property that no constructed [t] can ever point out of
 * its underlying buffer. This property is guarded by all of the constructing
 * functions and the fact that the type is private, and used by various
 * functions that would otherwise be completely unsafe.
 *
 * Furthermore, no operation on [t] is allowed to extend the view on the
 * underlying Bigarray structure, only narrowing is allowed.
 *
 * All well-intended souls are kindly invited to cross-check that the code
 * indeed maintains this invariant.
 *)

type t = { buffer : buffer; off : int; len : int }

let pp_t ppf t =
  Format.fprintf ppf "[%d,%d](%d)" t.off t.len (Bytes.length t.buffer)

let string_t ppf str = Format.fprintf ppf "[%d]" (String.length str)
let bytes_t ppf str = Format.fprintf ppf "[%d]" (Bytes.length str)

let err fmt =
  let b = Buffer.create 20 in
  (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf =
    Format.pp_print_flush ppf ();
    invalid_arg (Buffer.contents b)
  in
  Format.kfprintf k ppf fmt

let err_of_bigarray t = err "Cstruct.of_bigarray off=%d len=%d" t
let err_sub t = err "Cstruct.sub: %a off=%d len=%d" pp_t t
let err_shift t = err "Cstruct.shift %a %d" pp_t t
let err_shiftv n = err "Cstruct.shiftv short by %d" n

let err_copy_to_string caller t =
  err "Cstruct.%s %a off=%d len=%d" caller pp_t t

let err_to_hex_string t = err "Cstruct.to_hex_string %a off=%d len=%d" pp_t t

let err_blit_src src dst =
  err "Cstruct.blit src=%a dst=%a src-off=%d len=%d" pp_t src pp_t dst

let err_blit_dst src dst =
  err "Cstruct.blit src=%a dst=%a dst-off=%d len=%d" pp_t src pp_t dst

let err_blit_from_string_src src dst =
  err "Cstruct.blit_from_string src=%a dst=%a src-off=%d len=%d" string_t src
    pp_t dst

let err_blit_from_string_dst src dst =
  err "Cstruct.blit_from_string src=%a dst=%a dst-off=%d len=%d" string_t src
    pp_t dst

let err_blit_from_bytes_src src dst =
  err "Cstruct.blit_from_bytes src=%a dst=%a src-off=%d len=%d" bytes_t src pp_t
    dst

let err_blit_from_bytes_dst src dst =
  err "Cstruct.blit_from_bytes src=%a dst=%a dst-off=%d len=%d" bytes_t src pp_t
    dst

let err_blit_to_bytes_src src dst =
  err "Cstruct.blit_to_bytes src=%a dst=%a src-off=%d len=%d" pp_t src bytes_t
    dst

let err_blit_to_bytes_dst src dst =
  err "Cstruct.blit_to_bytes src=%a dst=%a dst-off=%d len=%d" pp_t src bytes_t
    dst

let err_invalid_bounds f =
  err "invalid bounds in Cstruct.%s %a off=%d len=%d" f pp_t
[@@inline never]

let err_split t = err "Cstruct.split %a start=%d off=%d" pp_t t
let err_iter t = err "Cstruct.iter %a i=%d len=%d" pp_t t

let create_unsafe len =
  let buffer = Bytes.create len in
  { buffer; len; off = 0 }

let check_bounds t len = len >= 0 && Bytes.length t.buffer >= len
let empty = create_unsafe 0

external check_alignment_bigstring : buffer -> int -> int -> bool
  = "caml_check_alignment_bigstring"

let check_alignment t alignment =
  if alignment > 0 then check_alignment_bigstring t.buffer t.off alignment
  else invalid_arg "check_alignment must be positive integer"

type byte = char

let byte (i : int) : byte = Char.chr i
let byte_to_int (b : byte) = int_of_char b

type uint8 = int
type uint16 = int
type uint32 = int32
type uint64 = int64

let debug t =
  let max_len = Bytes.length t.buffer in
  if t.off + t.len > max_len || t.len < 0 || t.off < 0 then (
    Format.printf "ERROR: t.off+t.len=%d %a\n%!" (t.off + t.len) pp_t t;
    assert false)
  else Format.asprintf "%a" pp_t t

let sub t off len =
  (* from https://github.com/mirage/ocaml-cstruct/pull/245

     Cstruct.sub should select what a programmer intuitively expects a
     sub-cstruct to be. I imagine holding out my hands, with the left
     representing the start offset and the right the end. I think of a
     sub-cstruct as any span within this range. If I move my left hand only to
     the right (new_start >= t.off), and my right hand only to the left
     (new_end <= old_end), and they don't cross (new_start <= new_end), then I
     feel sure the result will be a valid sub-cstruct. And if I violate any one
     of these constraints (e.g. moving my left hand further left), then I feel
     sure that the result wouldn't be something I'd consider to be a sub-cstruct.

     Wrapping considerations in modular arithmetic:

     Note that if x is non-negative, and x + y wraps, then x + y must be
     negative. This is easy to see with modular arithmetic because if y is
     negative then the two arguments will cancel to some degree the result
     cannot be further from zero than one of the arguments. If y is positive
     then x + y can wrap, but even max_int + max_int doesn't wrap all the way to
     zero.

     The three possibly-wrapping operations are:

     new_start = t.off + off. t.off is non-negative so if this wraps then
     new_start will be negative and will fail the new_start >= t.off test.

     new_end = new_start + len. The above test ensures that new_start is
     non-negative in any successful return. So if this wraps then new_end will
     be negative and will fail the new_start <= new_end test.

     old_end = t.off + t.len. This uses only the existing trusted values. It
     could only wrap if the underlying bigarray had a negative length! *)
  let new_start = t.off + off in
  let new_end = new_start + len in
  let old_end = t.off + t.len in
  if new_start >= t.off && new_end <= old_end && new_start <= new_end then
    { t with off = new_start; len }
  else err_sub t off len

let shift t amount =
  let off = t.off + amount in
  let len = t.len - amount in
  if amount < 0 || amount > t.len || not (check_bounds t (off + len)) then
    err_shift t amount
  else { t with off; len }

let rec skip_empty = function t :: ts when t.len = 0 -> skip_empty ts | x -> x

let rec shiftv ts = function
  | 0 -> skip_empty ts
  | n -> (
      match ts with
      | [] -> err_shiftv n
      | t :: ts when n >= t.len -> shiftv ts (n - t.len)
      | t :: ts -> shift t n :: ts)

external unsafe_blit_bigstring_to_bigstring :
  buffer -> int -> buffer -> int -> int -> unit
  = "caml_blit_bigstring_to_bigstring"
[@@noalloc]

external unsafe_blit_string_to_bigstring :
  string -> int -> buffer -> int -> int -> unit
  = "caml_blit_string_to_bigstring"
[@@noalloc]

external unsafe_blit_bytes_to_bigstring :
  Bytes.t -> int -> buffer -> int -> int -> unit
  = "caml_blit_string_to_bigstring"
[@@noalloc]

external unsafe_blit_bigstring_to_bytes :
  buffer -> int -> Bytes.t -> int -> int -> unit
  = "caml_blit_bigstring_to_string"
[@@noalloc]

external unsafe_compare_bigstring : buffer -> int -> buffer -> int -> int -> int
  = "caml_compare_bigstring"
[@@noalloc]

external unsafe_fill_bigstring : buffer -> int -> int -> int -> unit
  = "caml_fill_bigstring"
[@@noalloc]

let copy_to_string caller src srcoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then
    err_copy_to_string caller src srcoff len
  else
    let b = Bytes.create len in
    unsafe_blit_bigstring_to_bytes src.buffer (src.off + srcoff) b 0 len;
    (* The following call is safe, since b is not visible elsewhere. *)
    Bytes.unsafe_to_string b

let copy = copy_to_string "copy"

let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then
    err_blit_src src dst srcoff len
  else if dstoff < 0 || dst.len - dstoff < len then
    err_blit_dst src dst dstoff len
  else
    unsafe_blit_bigstring_to_bigstring src.buffer (src.off + srcoff) dst.buffer
      (dst.off + dstoff) len

let sub_copy cstr off len : t =
  let cstr2 = create_unsafe len in
  blit cstr off cstr2 0 len;
  cstr2

let blit_from_string src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || String.length src - srcoff < len
  then err_blit_from_string_src src dst srcoff len
  else if dst.len - dstoff < len then
    err_blit_from_string_dst src dst dstoff len
  else
    unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off + dstoff) len

let blit_from_bytes src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || Bytes.length src - srcoff < len then
    err_blit_from_bytes_src src dst srcoff len
  else if dst.len - dstoff < len then err_blit_from_bytes_dst src dst dstoff len
  else
    unsafe_blit_bytes_to_bigstring src srcoff dst.buffer (dst.off + dstoff) len

let blit_to_bytes src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || src.len - srcoff < len then
    err_blit_to_bytes_src src dst srcoff len
  else if Bytes.length dst - dstoff < len then
    err_blit_to_bytes_dst src dst dstoff len
  else
    unsafe_blit_bigstring_to_bytes src.buffer (src.off + srcoff) dst dstoff len

let compare t1 t2 =
  let l1 = t1.len and l2 = t2.len in
  match compare l1 l2 with
  | 0 -> (
      match unsafe_compare_bigstring t1.buffer t1.off t2.buffer t2.off l1 with
      | 0 -> 0
      | r -> if r < 0 then -1 else 1)
  | r -> r

let equal t1 t2 = compare t1 t2 = 0

(* Note that this is only safe as long as all [t]s are coherent. *)
let memset t x = unsafe_fill_bigstring t.buffer t.off t.len x

let create len =
  let t = create_unsafe len in
  memset t 0;
  t

let set_uint8 t i c =
  if i >= t.len || i < 0 then err_invalid_bounds "set_uint8" t i 1
  else Bytes.set t.buffer (t.off + i) (Char.unsafe_chr c)

let set_char t i c =
  if i >= t.len || i < 0 then err_invalid_bounds "set_char" t i 1
  else Bytes.set t.buffer (t.off + i) c

let get_uint8 t i =
  if i >= t.len || i < 0 then err_invalid_bounds "get_uint8" t i 1
  else Char.code (Bytes.get t.buffer (t.off + i))

let get_char t i =
  if i >= t.len || i < 0 then err_invalid_bounds "get_char" t i 1
  else Bytes.get t.buffer (t.off + i)

external ba_set_int16 : buffer -> int -> uint16 -> unit = "%caml_bytes_set16u"
external ba_set_int32 : buffer -> int -> uint32 -> unit = "%caml_bytes_set32u"
external ba_set_int64 : buffer -> int -> uint64 -> unit = "%caml_bytes_set64u"
external ba_get_int16 : buffer -> int -> uint16 = "%caml_bytes_get16u"
external ba_get_int32 : buffer -> int -> uint32 = "%caml_bytes_get32u"
external ba_get_int64 : buffer -> int -> uint64 = "%caml_bytes_get64u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let set_uint16 swap p t i c =
  if i > t.len - 2 || i < 0 then err_invalid_bounds (p ^ ".set_uint16") t i 2
  else ba_set_int16 t.buffer (t.off + i) (if swap then swap16 c else c)
[@@inline]

let set_uint32 swap p t i c =
  if i > t.len - 4 || i < 0 then err_invalid_bounds (p ^ ".set_uint32") t i 4
  else ba_set_int32 t.buffer (t.off + i) (if swap then swap32 c else c)
[@@inline]

let set_uint64 swap p t i c =
  if i > t.len - 8 || i < 0 then err_invalid_bounds (p ^ ".set_uint64") t i 8
  else ba_set_int64 t.buffer (t.off + i) (if swap then swap64 c else c)
[@@inline]

let get_uint16 swap p t i =
  if i > t.len - 2 || i < 0 then err_invalid_bounds (p ^ ".get_uint16") t i 2
  else
    let r = ba_get_int16 t.buffer (t.off + i) in
    if swap then swap16 r else r
[@@inline]

let get_uint32 swap p t i =
  if i > t.len - 4 || i < 0 then err_invalid_bounds (p ^ ".get_uint32") t i 4
  else
    let r = ba_get_int32 t.buffer (t.off + i) in
    if swap then swap32 r else r
[@@inline]

let get_uint64 swap p t i =
  if i > t.len - 8 || i < 0 then err_invalid_bounds (p ^ ".get_uint64") t i 8
  else
    let r = ba_get_int64 t.buffer (t.off + i) in
    if swap then swap64 r else r
[@@inline]

module BE = struct
  let set_uint16 t i c = set_uint16 (not Sys.big_endian) "BE" t i c [@@inline]
  let set_uint32 t i c = set_uint32 (not Sys.big_endian) "BE" t i c [@@inline]
  let set_uint64 t i c = set_uint64 (not Sys.big_endian) "BE" t i c [@@inline]
  let get_uint16 t i = get_uint16 (not Sys.big_endian) "BE" t i [@@inline]
  let get_uint32 t i = get_uint32 (not Sys.big_endian) "BE" t i [@@inline]
  let get_uint64 t i = get_uint64 (not Sys.big_endian) "BE" t i [@@inline]
end

module LE = struct
  let set_uint16 t i c = set_uint16 Sys.big_endian "LE" t i c [@@inline]
  let set_uint32 t i c = set_uint32 Sys.big_endian "LE" t i c [@@inline]
  let set_uint64 t i c = set_uint64 Sys.big_endian "LE" t i c [@@inline]
  let get_uint16 t i = get_uint16 Sys.big_endian "LE" t i [@@inline]
  let get_uint32 t i = get_uint32 Sys.big_endian "LE" t i [@@inline]
  let get_uint64 t i = get_uint64 Sys.big_endian "LE" t i [@@inline]
end

module HE = struct
  let set_uint16 t i c = set_uint16 false "HE" t i c [@@inline]
  let set_uint32 t i c = set_uint32 false "HE" t i c [@@inline]
  let set_uint64 t i c = set_uint64 false "HE" t i c [@@inline]
  let get_uint16 t i = get_uint16 false "HE" t i [@@inline]
  let get_uint32 t i = get_uint32 false "HE" t i [@@inline]
  let get_uint64 t i = get_uint64 false "HE" t i [@@inline]
end

let to_bigarray buffer =
  (* Not shared, hmmm?! *)
  let len = buffer.len - buffer.off in
  let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  unsafe_blit_bytes_to_bigstring buffer.buffer 0 (Obj.magic ba) 0 len;
  ba

let length { len; _ } = len

(** [sum_lengths ~caller acc l] is [acc] plus the sum of the lengths
    of the elements of [l].  Raises [Invalid_argument caller] if
    arithmetic overflows. *)
let rec sum_lengths_aux ~caller acc = function
  | [] -> acc
  | h :: t ->
      let sum = length h + acc in
      if sum < acc then invalid_arg caller else sum_lengths_aux ~caller sum t

let sum_lengths ~caller l = sum_lengths_aux ~caller 0 l
let lenv l = sum_lengths ~caller:"Cstruct.lenv" l

let copyv ts =
  let sz = sum_lengths ~caller:"Cstruct.copyv" ts in
  let dst = Bytes.create sz in
  let _ =
    List.fold_left
      (fun off src ->
        let x = length src in
        unsafe_blit_bigstring_to_bytes src.buffer src.off dst off x;
        off + x)
      0 ts
  in
  (* The following call is safe, since dst is not visible elsewhere. *)
  Bytes.unsafe_to_string dst

let fillv ~src ~dst =
  let rec aux dst n = function
    | [] -> (n, [])
    | hd :: tl ->
        let avail = length dst in
        let first = length hd in
        if first <= avail then (
          blit hd 0 dst 0 first;
          aux (shift dst first) (n + first) tl)
        else (
          blit hd 0 dst 0 avail;
          let rest_hd = shift hd avail in
          (n + avail, rest_hd :: tl))
  in
  aux dst 0 src

let to_string ?(off = 0) ?len:sz t =
  let len = match sz with None -> length t - off | Some l -> l in
  copy_to_string "to_string" t off len

let to_hex_string ?(off = 0) ?len:sz t : string =
  let[@inline] nibble_to_char (i : int) : char =
    if i < 10 then Char.chr (i + Char.code '0')
    else Char.chr (i - 10 + Char.code 'a')
  in

  let len = match sz with None -> length t - off | Some l -> l in
  if len < 0 || off < 0 || t.len - off < len then err_to_hex_string t off len
  else
    let out = Bytes.create (2 * len) in
    for i = 0 to len - 1 do
      let c = Char.code @@ Bytes.get t.buffer (i + t.off + off) in
      Bytes.set out (2 * i) (nibble_to_char (c lsr 4));
      Bytes.set out ((2 * i) + 1) (nibble_to_char (c land 0xf))
    done;
    Bytes.unsafe_to_string out

let to_bytes ?off ?len t = Bytes.unsafe_of_string (to_string ?off ?len t)

let[@inline always] of_data_abstract blitfun lenfun ?allocator ?(off = 0) ?len
    buf =
  let buflen = match len with None -> lenfun buf - off | Some len -> len in
  match allocator with
  | None ->
      let c = create_unsafe buflen in
      blitfun buf off c 0 buflen;
      c
  | Some fn ->
      let c = fn buflen in
      blitfun buf off c 0 buflen;
      { c with len = buflen }

let of_string ?allocator ?off ?len buf =
  of_data_abstract blit_from_string String.length ?allocator ?off ?len buf

let of_bytes ?allocator ?off ?len buf =
  of_data_abstract blit_from_bytes Bytes.length ?allocator ?off ?len buf

let of_hex ?(off = 0) ?len str =
  let str =
    let l = match len with None -> String.length str - off | Some l -> l in
    String.sub str off l
  in
  let string_fold ~f ~z str =
    let st = ref z in
    String.iter (fun c -> st := f !st c) str;
    !st
  in
  let hexdigit p = function
    | 'a' .. 'f' as x -> int_of_char x - 87
    | 'A' .. 'F' as x -> int_of_char x - 55
    | '0' .. '9' as x -> int_of_char x - 48
    | x ->
        Format.ksprintf invalid_arg "of_hex: invalid character at pos %d: %C" p
          x
  in
  let whitespace = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  match
    string_fold
      ~f:(fun (cs, i, p, acc) ->
        let p' = succ p in
        function
        | char when whitespace char -> (cs, i, p', acc)
        | char -> (
            match (acc, hexdigit p char) with
            | None, x -> (cs, i, p', Some (x lsl 4))
            | Some y, x ->
                set_uint8 cs i (x lor y);
                (cs, succ i, p', None)))
      ~z:(create_unsafe (String.length str lsr 1), 0, 0, None)
      str
  with
  | _, _, _, Some _ ->
      Format.ksprintf invalid_arg "of_hex: odd numbers of characters"
  | cs, i, _, _ -> sub cs 0 i

let hexdump_pp fmt t =
  let before fmt = function
    | 0 -> ()
    | 8 -> Format.fprintf fmt "  "
    | _ -> Format.fprintf fmt " "
  in
  let after fmt = function 15 -> Format.fprintf fmt "@;" | _ -> () in
  Format.pp_open_vbox fmt 0;
  for i = 0 to length t - 1 do
    let column = i mod 16 in
    let c = Char.code (Bytes.get t.buffer (t.off + i)) in
    Format.fprintf fmt "%a%.2x%a" before column c after column
  done;
  Format.pp_close_box fmt ()

let hexdump = Format.printf "@\n%a@." hexdump_pp

let hexdump_to_buffer buf t =
  let f = Format.formatter_of_buffer buf in
  Format.fprintf f "@\n%a@." hexdump_pp t

let split ?(start = 0) t off =
  try
    let header = sub t start off in
    let body = sub t (start + off) (length t - off - start) in
    (header, body)
  with Invalid_argument _ -> err_split t start off

type 'a iter = unit -> 'a option

let iter lenfn pfn t =
  let body = ref (Some t) in
  let i = ref 0 in
  fun () ->
    match !body with
    | Some buf when length buf = 0 ->
        body := None;
        None
    | Some buf -> (
        match lenfn buf with
        | None ->
            body := None;
            None
        | Some plen ->
            incr i;
            let p, rest =
              try split buf plen
              with Invalid_argument _ -> err_iter buf !i plen
            in
            body := Some rest;
            Some (pfn p))
    | None -> None

let rec fold f next acc =
  match next () with None -> acc | Some v -> fold f next (f acc v)

let append cs1 cs2 =
  let l1 = length cs1 and l2 = length cs2 in
  let cs = create_unsafe (l1 + l2) in
  blit cs1 0 cs 0 l1;
  blit cs2 0 cs l1 l2;
  cs

let concat = function
  | [] -> create_unsafe 0
  | [ cs ] -> cs
  | css ->
      let result = create_unsafe (sum_lengths ~caller:"Cstruct.concat" css) in
      let aux off cs =
        let n = length cs in
        blit cs 0 result off n;
        off + n
      in
      ignore @@ List.fold_left aux 0 css;
      result

let rev t =
  let n = length t in
  let out = create_unsafe n in
  for i_src = 0 to n - 1 do
    let byte = get_uint8 t i_src in
    let i_dst = n - 1 - i_src in
    set_uint8 out i_dst byte
  done;
  out

(* Convenience function. *)

external unsafe_blit_string_to_bigstring :
  string -> int -> buffer -> int -> int -> unit
  = "caml_blit_string_to_bigstring"
[@@noalloc]

let get { buffer; off; len } zidx =
  if zidx < 0 || zidx >= len then invalid_arg "index out of bounds";
  Bytes.get buffer (off + zidx)

let get_byte { buffer; off; len } zidx =
  if zidx < 0 || zidx >= len then invalid_arg "index out of bounds";
  Char.code (Bytes.get buffer (off + zidx))

let start_pos { off; _ } = off
let stop_pos { off; len; _ } = off + len

let head ?(rev = false) ({ len; _ } as cs) =
  if len = 0 then None else Some (get_char cs (if rev then len - 1 else 0))

let is_empty { len; _ } = len = 0

let is_prefix ~affix:({ len = alen; _ } as affix) ({ len; _ } as cs) =
  if alen > len then false
  else
    let max_zidx = alen - 1 in
    let rec loop i =
      if i > max_zidx then true
      else if get_char affix i <> get_char cs i then false
      else loop (succ i)
    in
    loop 0

let is_infix ~affix:({ len = alen; _ } as affix) ({ len; _ } as cs) =
  if alen > len then false
  else
    let max_zidx_a = alen - 1 in
    let max_zidx_s = len - alen in
    let rec loop i k =
      if i > max_zidx_s then false
      else if k > max_zidx_a then true
      else if k > 0 then
        if get_char affix k = get_char cs (i + k) then loop i (succ k)
        else loop (succ i) 0
      else if get_char affix 0 = get_char cs i then loop i 1
      else loop (succ i) 0
    in
    loop 0 0

let is_suffix ~affix:({ len = alen; _ } as affix) ({ len; _ } as cs) =
  if alen > len then false
  else
    let max_zidx = alen - 1 in
    let max_zidx_a = alen - 1 in
    let max_zidx_s = len - 1 in
    let rec loop i =
      if i > max_zidx then true
      else if get_char affix (max_zidx_a - i) <> get_char cs (max_zidx_s - i)
      then false
      else loop (succ i)
    in
    loop 0

let for_all sat cs =
  let rec go acc i =
    if i < length cs then go (sat (get_char cs i) && acc) (succ i) else acc
  in
  go true 0

let exists sat cs =
  let rec go acc i =
    if i < length cs then go (sat (get_char cs i) || acc) (succ i) else acc
  in
  go false 0

let is_white = function ' ' | '\t' .. '\r' -> true | _ -> false
