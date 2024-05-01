(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

(** {2 Base types } *)

type buffer
(** Type of a buffer. A cstruct is composed of an underlying buffer
    and position/length within this buffer. *)

type t = private { buffer : buffer; off : int; len : int }
(** Type of a cstruct. *)

type byte = char
(** A single byte type *)

val byte : int -> byte
(** [byte v] convert [v] to a single byte.
    @raise Invalid_argument if [v] is negative or greater than 255. *)

type uint8 = int
(** 8-bit unsigned integer.  The representation is currently an
    unboxed OCaml integer. *)

type uint16 = int
(** 16-bit unsigned integer.  The representation is currently an
    unboxed OCaml integer. *)

type uint32 = int32
(** 32-bit unsigned integer.  The representation is currently a
    boxed OCaml int32. *)

type uint64 = int64
(** 64-bit unsigned integer.  The representation is currently a
    boxed OCaml int64. *)

(** {2 Creation and conversion} *)

val empty : t
(** [empty] is the cstruct of length 0. *)

val create : int -> t
(** [create len] is a fresh cstruct of size [len] with an offset of 0,
    filled with zero bytes. *)

val create_unsafe : int -> t
(** [create_unsafe len] is a cstruct of size [len] with an offset of 0.

    Note that the returned cstruct will contain arbitrary data,
    likely including the contents of previously-deallocated cstructs.

    Beware!

    Forgetting to replace this data could cause your application
    to leak sensitive information.
*)

val of_string : ?allocator:(int -> t) -> ?off:int -> ?len:int -> string -> t
(** [of_string ~allocator ~off ~len str] is the cstruct representation of [str]
    slice located at offset [off] (default [0]) and of length [len] (default
    [String.length str - off]),
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used.

    @raise Invalid_argument if [off] or [len] is negative, or
    [String.length str - off] < [len].
*)

val of_bytes : ?allocator:(int -> t) -> ?off:int -> ?len:int -> bytes -> t
(** [of_bytes ~allocator byt] is the cstruct representation of [byt]
    slice located at offset [off] (default [0]) and of length [len] (default
    [Bytes.length byt - off]),
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used.

    @raise Invalid_argument if [off] or [len] is negative, or
    [Bytes.length str - off] < [len]. *)

val of_hex : ?off:int -> ?len:int -> string -> t
(** [of_hex ~off ~len str] is the cstruct [cs].  Every pair of hex-encoded
    characters in [str] starting at offset [off] (default [0]) of length [len]
    (default [String.length str - off]) are converted to one byte in [cs].
    Whitespaces (space, newline, tab, carriage return) in [str] are skipped.

    @raise Invalid_argument if the input string contains invalid characters or
    has an odd numbers of non-whitespace characters, or if [off] or [len] are
    negative, or [String.length str - off] < [len]. *)

(** {2 Comparison } *)

val equal : t -> t -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] correspond to the same sequence of
    bytes. *)

val compare : t -> t -> int
(** [compare t1 t2] gives an unspecified total ordering over {!t}. *)

(** {2 Getters and Setters } *)

val byte_to_int : byte -> int
(** Convert a byte to an integer *)

val check_bounds : t -> int -> bool
(** [check_bounds cstr len] is [true] if [len] is a non-negative integer and
    [cstr.buffer]'s size is greater or equal than [len] [false] otherwise.*)

val check_alignment : t -> int -> bool
(** [check_alignment cstr alignment] is [true] if the first byte stored
    within [cstr] is at a memory address where [address mod alignment = 0],
    [false] otherwise.
    Typical uses are to check a buffer is aligned to a page or disk sector
    boundary.
    @raise Invalid_argument if [alignment] is not a positive integer. *)

val get_char : t -> int -> char
(** [get_char t off] returns the character contained in the cstruct
    at offset [off].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val get_uint8 : t -> int -> uint8
(** [get_uint8 t off] returns the byte contained in the cstruct
    at offset [off].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val set_char : t -> int -> char -> unit
(** [set_char t off c] sets the byte contained in the cstruct
    at offset [off] to character [c].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val set_uint8 : t -> int -> uint8 -> unit
(** [set_uint8 t off c] sets the byte contained in the cstruct
    at offset [off] to byte [c].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val sub : t -> int -> int -> t
(** [sub cstr off len] is [{ t with off = t.off + off; len }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val sub_copy : t -> int -> int -> t
(** [sub_copy cstr off len] is a new copy of [sub cstr off len],
    that does not share the underlying buffer of [cstr].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val shift : t -> int -> t
(** [shift cstr len] is [{ cstr with off=t.off+len; len=t.len-len }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val copy : t -> int -> int -> string
[@@ocaml.alert
  deprecated
    "this is just like [to_string] without defaults, were you looking for \
     [sub_copy]?"]
(** [copy cstr off len] is the string representation of the segment of
    [t] starting at [off] of size [len]. It is equivalent to
    [Cstruct.to_string cstr ~off ~len].
    @raise Invalid_argument if [off] and [len] do not designate a
    valid segment of [t]. *)

val blit : t -> int -> t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] characters from
    cstruct [src], starting at index [srcoff], to cstruct [dst],
    starting at index [dstoff]. It works correctly even if [src] and
    [dst] are the same string, and the source and destination
    intervals overlap.

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val blit_from_string : string -> int -> t -> int -> int -> unit
(** [blit_from_string src srcoff dst dstoff len] copies [len]
    characters from string [src], starting at index [srcoff], to
    cstruct [dst], starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid substring of [src], or if [dstoff] and [len] do not
    designate a valid segment of [dst]. *)

val blit_from_bytes : bytes -> int -> t -> int -> int -> unit
(** [blit_from_bytes src srcoff dst dstoff len] copies [len]
    characters from bytes [src], starting at index [srcoff], to
    cstruct [dst], starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid subsequence of [src], or if [dstoff] and [len] do not
    designate a valid segment of [dst]. *)

val blit_to_bytes : t -> int -> bytes -> int -> int -> unit
(** [blit_to_bytes src srcoff dst dstoff len] copies [len] characters
    from cstruct [src], starting at index [srcoff], to the [dst] buffer,
    starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val memset : t -> int -> unit
(** [memset t x] sets all the bytes of [t] to [x land 0xff]. *)

val split : ?start:int -> t -> int -> t * t
(** [split ~start cstr len] is a tuple containing the cstruct
    extracted from [cstr] at offset [start] (default: 0) of length
    [len] as first element, and the rest of [cstr] as second
    element.
    @raise Invalid_argument if [start] exceeds the cstruct length,
    or if there is a bounds violation of the cstruct via [len+start]. *)

val to_string : ?off:int -> ?len:int -> t -> string
(** [to_string ~off ~len t] will allocate a fresh OCaml [string] and copy the
    contents of the cstruct starting at offset [off] (default [0]) of length
    [len] (default [Cstruct.length t - off]) into it, and return that string.

    @raise Invalid_argument if [off] or [len] is negative, or
    [Cstruct.length t - off] < [len]. *)

val to_hex_string : ?off:int -> ?len:int -> t -> string
(** [to_hex_string ~off ~len t] is a fresh OCaml [string] containing
    the hex representation of [sub t off len]. It is therefore of length
    [2 * len]. This string can be read back into a Cstruct using {!of_hex}.
    @raise Invalid_argument if [off] or [len] is negative, or
      if [Cstruct.length t - off < len].
    @since 6.2 *)

val to_bytes : ?off:int -> ?len:int -> t -> bytes
(** [to_bytes ~off ~len t] will allocate a fresh OCaml [bytes] and copy the
    contents of the cstruct starting at offset [off] (default [0]) of length
    [len] (default [Cstruct.length t - off]) into it, and return that bytes.

    @raise Invalid_argument if [off] or [len] is negative, or
    [Cstruct.length str - off] < [len]. *)

val to_bigarray :
  t -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** [to_bigarray t] converts a {!t} into a {!type:buffer} Bigarray, using
    the Bigarray slicing to allocate a fresh array that preserves
    sharing of the underlying buffer. *)

module BE : sig
  (** Get/set big-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16 : t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32 : t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64 : t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16 : t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32 : t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64 : t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)
end

module LE : sig
  (** Get/set little-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16 : t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32 : t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64 : t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16 : t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32 : t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64 : t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)
end

module HE : sig
  (** Get/set host-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16 : t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long host-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32 : t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long host-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64 : t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long host-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16 : t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long host-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32 : t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long host-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64 : t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long host-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)
end

(** {2 Debugging } *)

val hexdump : t -> unit
(** When the going gets tough, the tough hexdump their cstructs
    and peer at it until the bug disappears.  This will directly
    prettyprint the contents of the cstruct to the standard output. *)

val hexdump_to_buffer : Buffer.t -> t -> unit
(** [hexdump_to_buffer buf c] will append the pretty-printed hexdump
    of the cstruct [c] to the buffer [buf]. *)

val hexdump_pp : Format.formatter -> t -> unit
(** [hexdump_pp f c] pretty-prints a hexdump of [c] to [f]. *)

val debug : t -> string
(** [debug t] will print out the internal details of a cstruct such
    as its base offset and the length, and raise an assertion failure
    if invariants have been violated.  Not intended for casual use. *)

(** {2 List of buffers} *)

val lenv : t list -> int
(** [lenv cstrs] is the combined length of all cstructs in [cstrs].
    @raise Invalid_argument if computing the sum overflows. *)

val copyv : t list -> string
(** [copyv cstrs] is the string representation of the concatenation of
    all cstructs in [cstrs].
    @raise Invalid_argument if the length of the result would
    exceed [Sys.max_string_length]. *)

val fillv : src:t list -> dst:t -> int * t list
(** [fillv ~src ~dst] copies from [src] to [dst] until [src] is exhausted or [dst] is full.
    Returns the number of bytes copied and the remaining data from [src], if any.
    This is useful if you want buffer data into fixed-sized chunks. *)

val shiftv : t list -> int -> t list
(** [shiftv ts n] is [ts] without the first [n] bytes.
    It has the property that [equal (concat (shiftv ts n)) (shift (concat ts) n)].
    This operation is fairly fast, as it will share the tail of the list.
    The first item in the returned list is never an empty cstruct,
    so you'll get [[]] if and only if [lenv ts = n]. *)

(** {2 Iterations} *)

type 'a iter = unit -> 'a option
(** Type of an iterator. *)

val iter : (t -> int option) -> (t -> 'a) -> t -> 'a iter
(** [iter lenf of_cstr cstr] is an iterator over [cstr] that returns
    elements of size [lenf cstr] and type [of_cstr cstr]. *)

val fold : ('b -> 'a -> 'b) -> 'a iter -> 'b -> 'b
(** [fold f iter acc] is [(f iterN accN ... (f iter acc)...)]. *)

val append : t -> t -> t
(** [append t1 t2] is the concatenation [t1 || t2]. *)

val concat : t list -> t
(** [concat ts] is the concatenation of all the [ts]. It is not guaranteed that
 * the result is a newly created [t] in the zero- and one-element cases. *)

val rev : t -> t
(** [rev t] is [t] in reverse order. The return value is a freshly allocated
    cstruct, and the argument is not modified. *)

(** {1 Helpers to parse.}

    [Cstruct] is used to manipulate {i payloads} which can be formatted
   according an {{:https://perdu.com/}RFC} or an user-defined format. In such context, this module
   provides utilities to be able to easily {i parse} {i payloads}.

    Due to the type {!Cstruct.t}, no copy are done when you use these utilities
   and you are able to extract your information without a big performance cost.

    More precisely, each values returned by these utilities will be located into
   the minor-heap where the base buffer will never be copied or relocated.

    For instance, to parse a Git tree object:

{v
  entry := perm ' ' name '\000' 20byte
  tree  := entry *
v}

    {[
      open Cstruct

      let ( >>= ) = Option.bind

      let rec hash_of_name ~name payload =
        if is_empty payload then raise Not_found
        else
          cut ~sep:(v " ") payload >>= fun (_, payload) ->
          cut ~sep:(v "\000") payload >>= fun (name', payload) ->
          if name = name' then with_range ~len:20 payload
          else hash_of_name ~name (shift payload 20)
    ]}

    A [Cstruct] defines a possibly empty subsequence of bytes in a {e base}
   buffer (a {!Bigarray.Array1.t}).

    The positions of a buffer [b] of length [l] are the slits found
   before each byte and after the last byte of the buffer. They are
   labelled from left to right by increasing number in the range \[[0];[l]\].

{v
positions  0   1   2   3   4    l-1    l
           +---+---+---+---+     +-----+
  indices  | 0 | 1 | 2 | 3 | ... | l-1 |
           +---+---+---+---+     +-----+
v}

    The [i]th byte index is between positions [i] and [i+1].

    Formally we define a subbuffer of [b] as being a subsequence
   of bytes defined by a {e off} position and a {e len} number. When
   [len] is [0] the subbuffer is {e empty}. Note that for a given
   base buffer there are as many empty subbuffers as there are positions
   in the buffer.

    Like in strings, we index the bytes of a subbuffer using zero-based
   indices.
*)

val get : t -> int -> char
(** [get cs zidx] is the byte of [cs] at its zero-based index [zidx].
    It's an alias of {!get_char}.

    @raise Invalid_argument if [zidx] is not an index of [cs]. *)

val get_byte : t -> int -> int
(** [get_byte cs zidx] is [Char.code (get cs zidx)]. It's an alias of {!get_uint8}. *)
