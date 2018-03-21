(* Copyright (c) 2018 Rizo I <rizo@odis.io>
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
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

open Order
open Control


(** A type that represents either a wrapped value or none, the absence of a
    value. *)


(** {1:overview Overview}

    Option types provide a safe way to deal with potentially absent values. In
    most imperative languages this is achieved with a {i null pointer}. Continue... *)


(** {1:option_type Option Type} *)

(** Optional values of type ['a].

    This type represents an optional value: it is either [Some] and contains a
    value, or [None], and does not.

    The complementary {!Option} module includes operations on [option] values. *)
type nonrec 'a option = 'a option =
  | None        (** No value. *)
  | Some of 'a  (** A value of type 'a. *)


(** Module with operations for the {!type:option} type.

    Option types are very useful to represent computations that may fail in one
    obvious way (like getting an element from an empty list). For more
    complicated errors consider using the [result] type which can include
    detailed descriptions of the errors. *)
module Option : sig

  (** Optional values of type ['a]. *)
  type 'a t = 'a option =
    | None        (** No value. *)
    | Some of 'a  (** A value of type 'a. *)

  (** Module exports suitable for [open]ing. *)
  module type Export = sig
    val some : 'a -> 'a option
    (** Produces an option containing a given value.

        [some x] wraps [x] with the [Some] constructor. This function is useful
        for partial application.

        {3 Examples}
{[
assert (List.map some [1; 2; 3] = [Some 1; Some 2; Some 3])
]} *)

    val none : 'a option
    (** [none] is the [None] constructor. Not very useful but included for
        consistency with {!val:some}. *)

    val is_some : 'a option -> bool
    (** [is_some option] is [true] if [option] is a [Some] value. *)

    val is_none : 'a option -> bool
    (** [is_none option] is [true] if [option] is a [None] value. *)

    val if_some : ('a -> unit) -> 'a option -> unit
    (** [if_some f option] applies an effectful function [f] to the value wrapped
        in [option] or does nothing if [option] contains no value.

        {3 Examples}
{[
Some "Hello, world!" |> if_some print_endline
(* Output: Hello, World! *)

None |> if_some print
(* No output *)
]} *)

    val if_none : (unit -> unit) -> 'a option -> unit
    (** [if_none f option] calls an effectful function [f] if [option] is a
        [None] value or does nothing otherwise.

        {3 Examples}
{[
None |> if_none (fun () -> print_endline "no value")
(* Output: no value *)

Some 42 |> if_none (fun () -> print_endline "no value")
(* No output *)
]} *)

    val or_else : (unit -> 'a) -> 'a option -> 'a
    (** [or_else f opt] extracts the optional value. If the optional is
        [None], the default value [f ()] is returned. A thunk is used instead of
        a direct value to avoid the default value evaluation when the option is
        set.

        {b See also:} {{: #val-or} [Option.(or)]}

        {3 Examples}
{[
assert (Some 1 |> Option.or_else (fun () -> 0) = 1);
assert (None |> Option.or_else (fun () -> 0) = 0);

(* [read_line] will not be called in the following example. *)
assert (Some "Bob" |> Option.or_else read_line = "Bob");
]} *)

    val ( or ) : 'a option -> 'a -> 'a
    (** Unwrap the option {i or} use a default value.

        [option or default] is the flipped infix version of [Option.or_else]
        equivalent to [option |> or_else (fun () -> default)]. The [default]
        value will be always evaluated, for lazy evaluation use
        [Option.or_else].

        {b Note:} this operator has a very low precedence, {i e.g.} [option or
        0 + 1] is evaluated as [option or (0 + 1)] and {i not} as [(option or 0) +
        1]. Complex expressions should be grouped explicitly to avoid ambiguity.

        {b See also:} {{: #val-or_else} [Option.or_else]}

        {3 Examples}
{[
assert ((None or 0) = 0);
assert ((None or 1 + 2) = 3);
assert ((Some 1 or 0) = 1);
assert ((Some 1 or 1 + 2) = 4);
assert (((Some 1 or 1) + 2) = 3);
]} *)
  end

  (** {2:public_exports Public Exports} *)
  include Export

  val case : some: ('a -> 'b) -> none: (unit -> 'b) -> 'a option -> 'b
  (** Pattern-matching on option values with functions.

      [case ~none ~some option] is the function [some] applied to the value
      wrapped by [option], or [none ()] if [option] has no value.

      {3 Examples}
{[
assert (None    |> Option.case ~some:((+) 1) ~none:(fun () -> 0) = 0);
assert (Some 42 |> Option.case ~some:((+) 1) ~none:(fun () -> 0) = 43);
]} *)

  val is_empty : 'a option -> bool
  (** [is_empty opt] is [true] if the option is [None] and [false] otherwise. *)

  val catch : (unit -> 'a) -> 'a option
  (** [catch f] wraps the call to [f], returning [None] if it raises an
      exception or its return value as [Some] otherwise.

      {3 Examples}
{[
Option.catch read_line |> Option.or_else (fun () -> "nothing")
]} *)

  (* val dump : (Format.formatter -> 'a -> unit) -> 'a option *)
  (*   -> Format.formatter -> 'a option -> unit *)
  (** Pretty-printer for the option value that dumpped . *)

  val hash : ('a -> int) -> 'a option -> int
  (** Produces a hash for the option value. *)


  (** {2:option_conversions Conversions} *)

  val to_bool : 'a option -> bool
  (** [to_bool self] is an alis for {!is_some}.

      {3 Examples}
{[
assert (Option.to_bool (Some 42) = true);
assert (Option.to_bool None = false);
]} *)

  val to_list : 'a option -> 'a list
  (** [to_list self] is a singleton list with the value wrapped by [self] or an empty list if [self] is [None].

      {3 Examples}
{[
assert (Option.to_bool (Some 42) = true);
assert (Option.to_bool None = false);
]} *)

  val to_result : error: 'b -> 'a option -> ('a, 'b) result
  (** [to_result ~error t] converts an option value into a result. If the
      option value is [None], the [error] value will be used for the [Error]
      result case.

      {3 Examples}
{[
assert (Option.to_result (Some 42) = Ok 42);
assert (Option.to_result ~error:"No" None = Error "No");
]} *)


  (* Functor *)
  val ( <@> ) : ('a -> 'b) -> 'a option ->  'b option
  (** [f <@> self] will apply [f] to the value wrapped by [self], returning an
      option with the resulting value, or [None] if [self] does not not have any
      value. This operator is an infix version of [map].

      {3 Examples}

{[
assert (List.reverse <@> Some [1; 2; 3] == Some [3; 2; 1]);
assert (Int.to_string <@> None == None);
]} *)


  (** {2:implemented_interfaces Implemented Interfaces} *)

  include Ordered1.Extension    with type 'a t := 'a option
  include Equal1.Extension      with type 'a t := 'a option
  include Monad.Extension       with type 'a t := 'a option
  include Functor.Extension     with type 'a t := 'a option
  include Applicative.Extension with type 'a t := 'a option


  (** {2:options_unsafe_operations Unsafe Operations}

      This section includes the operations that may raise exceptions. *)

  exception No_value
  (** Exception raised when forcing a [None] option value. *)

  val force : 'a option -> 'a
  (** Forces the extraction the optional value.

      @raise No_value if [option] is [None].

      {3 Examples}
{[
assert (Option.force (List.head [1; 2; 3]) = 1);
assert (raises (Option.force (List.head [])))
]} *)

  val or_fail : string -> 'a option -> 'a
  (** [or_fail message option] forces the extraction of the optional value and
      fails with [message] if [option] does not contain a value.

      @raise Failure if [self] is [None].

      {3 Examples:}
{[
assert (List.head [1; 2; 3] |> Option.or_fail "empty list" = 1);
]} *)
end

(** {1:public_exports Public Definitions}

    These are the public definitions that will be exported when the top-level
    [Options] module is open. *)
include Option.Export

