open Compare
open Control

type nonrec 'a option = 'a option =
  | None
  | Some of 'a

module Option = struct
  type 'a t = 'a option = None | Some of 'a

  exception No_value

  let case ~some ~none self =
    match self with
    | Some x -> some x
    | None   -> none ()

  module type Export = sig
    val some : 'a -> 'a t
    val none : 'a t
    val is_some : 'a t -> bool
    val is_none : 'a t -> bool
    val if_some : ('a -> unit) -> 'a t -> unit
    val if_none : (unit -> unit) -> 'a t -> unit
    val or_else : (unit -> 'a) -> 'a t -> 'a
    val ( or ) : 'a t -> 'a -> 'a
  end

  module Export = struct
    let some x = Some x
    let none   = None

    let is_some = function Some _ -> true  | None -> false
    let is_none = function Some _ -> false | None -> true

    let (or) self default =
      match self with
      | Some x -> x
      | None -> default

    let or_else default self =
      match self with
      | Some x -> x
      | None   -> default ()

    let if_some f t =
      match t with
      | None -> ()
      | Some x -> f x

    let if_none f t =
      match t with
      | None -> f ()
      | Some _ -> ()
  end

  include Export

  let is_empty = is_none

  let catch f =
    try
      Some (f ())
    with _ -> None

  let to_list self =
    match self with
    | Some x -> [x]
    | None -> []

  let to_result ~error self =
    match self with
    | Some x -> Ok x
    | None -> Error error

  let to_bool self =
    match self with
    | Some _ -> true
    | None -> false

  include Equal1.Make(struct
      type nonrec 'a t = 'a t

      let equal eq_a t1 t2 =
        match t1, t2 with
        | None, None   -> true
        | Some a1, Some a2 -> eq_a a1 a2
        | _ -> false
    end)

  include Ordered1.Make(struct
      type nonrec 'a t = 'a t

      let compare cmp_a t1 t2 =
        match t1, t2 with
        | None, None -> Equal
        | None, Some _ -> Less
        | Some _, None -> Greater
        | Some a1, Some a2 -> cmp_a a1 a2
    end)


  (* Unsafe *)

  let force self =
    match self with
    | Some x -> x
    | None   -> raise No_value

  let or_fail message self =
    match self with
    | Some x -> x
    | None -> failwith message


  (* Monad *)

  module Monad_instance = struct
    type 'a t = 'a option

    let return x = Some x

    let bind f self =
      match self with
      | Some x -> f x
      | None -> None
  end

  module Functor_instance = Monad.To_functor(Monad_instance)
  module Applicative_instance = Monad.To_applicative(Monad_instance)

  include Monad.Extension(Monad_instance)
  include Functor.Extension(Functor_instance)
  include Applicative.Extension(Applicative_instance)


  (* Default1 *)
  (* let default = None *)

  (* Hashable1 *)
  (* let hash _ self = *)
  (*   Hashtbl.hash self *)

  (* Printable1 *)
  (* include Printable1.Make(struct *)
  (*     type nonrec 'a t = 'a t *)

  (*     let pp pp1 fmt self = *)
  (*       match self with *)
  (*       | None   -> *)
  (*         Format.pp_print_string fmt "None" *)
  (*       | Some x -> *)
  (*         Format.pp_print_string fmt "(Some "; *)
  (*         (pp1 fmt) x; *)
  (*         Format.pp_print_string fmt ")" *)
  (*   end) *)
end

include Option.Export

