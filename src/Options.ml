
type nonrec 'a option = 'a option =
  | None
  | Some of 'a

module Option = struct
  type nonrec 'a t = 'a option =
    | None
    | Some of 'a

  exception No_value

  let case ~some ~none self =
    match self with
    | Some x -> some x
    | None   -> none ()

  module Prelude = struct
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

  include Prelude

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

  let hash item_hash self =
    match self with
    | None -> 42
    | Some x -> Hashtbl.seeded_hash 43 (item_hash x)

  (* let dump pp1 fmt self = *)
  (*   match self with *)
  (*   | None   -> *)
  (*     Format.pp_print_string fmt "None" *)
  (*   | Some x -> *)
  (*     Format.pp_print_string fmt "(Some "; *)
  (*     (pp1 fmt) x; *)
  (*     Format.pp_print_string fmt ")" *)

  let equal eq_a t1 t2 =
    match t1, t2 with
    | None, None   -> true
    | Some a1, Some a2 -> eq_a a1 a2
    | _ -> false

  let compare cmp_a t1 t2 =
    match t1, t2 with
    | None, None -> `Equal
    | None, Some _ -> `Less
    | Some _, None -> `Greater
    | Some a1, Some a2 -> cmp_a a1 a2

  module Compat = struct
    let compare cmp_a t1 t2 =
      match t1, t2 with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some a1, Some a2 -> cmp_a a1 a2
  end


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

  let return x = Some x

  let bind f self =
    match self with
    | Some x -> f x
    | None -> None

  let map f self =
    match self with
    | Some x -> Some (f x)
    | None -> None

  let (<@>) = map
end

include Option.Prelude

