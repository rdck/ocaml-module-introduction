open Core

module type SET = sig

  type 'a set

  (* create a singleton set *)
  val singleton : 'a -> 'a set

  (* create the set of all elements in a list *)
  val of_list : 'a list -> 'a set

  (* take the union of two sets *)
  val union : 'a set -> 'a set -> 'a set

  (* take the difference of two sets *)
  val difference : 'a set -> 'a set -> 'a set

  (* further operations omitted for the sake of brevity *)

end

module SetModule = struct

  type 'a set = 'a list

  (* Yes, polymorphic equality is controversial! We will get to that later. *)
  let eq = Stdlib.(=)

  let rec union left = function
    | [] -> left
    | x :: xs ->
        if List.exists left ~f:(eq x)
        then union left xs
        else union (x :: left) xs

  let of_list l =
    union [] l

  let singleton a = [ a ]

  let difference left right =
    let filter a = not (List.exists right ~f:(eq a)) in
    List.filter left ~f:filter

end

module Set : SET = SetModule

module type EQUALITY = sig
  type t
  val eq : t -> t -> bool
end

module SetFunctor = functor (E : EQUALITY) -> struct

  type element = E.t
  type set = element list

  let rec union left = function
    | [] -> left
    | x :: xs ->
        if List.exists left ~f:(E.eq x)
        then union left xs
        else union (x :: left) xs

  let of_list l =
    union [] l

  let singleton a = [ a ]

  let difference left right =
    let filter a = not (List.exists right ~f:(E.eq a)) in
    List.filter left ~f:filter

end

module IntEquality : EQUALITY = struct
  type t = int
  let eq = Int.equal
end

module IntSet = SetFunctor(IntEquality)

module type SET_FUNCTOR = functor (E : EQUALITY) -> sig

  type element = E.t
  type set

  (* create a singleton set *)
  val singleton : element -> set

  (* create the set of all elements in a list *)
  val of_list : element list -> set

  (* take the union of two sets *)
  val union : set -> set -> set

  (* take the difference of two sets *)
  val difference : set -> set -> set

  (* further operations omitted for the sake of brevity *)

end

module SetFunctor' : SET_FUNCTOR = SetFunctor

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(* an example instance *)
module OptionFunctor : FUNCTOR = struct
  type 'a t = 'a option
  let map f xs = Option.map xs ~f:f
end

module type APPLICATIVE = sig
  type 'a t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

(* an example instance *)
module OptionApplicative : APPLICATIVE = struct
  type 'a t = 'a option
  let pure a = Some a
  let apply fs xs = match (fs, xs) with
  | (Some f, Some x) -> Some (f x)
  | _ -> None
end

let () = Printf.printf "done\n"
