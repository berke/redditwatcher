(* Prioqueue *)

module Make :
  functor (A : sig type t val compare : t -> t -> int end) ->
    sig
      exception Empty
      type 'a elt = {
        mutable priority : A.t;
        mutable position : int option;
        mutable data : 'a;
      }
      type 'a t = { mutable n : int; mutable a : 'a elt option array; }
      val lt : 'a elt option -> 'b elt option -> bool
      val setqa : 'a t -> int -> 'a elt option -> unit
      val create : unit -> 'a t
      val clear : 'a t -> unit
      val is_empty : 'a t -> bool
      val length : 'a t -> int
      val iter : (int -> A.t -> 'a -> unit) -> 'a t -> unit
      val array_swap : 'a t -> int -> int -> unit
      val child_0 : int -> int
      val child_1 : int -> int
      val parent : int -> int
      val percolate_up : 'a t -> int -> unit
      val percolate_down : 'a t -> int -> unit
      val update : 'a t -> 'b elt -> unit
      val data : 'a elt -> 'a
      val priority : 'a elt -> A.t
      val add : 'a t -> 'a -> A.t -> 'a elt
      val get : 'a t -> int -> 'a elt
      val prealloc : int -> int
      val remove : 'a t -> int -> unit
      val take : 'a t -> 'a elt
    end
