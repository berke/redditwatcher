(* Priority queue *)

module Make (A : sig type t val compare : t -> t -> int end) =
  struct
    exception Empty

    type 'a elt =
        { mutable priority:A.t;
          mutable position:int option;
          mutable data:'a }
    
    type 'a t =
        { mutable n:int;
          mutable a:'a elt option array;
        } 
    
    let lt x y =
      match (x,y) with
	| (None,None) -> false
	| (Some(x),None) -> true
	| (None,Some(x)) -> false
	| (Some(x),Some(y)) -> A.compare x.priority y.priority = -1
    
    let setqa q i x =
      match (q.a.(i),x) with
      |	(None,None) -> ()
      |	(None,Some(x)) ->
	  q.a.(i) <- Some(x);
	  x.position <- Some(i)
      |	(Some(x),None) ->
	  q.a.(i) <- None;
	  x.position <- None
      |	(Some(x),Some(y)) ->
	  q.a.(i) <- Some(y);
	  x.position <- None;
	  y.position <- Some(i)

    let create () =
      { n = 0;
	a = Array.create 16 None }
    
    let clear q =
      q.n <- 0;
      for i = 0 to q.n - 1 do
	setqa q i None
      done
    
    let is_empty q = q.n = 0

    let length q = q.n

    let iter (f:int -> A.t -> 'a -> unit) q =
      for i = 0 to q.n - 1 do
	match q.a.(i) with
	| Some(x) -> f i x.priority x.data
	| None -> raise (Invalid_argument "malformed queue")
      done
    
    (* Priorities are sorted in ascending order *)
	
    let array_swap q i j =
      let t = q.a.(i) in
      setqa q i q.a.(j);
      setqa q j t
	  
    let child_0 i = i * 2 + 1
    let child_1 i = i * 2 + 2
    let parent i = (i - 1) / 2
	  
    let rec percolate_up q i =
      if i > 0 && (lt q.a.(i) q.a.(parent i)) then
	begin
	  array_swap q i (parent i);
	  percolate_up q (parent i)
	end
	  
    let rec percolate_down q i =
      let zonk j =
	begin
	  array_swap q i j;
	  percolate_down q j
	end
      in
      if child_1 i < q.n then
        (* on promouvoit le fils ayant la plus petite priorité au poste de calife *)
	begin
	  if lt q.a.(child_0 i) q.a.(child_1 i) then
	    zonk (child_0 i)
	  else
	    zonk (child_1 i)
	end
      else
	if child_0 i < q.n then
          (* il n'y a qu'un seul fils, celui-ci devient calife à la place du calife *)
	  zonk (child_0 i)
	else
          (* le calife est mort et il n'y a rien a faire *)
	  ()
	
    let update q e =
      match e.position with
      |	None -> ()
      |	Some(i) ->
	  if i > 0 && (lt q.a.(i) q.a.(parent i)) then
	    percolate_up q i
	  else
	    percolate_down q i

    let data x = x.data
    let priority x = x.priority

    let add q x p =
      let n = q.n in
      if n = Array.length q.a then
	begin
	  q.a <- Array.append q.a (Array.create (max 256 n) None);
	end;
      let c = { priority = p; data = x; position = None } in
      setqa q n (Some c);
      q.n <- n + 1;
      percolate_up q n;
      c
    
    let get q i =
      if i < 0 || i >= q.n then raise (Invalid_argument "index out of range")
      else match q.a.(i) with
      |	Some(x) -> x
      |	None -> raise (Invalid_argument "malformed queue")
    
    let prealloc n = max (min (n / 16) 256) 16
	
    let remove q i =
      if i < 0 || i >= q.n then raise (Invalid_argument "index out of range")
      else
	let n = q.n in
	q.n <- n - 1;
	if i + 1 < n then
	  begin
      	    array_swap q i (n - 1);
	    percolate_down q i
	  end;
	let m = Array.length q.a in
	if m - n > 256 || n < m / 2 then
	  q.a <- Array.sub q.a 0 (max n (min m (prealloc n)))
    
    let take q =
      if q.n = 0 then raise Empty
      else
	let r = q.a.(0) in
	remove q 0;
	match r with
	| Some(x) -> x
	| None -> raise (Invalid_argument "malformed queue")
    
  end

