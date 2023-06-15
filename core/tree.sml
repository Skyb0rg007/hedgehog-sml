(* Lazy rose trees
 * Used to represent a value and its shrinks
 *)

signature TREE =
  sig
    type 'a t

    (* Deconstructing *)
    val root : 'a t -> 'a
    val children : 'a t -> 'a t Seq.t

    (* Constructing *)
    val node : 'a * 'a t Seq.t -> 'a t
    val consChild : 'a * 'a t -> 'a t
    val unfold : ('a -> 'a Seq.t) -> 'a -> 'a t
    val expand : ('a -> 'a Seq.t) -> 'a t -> 'a t

    (* Operations *)
    val singleton : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapPartial : ('a -> 'b option) -> 'a t -> 'b t option
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val bindPartial : ('a -> 'b t option) -> 'a t -> 'b t option
    val zipWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t

    (* Combine lists of trees heuristically for shrinking purposes *)
    val interleave : 'a t list -> 'a list t

    (* Produces a string rendering of the tree for debugging purposes *)
    val render : string t -> string
  end

structure Tree : TREE =
struct

  datatype 'a t = Node of 'a * 'a t Seq.t

  val node = Node

  fun root (Node (x, _)) = x

  fun children (Node (_, xs)) = xs

  fun unfold f x =
    Node (x, Seq.map (unfold f) (f x))

  fun expand f (Node (x, xs)) =
    Node (x, Seq.append (Seq.map (expand f) xs, Seq.map (unfold f) (f x)))

  fun map f (Node (x, xs)) = Node (f x, Seq.map (map f) xs)

  fun singleton a = Node (a, Seq.empty)

  fun consChild (x, Node (root, children)) =
    Node (root, Seq.cons (singleton x, children))

  fun bind k (Node (x, xs)) =
    let
      val Node (y, ys) = k x
    in
      Node (y, Seq.append (Seq.map (bind k) xs, ys))
    end

  fun bindPartial k (Node (x, xs)) =
    case k x of
        NONE => NONE
      | SOME (Node (y, ys)) =>
          SOME (Node (y, Seq.append (Seq.mapPartial (bindPartial k) xs, ys)))

  fun zipWith f (n1 as Node (x, xs), n2 as Node (y, ys)) =
    let
      val left = Seq.map (fn x => zipWith f (x, n2)) xs
      val right = Seq.map (fn y => zipWith f (n1, y)) ys
    in
      Node (f (x, y), Seq.append (left, right))
    end

  fun mapPartial f (Node (x, xs)) =
    case f x of
        NONE => NONE
      | SOME y => SOME (Node (y, Seq.mapPartial (mapPartial f) xs))

  (* Interleaving trees *)
  local
    fun splitAt _ [] = ([], [])
      | splitAt 1 (x :: xs) = ([x], xs)
      | splitAt n (x :: xs) =
          case splitAt (n - 1) xs of
              (xs1, xs2) => (x :: xs1, xs2)

    (* All ways to remove chunks of size `k` from a list *)
    fun removes k =
      let
        fun loop [] () = Seq.Nil
          | loop xs () =
            let
              val (xs1, xs2) = splitAt k xs
            in
              Seq.Cons (xs2, Seq.map (fn l => xs1 @ l) (loop xs2))
            end
      in
        loop
      end

    (* All ways to split a list *)
    fun splits xs =
      let
        fun loop (_, []) () = Seq.Nil
          | loop (f, r :: rs) () =
              Seq.Cons ((List.rev f, r, rs), loop (r :: f, rs))
      in
        loop ([], xs)
      end

    fun dropSome ts =
      let
        (* ns = [n, n/2, n/4, n/8, ..., 1] *)
        val ns = Seq.takeWhile (fn k => k > 0)
          (Seq.iterate (fn k => k div 2) (List.length ts))
        val ts' = Seq.concatMap (fn n => removes n ts) ns
      in
        Seq.map interleave ts'
      end

    and shrinkOne ts =
      Seq.concatMap
        (fn (xs, Node (_, ys), zs) =>
          Seq.map (fn y => interleave (xs @ [y] @ zs)) ys)
        (splits ts)

    and interleave ts =
      Node (List.map root ts, Seq.append (dropSome ts, shrinkOne ts))
  in
    val interleave = interleave
  end

  (* Printing a tree *)
  local
    val lines = String.fields (fn c => c = #"\n")

    fun shift (_, _, []) = []
      | shift (head, other, x :: xs) =
          head ^ x :: List.map (fn x => other ^ x) xs

    fun renderNode str = if String.size str = 1 then " " ^ str else str

    fun renderTree (Node (x, xs)) =
      lines (renderNode x) @ renderForest xs

    and renderForest seq =
      case seq () of
          Seq.Nil => []
        | Seq.Cons (x, seq') => renderForest' x seq'

    and renderForest' x seq =
      case seq () of
          Seq.Nil =>
            shift (" └╼", "   ", renderTree x)
        | Seq.Cons (y, rest) =>
            shift (" ├╼", " │ ", renderTree x) @ renderForest' y rest
  in
    fun render t = String.concatWith "\n" (renderTree t)
  end

end
