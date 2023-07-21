(* Lazy sequences *)
signature SEQ =
  sig
    datatype 'a node
      = Nil
      | Cons of 'a * (unit -> 'a node)

    type 'a t = unit -> 'a node

    (* Construction *)
    val empty : 'a t
    val singleton : 'a -> 'a t
    val cons : 'a * 'a t -> 'a t
    val fromList : 'a list -> 'a t
    val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
    val iterate : ('a -> 'a) -> 'a -> 'a t

    (* Operations *)
    val isEmpty : 'a t -> bool
    val head : 'a t -> 'a option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapPartial : ('a -> 'b option) -> 'a t -> 'b t
    val filter : ('a -> bool) -> 'a t -> 'a t
    val drop : int -> 'a t -> 'a t
    val takeWhile : ('a -> bool) -> 'a t -> 'a t
    val append : 'a t * 'a t -> 'a t
    val concat : 'a t t -> 'a t
    val concatMap : ('a -> 'b t) -> 'a t -> 'b t
    val zipWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
    val find : ('a -> bool) -> 'a t -> 'a option
    val toList : 'a t -> 'a list
    val app : ('a -> unit) -> 'a t -> unit
  end

structure Seq : SEQ =
  struct
    datatype 'a node
      = Nil
      | Cons of 'a * 'a t

    withtype 'a t = unit -> 'a node

    fun empty () = Nil

    fun isEmpty s =
      case s () of
          Nil => true
        | Cons _ => false

    fun head s =
      case s () of
          Nil => NONE
        | Cons (x, _) => SOME x

    fun singleton x () = Cons (x, empty)

    fun cons (x, xs) () = Cons (x, xs)

    fun fromList xs = List.foldr cons empty xs

    fun append (s1, s2) () =
      case s1 () of
          Nil => s2 ()
        | Cons (x, xs) => Cons (x, append (xs, s2))

    fun map f s () =
      case s () of
          Nil => Nil
        | Cons (x, xs) => Cons (f x, map f xs)

    fun mapPartial f s () =
      case s () of
          Nil => Nil
        | Cons (x, xs) =>
            case f x of
                NONE => mapPartial f xs ()
              | SOME y => Cons (y, mapPartial f xs)

    fun filter p s () =
      case s () of
          Nil => Nil
        | Cons (x, xs) =>
            if p x
              then Cons (x, filter p xs)
              else filter p xs ()

    fun concat s () =
      case s () of
          Nil => Nil
        | Cons (x, xs) => append (x, concat xs) ()

    fun concatMap f s () =
      case s () of
          Nil => Nil
        | Cons (x, xs) => append (f x, concatMap f xs) ()

    fun unfold f seed () =
      case f seed of
          NONE => Nil
        | SOME (x, seed') => Cons (x, unfold f seed')

    fun drop n s =
      if n <= 0
        then s
        else case s () of
                Nil => empty
              | Cons (_, s') => drop (n - 1) s'

    fun takeWhile p s () =
      case s () of
          Nil => Nil
        | Cons (x, xs) =>
            if p x then Cons (x, takeWhile p xs) else Nil

    fun iterate f x () =
      Cons (x, fn () => iterate f (f x) ())

    fun zipWith f (s1, s2) () =
      case s1 () of
          Nil => Nil
        | Cons (x, xs) =>
            case s2 () of
                Nil => Nil
              | Cons (y, ys) => Cons (f (x, y), zipWith f (xs, ys))

    fun find p s =
      case s () of
          Nil => NONE
        | Cons (x, xs) => if p x then SOME x else find p xs

    fun toList s =
      case s () of
          Nil => []
        | Cons (x, xs) => x :: toList xs

    fun app f s =
      case s () of
          Nil => ()
        | Cons (x, xs) => (f x; app f xs)
  end
