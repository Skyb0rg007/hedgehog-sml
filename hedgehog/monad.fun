(* monad.fun
 * © 2023 Skye Soss
 *
 * Monadic operations
 *)

signature MONAD_BASE =
  sig
    type 'a t

    val pure : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

signature MONAD =
  sig
    type 'a t

    (* Inject a pure value into the monadic type *)
    val pure : 'a -> 'a t

    (* Lift a function into the monadic type *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (* Sequentially compose two monadic actions *)
    val bind : 'a t -> ('a -> 'b t) -> 'b t

    (* Promote function application to the monadic type *)
    val ap : ('a -> 'b) t * 'a t -> 'b t

    (* Conventional monadic operator *)
    val join : 'a t t -> 'a t

    (* Perform two monadic operations, returning their results together *)
    val both : 'a t * 'b t -> ('a * 'b) t

    (* Promote a function of given arity to the monadic type *)
    val map2 : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
    val map3 : ('a * 'b * 'c -> 'd) -> 'a t * 'b t * 'c t -> 'd t
    val map4 : ('a * 'b * 'c * 'd -> 'e) -> 'a t * 'b t * 'c t * 'd t -> 'e t
    val map5 : ('a * 'b * 'c * 'd * 'e -> 'f) -> 'a t * 'b t * 'c t * 'd t * 'e t -> 'f t

    (* Perform a monadic action a given number of times, collecting results *)
    val replicate : int -> 'a t -> 'a list t

    (* Perform a monadic action a given number of times, ignoring results *)
    val replicate_ : int -> 'a t -> unit t

    (* Conditionally perform a monadic action *)
    val when : bool -> unit t -> unit t
    val unless : bool -> unit t -> unit t

    (* Perform a list of monadic actions left-to-right *)
    val sequence : 'a t list -> 'a list t

    (* Map each element to an action, then perform each left-to-right *)
    val mapM : ('a -> 'b t) -> 'a list -> 'b list t

    (* Generalization of `List.filter` to a monadic predicate *)
    val filterM : ('a -> bool t) -> 'a list -> 'a list t

    structure Infix :
      sig
        (* Infix `map` *)
        val <$> : ('a -> 'b) * 'a t -> 'b t

        (* Infix `map` with the argument order swapped *)
        val <&> : 'a t * ('a -> 'b) -> 'b t

        (* `m $> x` == `map (fn _ => x) m` *)
        val  $> : 'a t * 'b -> 'b t

        (* `$>` with the argument order swapped *)
        val <$  : 'a * 'b t -> 'a t

        (* Infix `ap` *)
        val <*> : ('a -> 'b) t * 'a t -> 'b t

        (* Infix `ap`, but evaluates the argument before the function *)
        val <**> : 'a t * ('a -> 'b) t -> 'b t

        (* Perform two actions in sequence, returning the second result *)
        val  *> : 'a t * 'b t -> 'b t

        (* Perform two actions in sequence, returning the first result *)
        val <*  : 'a t * 'b t -> 'a t

        (* Infix `bind` *)
        val >>= : 'a t * ('a -> 'b t) -> 'b t

        (* Infix `bind` with the argument order swapped *)
        val =<< : ('a -> 'b t) * 'a t -> 'b t

        (* Left-to-right Kleisli composition *)
        val >=> : ('a -> 'b t) * ('b -> 'c t) -> 'a -> 'c t

        (* Right-to-left Kleisli composition *)
        val <=< : ('b -> 'c t) * ('a -> 'b t) -> 'a -> 'c t
      end
  end

functor MonadFn(M : MONAD_BASE) : MONAD =
struct
  type 'a t = 'a M.t

  val pure = M.pure
  val bind = M.bind
  val map = M.map

  fun join m = bind m (fn x => x)

  fun ap (m1, m2) = bind m1 (fn f => map (fn x => f x) m2)

  fun both (m1, m2) = bind m1 (fn x => map (fn y => (x, y)) m2)

  fun map2 f (m1, m2) =
    bind m1 (fn x =>
    map (fn y => f (x, y)) m2)

  fun map3 f (m1, m2, m3) =
    bind m1 (fn x =>
    bind m2 (fn y =>
    map (fn z => f (x, y, z)) m3))

  fun map4 f (m1, m2, m3, m4) =
    bind m1 (fn x =>
    bind m2 (fn y =>
    bind m3 (fn z =>
    map (fn w => f (x, y, z, w)) m4)))

  fun map5 f (m1, m2, m3, m4, m5) =
    bind m1 (fn x =>
    bind m2 (fn y =>
    bind m3 (fn z =>
    bind m4 (fn w =>
    map (fn q => f (x, y, z, w, q)) m5))))

  fun replicate n m =
    if n <= 0
      then pure []
      else map2 op :: (m, replicate (n - 1) m)

  fun replicate_ n m =
    if n <= 0
      then pure ()
      else bind m (fn _ => replicate_ (n - 1) m)

  fun when true m = m
    | when false _ = pure ()

  fun unless true _ = pure ()
    | unless false m = m

  fun sequence xs = List.foldr (map2 op ::) (pure []) xs

  fun mapM f = List.foldr (fn (x, acc) => map2 op :: (f x, acc)) (pure [])

  local
    fun consIf x (true, xs) = x :: xs
      | consIf _ (false, xs) = xs
  in
    fun filterM p =
      List.foldr (fn (x, acc) => map2 (consIf x) (p x, acc)) (M.pure [])
  end

  structure Infix =
    struct
      fun <$> (f, g) = map f g
      fun <&> (g, f) = map f g
      fun $> (g, x) = map (fn _ => x) g
      fun <$ (x, g) = map (fn _ => x) g
      fun <*> (g1, g2) = map2 (fn (f, x) => f x) (g1, g2)
      fun <**> (g1, g2) = map2 (fn (x, f) => f x) (g1, g2)
      fun *> (g1, g2) = map2 #2 (g1, g2)
      fun <* (g1, g2) = map2 #1 (g1, g2)
      fun >>= (g, k) = bind g k
      fun =<< (k, g) = bind g k
      fun >=> (f, g) x = bind (f x) g
      fun <=< (g, f) = >=> (f, g)
    end
end
