
structure Gen =
  struct
    type 'a t = int * SplitMix.t -> 'a Tree.t option

    fun pure x _ = SOME (Tree.singleton x)

    fun map f g args =
      Option.map (Tree.map f) (g args)

    fun bind g k (size, seed) =
      let
        val (s1, s2) = SplitMix.split seed
      in
        case g (size, s1) of
            NONE => NONE
          | SOME t => Tree.bindPartial (fn x => k x (size, s2)) t
      end

    fun join m = bind m (fn x => x)

    fun map2 f (g1, g2) (size, seed) =
      let
        val (s1, s2) = SplitMix.split seed
      in
        case (g1 (size, s1), g2 (size, s2)) of
            (SOME x, SOME y) => SOME (Tree.zipWith f (x, y))
          | _ => NONE
      end

    fun map3 f (g1, g2, g3) =
      bind g1 (fn x =>
      bind g2 (fn y =>
      map (fn z => f (x, y, z)) g3))

    fun map4 f (g1, g2, g3, g4) =
      bind g1 (fn x =>
      bind g2 (fn y =>
      bind g3 (fn z =>
      map (fn w => f (x, y, z, w)) g4)))

    fun both args = map2 (fn p => p) args

    fun replicate n g =
      if n <= 0
        then pure []
        else bind g (fn x => map (fn xs => x :: xs) (replicate (n - 1) g))

    fun delay f args = f () args

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

    fun sized f (size, seed) = f size (size, seed)

    fun scale f g (size, seed) =
      let
        val size' = f size
      in
        if size' < 0
          then raise Fail "Gen.scale: negative size"
          else g (size', seed)
      end

    fun resize size g (_, seed) = g (size, seed)

    local
      fun golden n = Real.round (Real.fromInt n * 0.61803398875)
    in
      fun small g = scale golden g
    end

    val constant = pure

    fun discard _ = NONE

    fun ensure p g = bind g (fn x =>
      if p x
        then constant x
        else discard)

    fun or (g1, g2) (size, seed) =
      let
        val (s1, s2) = SplitMix.split seed
      in
        case g1 (size, s1) of
            NONE => g2 (size, s2)
          | SOME t => SOME t
      end

    fun freeze g (size, seed) =
      case g (size, seed) of
          NONE => NONE
        | SOME t => SOME (Tree.singleton (Tree.root t, fn _ => SOME t))

    fun mapPartial f g =
      let
        fun grow k = scale (fn size => 2 * k + size)

        fun try k =
          if k > 100
            then discard
            else
              bind (freeze (grow k g)) (fn (x, g') =>
              case f x of
                  NONE => try (k + 1)
                | SOME _ => Tree.mapPartial f o Option.valOf o g')
      in
        try 0
      end

    fun filter p = mapPartial (fn x => if p x then SOME x else NONE)

    fun shrink f g args =
      Option.map (Tree.expand f) (g args)

    fun prune g args =
      Option.map (Tree.singleton o Tree.root) (g args)

    local
      fun consNub (x: IntInf.int, s) () =
        case s () of
            Seq.Nil => Seq.Cons (x, s)
          | Seq.Cons (y, s') =>
              if x = y
                then Seq.Cons (y, s')
                else Seq.Cons (x, s)

      fun halves n =
        Seq.takeWhile (fn x => x <> 0)
          (Seq.iterate (fn x => IntInf.~>> (x, 0w1)) n)

      fun towards (dest, x) =
        if dest = x
          then Seq.empty
        else if dest = 0 andalso x = 1
          then Seq.singleton 0
        else
          let
            val diff = IntInf.~>> (x, 0w1) - IntInf.~>> (dest, 0w1)
          in
            consNub (dest, Seq.map (fn y => x - y) (halves diff))
          end

      fun binarySearchTree (bottom, top) =
        let
          val shrinks = towards (bottom, top)
          val children =
            Seq.zipWith binarySearchTree (shrinks, Seq.drop 1 shrinks)
        in
          Tree.node (top, children)
        end

      (* fun generate (range, size, seed) = *)
      (*   let *)
      (*     val (lo, hi) = Range.bounds (range, size) *)
      (*   in *)
      (*     #1 (SplitMix.intInfRange (lo, hi) seed) *)
      (*   end *)
    in
      (* fun intInf_ range (size, seed) = *)
      (*   SOME (Tree.singleton (generate (range, size, seed))) *)

      (* fun intInf range (size, seed) = *)
      (*   let *)
      (*     val origin = Range.origin range *)
      (*     val root = generate (range, size, seed) *)
      (*   in *)
      (*     if origin = root *)
      (*       then SOME (Tree.singleton root) *)
      (*       else SOME (Tree.consChild (origin, binarySearchTree (origin, root))) *)
      (*   end *)
    end

    (* fun element [] = raise Fail "Gen.element: empty list" *)
    (*   | element xs = *)
    (*     let *)
    (*       val len = LargeInt.fromInt (List.length xs) *)
    (*       val range = Range.constant 0 len *)
    (*     in *)
    (*       map (fn n => List.nth (xs, LargeInt.toInt n)) (intInf range) *)
    (*     end *)

    (* fun choice [] = raise Fail "Gen.choice: empty list" *)
    (*   | choice xs = *)
    (*     let *)
    (*       val len = LargeInt.fromInt (List.length xs) *)
    (*       val range = Range.constant 0 len *)
    (*     in *)
    (*       bind (intInf range) (fn n => List.nth (xs, LargeInt.toInt n)) *)
    (*     end *)

    (* fun frequency [] = raise Fail "Gen.frequency: empty list" *)
    (*   | frequency xs = *)
    (*     let *)
    (*       fun loop ([], sum, iis) = (sum, List.rev iis) *)
    (*         | loop ((i, _) :: xs, sum, iis) = *)
    (*           loop (xs, sum + i, sum + i :: iis) *)

    (*       val (sum, iis) = loop (xs, 0, []) *)
    (*       val sum = LargeInt.fromInt sum *)
    (*       val iis = Seq.fromList iis *)

    (*       fun pick (_, []) = raise Fail "impossible" *)
    (*         | pick (n, (k, x) :: xs) = *)
    (*           if n <= k *)
    (*             then x *)
    (*             else pick (n - k, xs) *)

    (*       val index = map LargeInt.toInt (intInf_ (Range.constant 1 sum)) *)
    (*       val index = shrink (fn n => Seq.takeWhile (fn m => m < n) iis) index *)
    (*     in *)
    (*       bind index (fn n => pick (n, xs)) *)
    (*     end *)

    fun recursive f nonrec rec_ =
      sized (fn n =>
        if n <= 1
          then f nonrec
          else f (nonrec @ List.map small rec_))

    (* fun option g = *)
    (*   sized (fn n => *)
    (*     frequency [(2, pure NONE), (1 + n, map SOME g)]) *)

    (* local *)
    (*   fun atLeast 0 _ = true *)
    (*     | atLeast n xs = not (List.null (List.drop (xs, n))) *)
    (* in *)
    (*   fun list range g = *)
    (*     sized (fn size => *)
    (*       ensure (atLeast (Int.fromLarge (Range.lowerBound (range, size)))) *)
    (*         (bind (intInf_ range) (fn n => *)
    (*           replicate (Int.fromLarge n) g))) *)
    (* end *)
  end
