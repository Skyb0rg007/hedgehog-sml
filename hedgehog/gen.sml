(* gen.sml
 * © 2023 Skye Soss
 *)

structure Gen =
  struct
    structure M = MonadFn(
      struct
        type 'a t = int * SplitMix.t -> 'a Tree.t option

        fun pure x _ = SOME (Tree.singleton x)

        fun map f g args = Option.map (Tree.map f) (g args)

        fun bind g k (size, seed) =
          let
            val (s1, s2) = SplitMix.split seed
          in
            case g (size, s1) of
                NONE => NONE
              | SOME t => Tree.bindPartial (fn x => k x (size, s2)) t
          end
      end)

    open M

    fun sample g =
      let
        fun loop n =
          if n <= 0
            then raise Fail "Gen.sample: too many discards, unable to generate a sample"
          else
            let
              val rng = SplitMix.new ()
            in
              case g (30, rng) of
                  NONE => loop (n - 1)
                | SOME t => Tree.root t
            end
      in
        loop 100
      end

    fun printWith toString size seed g =
      case g (size, seed) of
          NONE =>
            TextIO.print "=== Outcome ===\n<discard>\n"
        | SOME t =>
            (TextIO.print "=== Outcome ===\n";
             TextIO.print (toString (Tree.root t));
             TextIO.print "\n=== Shrinks ===\n";
             Seq.app
              (fn child =>
                (TextIO.print (toString (Tree.root child));
                 TextIO.print "\n"))
              (Tree.children t))

    fun print toString g = printWith toString 30 (SplitMix.new ()) g

    fun printTreeWith toString size seed g =
      case g (size, seed) of
          NONE => TextIO.print "<discard>\n"
        | SOME t =>
            (TextIO.print (Tree.render toString t);
             TextIO.print "\n")

    fun printTree toString g = printTreeWith toString 30 (SplitMix.new ()) g

    fun delay f args = f () args

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

    val _: 'a t -> ('a * 'a t) t = freeze

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

    fun shrink' f g args =
      Option.map (Tree.expand f) (g args)

    fun shrink f = shrink' (Seq.fromList o f)

    fun prune g args =
      Option.map (Tree.singleton o Tree.root) (g args)

    local
      val halves = Seq.unfold
        (fn 0 => NONE
          | n => SOME (n, IntInf.quot (n, 2)))

      fun towards (dest, x) =
        if dest = x
          then Seq.empty
          else
            let
              val diff = IntInf.quot (x, 2) - IntInf.quot (dest, 2)
              val rest = Seq.map (fn y => x - y) (halves diff)
            in
              case Seq.head rest of
                  NONE => Seq.cons (dest, rest)
                | SOME y =>
                    if dest = y
                      then rest
                      else Seq.cons (dest, rest)
            end

      fun bst (bot, top) =
        let
          val shrinks = towards (bot, top)
          val children = Seq.zipWith bst (shrinks, Seq.drop 1 shrinks)
        in
          Tree.node (top, children)
        end

      fun buildTree (origin, x) =
        if x = origin
          then SOME (Tree.singleton x)
          else SOME (Tree.consChild (origin, bst (origin, x)))
    in
      fun intInf range (size, seed) =
        let
          val origin = Range.origin range
          val (lo, hi) = Range.bounds (range, size)
        in
          buildTree (origin, SplitMix.intInfRange (lo, hi) seed)
        end
    end

    fun int range = map Int.fromLarge (intInf range)
    fun int32 range = map Int32.fromLarge (intInf range)
    fun int64 range = map Int64.fromLarge (intInf range)
    fun position range = map Position.fromLarge (intInf range)

    fun word range = map Word.fromLargeInt (intInf range)
    fun word8 range = map Word8.fromLargeInt (intInf range)
    fun word32 range = map Word32.fromLargeInt (intInf range)
    fun word64 range = map Word64.fromLargeInt (intInf range)
    fun largeWord range = map LargeWord.fromLargeInt (intInf range)
    fun sysWord range = map SysWord.fromLargeInt (intInf range)

    local
      val halves = Seq.unfold
        (fn r =>
          if LargeReal.!= (r, 0.0) andalso LargeReal.isFinite r
            then SOME (r, r / 2.0)
            else NONE)

      fun towardsFloat dest x =
        if LargeReal.== (dest, x)
          then Seq.empty
          else Seq.map (fn y => x - y) (halves (x - dest))
    in
      fun largeReal range (size, seed) =
        let
          val origin = RealRange.origin range
          val (lo, hi) = RealRange.bounds (range, size)
          val r = SplitMix.realRange (lo, hi) seed
        in
          SOME (Tree.unfold (towardsFloat origin) r)
        end
    end

    fun real range = map (Real.fromLarge IEEEReal.TO_NEAREST) (largeReal range)
    fun real64 range = map (Real64.fromLarge IEEEReal.TO_NEAREST) (largeReal range)

    fun element [] = raise Fail "Gen.element: empty list"
      | element xs =
          map (fn n => List.nth (xs, n))
            (int (Range.constant 0 (List.length xs - 1)))

    fun choice [] = raise Fail "Gen.choice: empty list"
      | choice xs =
          bind (int (Range.constant 0 (List.length xs - 1)))
            (fn n => List.nth (xs, n))

    fun frequency [] = raise Fail "Gen.frequency: empty list"
      | frequency xs =
        let
          fun loop ([], sum, iis) = (sum, List.rev iis)
            | loop ((i, _) :: xs, sum, iis) =
              loop (xs, sum + i, sum + i :: iis)

          val (sum, iis) = loop (xs, 0, [])

          fun pick (_, []) = raise Fail "impossible"
            | pick (n, (k, x) :: xs) =
              if n <= k
                then x
                else pick (n - k, xs)

          val range = Range.constant 1 sum
          val index = prune (int range)
          val index = shrink (fn n => List.filter (fn m => m < n) iis) index
        in
          bind index (fn n => pick (n, xs))
        end

    fun recursive f nonrec rec_ =
      sized (fn n =>
        if n <= 1
          then f nonrec
          else f (nonrec @ List.map small rec_))

    fun option g =
      sized (fn n =>
        frequency [(2, pure NONE), (1 + n, map SOME g)])

    local
      fun atLeast (0 : IntInf.int) _ = true
        | atLeast _ [] = false
        | atLeast n (_ :: xs) = atLeast (n - 1) xs

      fun interleave t = Option.map (Tree.interleave o Tree.root) t
    in
      fun list range gen =
        sized (fn size =>
          ensure (atLeast (Range.lowerBound (range, size)))
            (bind (prune (int range))
              (fn n =>
                interleave o replicate n (Option.map Tree.singleton o gen))))
    end

    fun either (g1, g2) =
      sized (fn size =>
        frequency [(2, map Either.INL g1), (1 + size, map Either.INR g2)])

    val bool = map (fn 0 => false | _ => true) (int (Range.constant 0 1))

    fun subsequence xs =
      shrink' Tree.shrinkList
        (filterM (fn _ => bool) xs)

    val binDigit = map Char.chr (int (Range.constant 48 49))
    val octDigit = map Char.chr (int (Range.constant 48 55))
    val digit = map Char.chr (int (Range.constant 48 57))
    val hexDigit = element (String.explode "0123456789aAbBcCdDeEfF")
    val lower = map Char.chr (int (Range.constant 97 122))
    val upper = map Char.chr (int (Range.constant 65 90))
    val alpha = element
      (String.explode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val alphaNum = element
      (String.explode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
    val ascii = map Char.chr (int (Range.constant 0 127))
    val latin1 = map Char.chr (int (Range.constant 0 255))
    val unicode =
      frequency
        [(0xD800, word (Range.constant 0x0000 0xD7FF)),
         (0x1FFE, word (Range.constant 0xE000 0xFFFD)),
         (0x100000, word (Range.constant 0x010000 0x10FFFF))]
    val unicodeAll =
      word (Range.constant 0 0x10FFFF)

    fun string range char = map String.implode (list range char)

    fun utf8 range wchar =
      raise Fail "NYI"

    local
      fun unique cmp n g =
        let
          fun try k m =
            if k > 100
              then discard
              else
                bind (replicate n (freeze g)) (fn kvs =>
                  let
                    val kvs' = List.map (fn ((k, _), g) => (k, g)) kvs
                    val m' = Map.insertUntil (m, kvs', n)
                  in
                    if Map.size m' >= n
                      then pure (Map.elems m')
                      else try (k + 1) m'
                  end)
        in
          try 0 (Map.empty cmp)
        end
    in

      fun assocList cmp range g =
        sized (fn size =>
          let
            val lb = IntInf.toInt (Range.lowerBound (range, size))
          in
            ensure (fn assoc => List.length assoc >= lb)
              (bind
                (shrink' Tree.shrinkList
                  (bind (int range)
                    (fn k => unique cmp k g)))
                sequence)
          end)

      fun uniqueList cmp n g =
        map (List.map #1)
          (assocList cmp n (map (fn k => (k, ())) g))

    end
    
    local
      datatype 'a subterms
        = One of 'a
        | All of 'a list

      fun shrinkSubterms (One _) = []
        | shrinkSubterms (All xs) = List.map One xs

      fun genSubterms gs =
        bind (shrink shrinkSubterms (map All (mapM (map #2 o freeze) gs)))
          (fn One m => map One m
            | All xs => map All (sequence xs))
    in
      fun subterms gs f =
        bind (genSubterms gs)
          (fn One x => pure x
            | All xs => f xs)

      fun subterm g f =
        bind (genSubterms [g])
          (fn One x => pure x
            | All [x] => f x
            | All _ => raise Fail "impossible")

      fun subterm2 g1 g2 f =
        bind (genSubterms [g1, g2])
          (fn One x => pure x
            | All [x, y] => f (x, y)
            | All _ => raise Fail "impossible")
    end
  end
