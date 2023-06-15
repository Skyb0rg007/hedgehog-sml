
signature HEDGEHOG_PROPERTY =
  sig
    type 'a t

    val pure : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    
    val cover : string * real * bool -> unit t
    val classify : string * bool -> unit t
    val label : string -> unit t
  end

structure Property : HEDGEHOG_PROPERTY =
  struct

    structure Cover =
      struct
        datatype t = Cover | NoCover

        fun Cover + _ = Cover
          | _ + Cover = Cover
          | NoCover + NoCover = NoCover

        fun fromBool true = Cover
          | fromBool false = NoCover

        fun toCount Cover = 1
          | toCount NoCover = 0
      end

    structure Label =
      struct
        datatype 'a t = T of string * real * 'a

        val make = T
        fun name (T (name, _, _)) = name
        fun minimum (T (_, min, _)) = min
        fun annotation (T (_, _, ann)) = ann

        fun map f (T (name, min, ann)) = T (name, min, f ann)

        fun combineWith f (T (_, _, x), T (name, min, y)) =
          T (name, min, f (x, y))

        fun covered (T (_, min, count), tests) =
          let
            val percentage = Real.fromInt count / Real.fromInt tests * 100.0
            val thousandths = Real.round (percentage * 10.0)
          in
            Real.fromInt thousandths / 10.0 >= min
          end

        fun bounds (T (_, min, ann), tests, confidence) =
          Score.wilsonBounds
            {positives = Int.toLarge ann,
             count = Int.toLarge tests,
             confidence = confidence}
      end

    structure Coverage =
      struct
        datatype 'a t = T of 'a Label.t list

        val empty = T []
        
        fun labels (T cs) = cs

        fun fromLabel lbl = T [lbl]

        fun count (T cs) = T (List.map (Label.map Cover.toCount) cs)

        fun unionWith f (T xs, T ys) =
          let
            fun loop ([], ys) = ys
              | loop (xs, []) = xs
              | loop (x :: xs, y :: ys) =
                case String.compare (Label.name x, Label.name y) of
                    LESS    => x :: loop (xs, y :: ys)
                  | GREATER => y :: loop (x :: xs, ys)
                  | EQUAL   => Label.combineWith f (x, y) :: loop (xs, ys)
          in
            T (loop (xs, ys))
          end

        fun unionsWith f = List.foldl (unionWith f) empty

        fun coverageFailures (T cs, tests) =
          List.filter (fn lbl => not (Label.covered (lbl, tests))) cs

        fun coverageSuccess (c, tests) =
          List.null (coverageFailures (c, tests))

        fun confidenceSuccess (T cs, tests, confidence) =
          List.all
            (fn lbl =>
              let
                val (lowerBound, _) = Label.bounds (lbl, tests, confidence)
              in
                lowerBound >= Label.minimum lbl / 100.0
              end)
            cs

        fun confidenceFailure (T cs, tests, confidence) =
          List.exists
            (fn lbl =>
              let
                val (_, upperBound) = Label.bounds (lbl, tests, confidence)
              in
                upperBound < Label.minimum lbl / 100.0
              end)
            cs
      end

    datatype log
      = ForAll of string * string
      | Info of string
      | Cover of Cover.t Coverage.t
      | Error of exn

    type 'a t = (log list * 'a option) Gen.t

    fun map f =
      Gen.map (fn (j, x) =>
        (j, Option.map f x) handle e => (j @ [Error e], NONE))

    fun bind p f =
      Gen.bind p (fn (j, x) =>
        case x of
            NONE => Gen.pure (j, NONE)
          | SOME a => Gen.map (fn (j', b) => (j @ j', b)) (f a)
              handle e => Gen.pure (j @ [Error e], NONE))

    fun pure a = Gen.pure ([], SOME a)

    fun writeLog log = Gen.pure ([log], SOME ())

    fun cover (name, minimum, covered) =
      let
        val lbl = Label.make (name, minimum, Cover.fromBool covered)
      in
        writeLog (Cover (Coverage.fromLabel lbl))
      end

    fun classify (name, covered) = cover (name, 0.0, covered)

    fun label name = cover (name, 0.0, true)

    fun fromGen g = Gen.map (fn x => ([], SOME x)) g

    fun forAllWith toString g =
      bind (fromGen g) (fn x =>
      bind (writeLog (Info (toString x))) (fn () =>
      pure x))

  end
