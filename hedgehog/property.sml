
structure Property =
  struct

    (* Whether a test is covered by a classifier *)
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

    (* The extent to which a test is covered by a classifier *)
    structure Label :
      sig
        type 'a t

        val make : string * real * 'a -> 'a t

        val name : 'a t -> string
        val minimum : 'a t -> real
        val annotation : 'a t -> 'a

        val map : ('a -> 'b) -> 'a t -> 'b t
        val combineWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
        val covered : int t * int -> bool
        val bounds : int t * int * LargeInt.int -> Real64.real * Real64.real
      end =
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

    (* Mapping from label to values.
     * Indicates the extent to which all the classifiers cover a test *)
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

    structure Log =
      struct
        datatype t
          = ForAll of string * string
          | Annotation of string
          | Footnote of string
          | Cover of Cover.t Coverage.t
          | Error of exn
      end

    datatype log
      = ForAll of string * string
      | Annotation of string
      | Footnote of string
      | Cover of Cover.t Coverage.t
      | Error of exn

    datatype termination
      = Early of LargeInt.int * int
      | NoEarly of LargeInt.int * int
      | NoConfidence of int

    type config = {
        discardLimit : int ref,
        shrinkLimit : int ref,
        shrinkRetries : int ref,
        termination : termination ref
      }

    type 'a t = (log list * 'a option) Gen.t

    datatype property = Property of unit t * config

    structure M = MonadFn(
      struct
        type 'a t = 'a t

        fun pure a = Gen.pure ([], SOME a)

        fun map f =
          Gen.map (fn (j, x) => (j, Option.map f x)
            handle e => (j @ [Error e], NONE))

        fun bind p f =
          Gen.bind p (fn (j, x) =>
            case x of
                NONE => Gen.pure (j, NONE)
              | SOME a => Gen.map (fn (j', b) => (j @ j', b)) (f a)
                  handle e => Gen.pure (j @ [Error e], NONE))
      end)

    open M


    val delay = Gen.delay
    val discard = Gen.discard

    fun writeLog log = Gen.pure ([log], SOME ())

    fun cover (name, minimum, covered) =
      let
        val lbl = Label.make (name, minimum, Cover.fromBool covered)
      in
        writeLog (Cover (Coverage.fromLabel lbl))
      end

    fun classify (name, covered) = cover (name, 0.0, covered)

    fun label name = cover (name, 0.0, true)

    fun annotate message = writeLog (Annotation message)

    fun fromGen g = Gen.map (fn x => ([], SOME x)) g

    fun forAllWith toString g =
      bind (fromGen g) (fn x =>
      bind (writeLog (Annotation (toString x))) (fn () =>
      pure x))

    fun property p =
      let
        val config = {
            discardLimit = ref 100,
            shrinkLimit = ref 1000,
            shrinkRetries = ref 0,
            termination = ref (NoConfidence 100)
          }
      in
        Property (p, config)
      end

    fun setConfidence (Property (_, {termination = t, ...}), c) =
      case !t of
          NoEarly (_, tests) => t := NoEarly (c, tests)
        | NoConfidence tests => t := NoEarly (c, tests)
        | Early (_, tests) => t := Early (c, tests)

    fun verifyTermination (Property (_, {termination = t, ...})) =
      case !t of
          NoEarly (c, tests) => t := Early (c, tests)
        | NoConfidence tests => t := Early (1000000000, tests)
        | Early (c, tests) => t := Early (c, tests)

    fun setTestLimit (Property (_, {termination = t, ...}), tests) =
      case !t of
          NoEarly (c, _) => t := NoEarly (c, tests)
        | NoConfidence _ => t := NoConfidence tests
        | Early (c, _) => t := Early (c, tests)

    fun setDiscardLimit (Property (_, {discardLimit, ...}), n) =
      discardLimit := n

    fun setShrinkLimit (Property (_, {shrinkLimit, ...}), n) =
      shrinkLimit := n

    fun setRetries (Property (_, {shrinkRetries, ...}), n) =
      shrinkRetries := n

    datatype status
      = Failed of {shrinks : int, log : log list}
      | GaveUp
      | OK

    datatype report = Report of {
        successes : int,
        discards : int,
        coverage : int Coverage.t,
        status : status
      }

    fun takeSmallest (n, lim, t, p, e) =
      if n < lim andalso p (Tree.root t)
        then
          case Seq.find (p o Tree.root) (Tree.children t) of
              NONE => e (n, Tree.root t)
            | SOME m => takeSmallest (n + 1, lim, m, p, e)
        else e (n, Tree.root t)

    fun takeSmallest' (n, lim, t) =
      let
        (* fun p NONE = false *)
        (*   | p (SOME _) = true *)
        fun p (_, opt) = Option.isSome opt

        fun e (shrinks, (j, NONE)) =
            Failed {shrinks = shrinks, log = j}
          | e (shrinks, (j, SOME ())) =
            Failed {shrinks = shrinks, log = j}
      in
        takeSmallest (n, lim, t, p, e)
      end

    val _ : int * int * (log list * unit option) Tree.t -> status = takeSmallest'

    fun report (p : unit t) =
      let
        val size = 30
        val sizeInc = 2
        val seed = SplitMix.new ()
        val testLimit = 100
        val discardLimit = 100
        val shrinkLimit = 100

        fun loop (successes, discards, size, seed, coverage) =
          if successes >= testLimit
            then
              let
                val fs = Coverage.coverageFailures (coverage, successes)
              in
                Report {
                  successes = successes,
                  discards = discards,
                  coverage = coverage,
                  status =
                    if List.null fs
                      then OK
                      else Failed {shrinks = 0, log = [Annotation "Insufficient coverage."]}
                }
              end
          else if discards >= discardLimit
            then
              Report {
                successes = successes,
                discards = discards,
                coverage = coverage,
                status = GaveUp
              }
          else
            let
              val (seed1, seed2) = SplitMix.split seed
              val res : (log list * unit option) Tree.t option = p (size, seed1)
                handle e => SOME (Tree.singleton ([], NONE))
            in
              case Option.map Tree.root res of
                  NONE => loop (successes, discards + 1, size + sizeInc, seed2, coverage)
                | SOME (j, NONE) =>
                    Report {
                      successes = successes,
                      discards = discards,
                      coverage = coverage,
                      status = takeSmallest' (0, shrinkLimit, Option.valOf res)
                    }
                | SOME (j, SOME ()) =>
                    let
                      fun getCoverage (Cover c) = SOME c
                        | getCoverage _ = NONE
                      val c =
                        Coverage.unionsWith Cover.+ (List.mapPartial getCoverage j)
                      val coverage' =
                        Coverage.unionWith op + (Coverage.count c, coverage)
                    in
                      loop (successes + 1, discards, size + sizeInc, seed2, coverage')
                    end
            end
      in
        loop (0, 0, size, seed, Coverage.empty)
      end

    fun run _ = ()
  end
