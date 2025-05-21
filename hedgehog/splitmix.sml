(* splitmix.sml
 * © 2023 Skye Soss
 *
 * Fast Splittable Pseudorandom Number Generators
 * https://gee.cs.oswego.edu/dl/papers/oopsla14.pdf
 *)

structure SplitMix :> SPLITMIX =
  struct
    infix >> << andb orb xorb

    val op >> = Word64.>>
    val op << = Word64.<<
    val op andb = Word64.andb
    val op orb = Word64.orb
    val op xorb = Word64.xorb

    datatype t = T of {gamma: Word64.word, value: Word64.word}

    (* https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation *)
    fun popCount 0wxffffffffffffffff = 64
      | popCount w =
      let
        val m1 = 0wx5555555555555555
        val m2 = 0wx3333333333333333
        val m4 = 0wx0f0f0f0f0f0f0f0f
        val h01 = 0wx0101010101010101

        val w = w - ((w >> 0w1) andb m1)
        val w = (w andb m2) + ((w >> 0w2) andb m2)
        val w = (w + (w >> 0w4)) andb m4
      in
        Word64.toInt ((w * h01) >> 0w56)
      end

    (* https://en.wikipedia.org/wiki/Find_first_set#CLZ *)
    fun leadingZeros 0w0 = 64
      | leadingZeros w =
      let
        val (w, n) =
          if (w andb 0wxffffffff00000000) = 0w0
          then (w << 0w32, 32)
          else (w, 0)
        val (w, n) =
          if (w andb 0wxffff000000000000) = 0w0
          then (w << 0w16, n + 16)
          else (w, n)
        val (w, n) =
          if (w andb 0wxff00000000000000) = 0w0
          then (w << 0w8, n + 8)
          else (w, n)
        val (w, n) =
          if (w andb 0wxf000000000000000) = 0w0
          then (w << 0w4, n + 4)
          else (w, n)
        val (w, n) =
          if (w andb 0wxc000000000000000) = 0w0
          then (w << 0w2, n + 2)
          else (w, n)
        val n =
          if (w andb 0wx8000000000000000) = 0w0
          then n + 1
          else n
      in
        n
      end

    fun mix64 w =
      let
        val w = (w xorb (w >> 0w33)) * 0wxff51afd7ed558ccd
        val w = (w xorb (w >> 0w33)) * 0wxc4ceb9fe1a85ec53
      in
        w xorb (w >> 0w33)
      end

    fun mixGamma w =
      let
        val w = (w xorb (w >> 0w30)) * 0wxbf58476d1ce4e5b9
        val w = (w xorb (w >> 0w27)) * 0wx94d049bb133111eb
        val w = (w xorb (w >> 0w31))
        val w = w orb 0w1
        val n = popCount (w xorb (w >> 0w1))
      in
        if Int.< (n, 24)
        then w xorb 0wxaaaaaaaaaaaaaaaa
        else w
      end

    (** Exports **)

    fun fromWord64 w =
      let
        val goldenGamma = 0wx9e3779b97f4a7c15
      in
        T {gamma = mixGamma (w + goldenGamma), value = mix64 w}
      end

    fun split (T {gamma, value}) =
      let
        val v = gamma + value
        val g = gamma + v
      in
        (T {gamma = gamma, value = g},
         T {gamma = mixGamma g, value = mix64 v})
      end

    local
      val global : t option ref = ref NONE

      fun initial () =
        let
          val msec = Word64.fromLargeInt (Time.toMilliseconds (Time.now ()))
          val word =
              ((msec andb 0wxFF000000) >> 0w24)
            + ((msec andb 0wx00FF0000) >> 0w8)
            + ((msec andb 0wx0000FF00) << 0w8)
            + ((msec andb 0wx000000FF) << 0w24)
        in
          fromWord64 word
        end
    in
      fun new () =
        let
          val g =
            case !global of
                SOME g => g
              | NONE => initial ()
          val (g1, g2) = split g
        in
          global := SOME g1;
          g2
        end
    end

    fun word64 (T {gamma, value}) =
      let
        val v = gamma + value
      in
        (mix64 v, T {gamma = gamma, value = v})
      end

    (* Generates a random word less than `range`
     * Algorithm:
     *   1. Generate a 64-bit word
     *   2. Mask out bits until it has the same number of bits as `range`
     *   3. If less than range, return it. Otherwise repeat step 1. *)
    fun bitmaskWithRejection (range, gen) =
      let
        val ones = Word64.notb 0w0
        val mask = ones >> Word.fromInt (leadingZeros (range orb 0w1))

        fun go gen =
          let
            val (w, gen) = word64 gen
            val w = w andb mask
          in
            if w < range
              then (w, gen)
              else go gen
          end
      in
        go gen
      end

    (* Calculate the number of 64-bit words long the IntInt.int is *)
    fun intInfWordSize n =
      let
        fun go (0, acc) = acc
          | go (i, acc) = go (IntInf.~>> (i, 0w64), acc + 1)
      in
        go (n, 0)
      end

    (* Generate a given number of 64-bit words, joining them together to create
     * a long IntInf.int value *)
    fun nextWord64s (n, gen) =
      let
        fun go (0, acc, gen) = (acc, gen)
          | go (i, acc, gen) =
            let
              val (w, gen) = word64 gen
              val acc' = IntInf.orb (IntInf.<< (acc, 0w64), Word64.toLargeInt w)
            in
              go (i - 1, acc', gen)
            end
      in
        go (n, 0, gen)
      end

    (* Generate a random number in the inclusive range `[0, range]` *)
    fun nextIntInf' (range, gen) =
      let
        val n = intInfWordSize range
        val k = Word.fromInt (Word64.wordSize * n)
        val twoToK = IntInf.<< (1, k)
        val modTwoToKMask = twoToK - 1
        val t = IntInf.rem (twoToK - range, range)

        fun go gen =
          let
            val (x, gen) = nextWord64s (n, gen)
            val m = x * range
            val l = IntInf.andb (m, modTwoToKMask)
          in
            if l < t
              then go gen
              else IntInf.~>> (m, k)
          end
      in
        go gen
      end

    fun intInfRange (lo, hi) =
      case IntInf.compare (lo, hi) of
           GREATER => raise Fail "SplitMix.intInfRange: lo > hi"
         | EQUAL => (fn _ => lo)
         | LESS => fn gen =>
             let
               val limit = hi - lo
               val bounded =
                 if limit < Word64.toLargeInt (Word64.notb 0w0)
                   then
                     (* Optimized algorithm if limit fits in 64-bit word *)
                     let
                       val n = Word64.fromLargeInt limit
                       val (w, gen) = bitmaskWithRejection (n + 0w1, gen)
                     in
                       Word64.toLargeInt w
                     end
                   else nextIntInf' (limit + 1, gen)
             in
               (lo + bounded)
             end

    val w64ToReal = LargeReal.fromLargeInt o Word64.toLargeInt

    (* Generate a real between 0.0 and 1.0 *)
    fun nextReal01 gen =
      let
        val (w, _) = word64 gen
      in
        w64ToReal w / w64ToReal (Word64.notb 0w0)
      end

    fun realRange (lo, hi) gen =
      if lo > hi
        then raise Fail "SplitMix.realRange: lo > hi"
      else if LargeReal.== (lo, hi)
        then lo
      else if LargeReal.isFinite lo andalso LargeReal.isFinite hi
        then
          let
            val x = nextReal01 gen
          in
            x * lo + (1.0 - x) * hi
          end
      else
        raise Fail "SplitMix.realRange: lo and hi must be finite"

  end
