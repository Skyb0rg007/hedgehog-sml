(* range.sig
 * © 2023 Skye Soss
 *
 * Representations of numeric ranges
 *)

structure Range : RANGE =
  struct
    datatype t = Range of IntInf.int * (int -> IntInf.int * IntInf.int)

    fun origin (Range (z, _)) = z
    fun bounds (Range (_, f), sz) = f sz
    fun lowerBound (Range (_, f), sz) = #1 (f sz)
    fun upperBound (Range (_, f), sz) = #2 (f sz)
    fun map k (Range (z, f)) =
      Range (k z, fn sz =>
        let
          val (lb, ub) = f sz
        in
          (k lb, k ub)
        end)

    fun singleton' x = Range (x, fn _ => (x, x))

    fun constantFrom' z x y = Range (z, fn _ => (x, y))

    fun constant' x y = constantFrom' x x y

    fun scaleLinear (size, origin, n) =
      let
        val size = Int.max (0, Int.min (99, size))
        val delta = n - origin
        val sign = IntInf.fromInt (IntInf.sign delta)
        val range = delta + sign
        val diff = IntInf.quot (range * IntInf.fromInt size, 100)
      in
        origin + diff
      end

    fun linearFrom' z x y =
      Range (z, fn sz =>
        let
          val lo = IntInf.max (x, IntInf.min (y, scaleLinear (sz, z, x)))
          val hi = IntInf.max (x, IntInf.min (y, scaleLinear (sz, z, y)))
        in
          (lo, hi)
        end)

    fun linear' x y = linearFrom' x x y

    fun scaleExp (size, origin, n) =
      let
        val size = Int.max (0, Int.min (99, size))
        val delta = n - origin
        val sign = LargeReal.fromInt (IntInf.sign delta)
        val base = abs delta + 1
        val baseReal = LargeReal.fromLargeInt base
        val exptReal = LargeReal.fromInt size / 99.0
        val absDiff = LargeReal.Math.pow (baseReal, exptReal) - 1.0
        val diffReal = absDiff * sign
        val diffLarge = LargeReal.toLargeInt IEEEReal.TO_NEAREST diffReal
      in
        origin + diffLarge
      end

    fun exponentialFrom' z x y =
      Range (z, fn sz =>
        let
          val lo = IntInf.max (x, IntInf.min (y, scaleExp (sz, z, x)))
          val hi = IntInf.max (x, IntInf.min (y, scaleExp (sz, z, y)))
        in
          (lo, hi)
        end)

    fun exponential' x y = exponentialFrom' x x y

    fun singleton x = singleton' (IntInf.fromInt x)

    fun constantFrom z x y =
      constantFrom'
        (IntInf.fromInt z)
        (IntInf.fromInt x)
        (IntInf.fromInt y)

    fun constant x y =
      constant'
        (IntInf.fromInt x)
        (IntInf.fromInt y)

    fun linearFrom z x y =
      linearFrom'
        (IntInf.fromInt z)
        (IntInf.fromInt x)
        (IntInf.fromInt y)

    fun linear x y =
      linear'
        (IntInf.fromInt x)
        (IntInf.fromInt y)

    fun exponentialFrom z x y =
      exponentialFrom'
        (IntInf.fromInt z)
        (IntInf.fromInt x)
        (IntInf.fromInt y)

    fun exponential x y =
      exponential'
        (IntInf.fromInt x)
        (IntInf.fromInt y)
  end

structure RealRange : REAL_RANGE =
  struct
    datatype t = Range of LargeReal.real * (int -> LargeReal.real * LargeReal.real)

    fun origin (Range (z, _)) = z
    fun bounds (Range (_, f), sz) = f sz
    fun lowerBound (Range (_, f), sz) = #1 (f sz)
    fun upperBound (Range (_, f), sz) = #2 (f sz)
    fun map k (Range (z, f)) =
      Range (k z, fn sz =>
        let
          val (lb, ub) = f sz
        in
          (k lb, k ub)
        end)

    fun singleton' x = Range (x, fn _ => (x, x))

    fun constantFrom' z x y = Range (z, fn _ => (x, y))

    fun constant' x y = constantFrom' x x y

    fun scaleLinear (size, origin, n) =
      let
        val size = Int.max (0, Int.min (99, size))
        val diff = (n - origin) * (LargeReal.fromInt size / 99.0)
      in
        origin + diff
      end

    fun linearFrom' z x y =
      Range (z, fn sz =>
        let
          val lo = LargeReal.max (x, LargeReal.min (y, scaleLinear (sz, z, x)))
          val hi = LargeReal.max (x, LargeReal.min (y, scaleLinear (sz, z, y)))
        in
          (lo, hi)
        end)

    fun linear' x y = linearFrom' x x y

    fun scaleExp (size : int, origin : LargeReal.real, n : LargeReal.real) =
      let
        val size = Int.max (0, Int.min (99, size))
        val delta = n - origin
        val sign = LargeReal.fromInt (LargeReal.sign delta)
        val base = LargeReal.abs delta + 1.0
        val expt = LargeReal.fromInt size / 99.0
        val diff = (LargeReal.Math.pow (base, expt) - 1.0) * sign
      in
        origin + diff
      end

    fun exponentialFrom' z x y =
      Range (z, fn sz =>
        let
          val lo = LargeReal.max (x, LargeReal.min (y, scaleExp (sz, z, x)))
          val hi = LargeReal.max (x, LargeReal.min (y, scaleExp (sz, z, y)))
        in
          (lo, hi)
        end)

    fun exponential' x y = exponentialFrom' x x y

    fun singleton x = singleton' (Real.fromLarge IEEEReal.TO_NEAREST x)

    fun constantFrom z x y =
      constantFrom'
        (Real.fromLarge IEEEReal.TO_NEAREST z)
        (Real.fromLarge IEEEReal.TO_NEAREST x)
        (Real.fromLarge IEEEReal.TO_NEAREST y)

    fun constant x y =
      constant'
        (Real.fromLarge IEEEReal.TO_NEAREST x)
        (Real.fromLarge IEEEReal.TO_NEAREST y)

    fun linearFrom z x y =
      linearFrom'
        (Real.fromLarge IEEEReal.TO_NEAREST z)
        (Real.fromLarge IEEEReal.TO_NEAREST x)
        (Real.fromLarge IEEEReal.TO_NEAREST y)

    fun linear x y =
      linear'
        (Real.fromLarge IEEEReal.TO_NEAREST x)
        (Real.fromLarge IEEEReal.TO_NEAREST y)

    fun exponentialFrom z x y =
      exponentialFrom'
        (Real.fromLarge IEEEReal.TO_NEAREST z)
        (Real.fromLarge IEEEReal.TO_NEAREST x)
        (Real.fromLarge IEEEReal.TO_NEAREST y)

    fun exponential x y =
      exponential'
        (Real.fromLarge IEEEReal.TO_NEAREST x)
        (Real.fromLarge IEEEReal.TO_NEAREST y)
  end
