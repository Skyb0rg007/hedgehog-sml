
structure IEEEReal =
  struct
    datatype rounding_mode
      = TO_NEAREST
      | TO_POSINF
      | TO_NEGINF
      | TO_ZERO
  end

structure Real =
  struct
    fun fromLargeInt n =
      Option.valOf (Real.fromString (LargeInt.toString n))

    fun fromLarge (_ : IEEEReal.rounding_mode) (n : real) = n

    fun toLargeInt mode x =
      if Real.isNan x
        then raise Domain
      else if not (Real.isFinite x)
        then raise Overflow
      else
        let
          val x =
            case mode of
                IEEEReal.TO_POSINF => Real.realCeil x
              | IEEEReal.TO_NEGINF => Real.realFloor x
              | IEEEReal.TO_NEAREST => Real.realRound x
              | IEEEReal.TO_ZERO => Real.realTrunc x
          val realfmt = StringCvt.FIX (SOME 0)
        in
          if Real.isFinite x
            then Option.valOf (LargeInt.fromString (Real.fmt realfmt x))
          else raise Overflow
        end

    open Real
  end

structure Real64 = Real
structure LargeReal = Real

structure Either =
struct
  datatype ('a, 'b) either = INL of 'a | INR of 'b
end
