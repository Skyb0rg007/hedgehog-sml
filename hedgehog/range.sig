(* range.sig
 * © 2023 Skye Soss
 *
 * Representations of numeric ranges, parametrized by a size parameter
 *)

signature RANGE =
  sig
    (* Range type. Holds the origin, along with a way of calculating bounds *)
    datatype t = Range of IntInf.int * (int -> IntInf.int * IntInf.int)

    (* The starting point for the range - what the value shrinks towards *)
    val origin : t -> IntInf.int
    (* Get the upper and lower bounds for a given size parameter *)
    val bounds : t * int -> IntInf.int * IntInf.int
    val lowerBound : t * int -> IntInf.int
    val upperBound : t * int -> IntInf.int

    (* Apply the function to the range's origin and bounds *)
    val map : (IntInf.int -> IntInf.int) -> t -> t

    (** Common ways to choose how the range scales with the size parameter
     ** The variants with 2 arguments are passed lower and upper bounds
     ** Those with 3 arguments are passed origin, then lower and upper bounds
     **)

    (* Only one value - origin = lowerBound = upperBound *)
    val singleton : int -> t
    (* No scaling, origin = lowerBound *)
    val constant : int -> int -> t
    (* No scaling *)
    val constantFrom : int -> int -> int -> t
    (* Scales linearly, origin = lowerBound *)
    val linear : int -> int -> t
    (* Scales linearly *)
    val linearFrom : int -> int -> int -> t
    (* Scales exponentially *)
    val exponential : int -> int -> t
    (* Scales exponentially, origin = lowerBound *)
    val exponentialFrom : int -> int -> int -> t

    (* The underlying functions which operate on infinite precision ints *)
    val singleton' : IntInf.int -> t
    val constant' : IntInf.int -> IntInf.int -> t
    val constantFrom' : IntInf.int -> IntInf.int -> IntInf.int -> t
    val linear' : IntInf.int -> IntInf.int -> t
    val linearFrom' : IntInf.int -> IntInf.int -> IntInf.int -> t
    val exponential' : IntInf.int -> IntInf.int -> t
    val exponentialFrom' : IntInf.int -> IntInf.int -> IntInf.int -> t
  end

signature REAL_RANGE =
  sig
    datatype t = Range of LargeReal.real * (int -> LargeReal.real * LargeReal.real)

    val origin : t -> LargeReal.real
    val bounds : t * int -> LargeReal.real * LargeReal.real
    val lowerBound : t * int -> LargeReal.real
    val upperBound : t * int -> LargeReal.real

    val map : (LargeReal.real -> LargeReal.real) -> t -> t

    val singleton : real -> t
    val constant : real -> real -> t
    val constantFrom : real -> real -> real -> t
    val linear : real -> real -> t
    val linearFrom : real -> real -> real -> t
    val exponential : real -> real -> t
    val exponentialFrom : real -> real -> real -> t

    val singleton' : LargeReal.real -> t
    val constant' : LargeReal.real -> LargeReal.real -> t
    val constantFrom' : LargeReal.real -> LargeReal.real -> LargeReal.real -> t
    val linear' : LargeReal.real -> LargeReal.real -> t
    val linearFrom' : LargeReal.real -> LargeReal.real -> LargeReal.real -> t
    val exponential' : LargeReal.real -> LargeReal.real -> t
    val exponentialFrom' : LargeReal.real -> LargeReal.real -> LargeReal.real -> t
  end

