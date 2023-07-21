(* splitmix.sml
 * © 2023 Skye Soss
 *
 * Interface to the random number generation mechanism
 *)

signature SPLITMIX =
  sig
    (* Random number generator. This is a value type (no internal mutation) *)
    type t

    (* Create a random number generator from a seed integer *)
    val fromWord64 : Word64.word -> t

    (* Create a random number generator using current time as initialization *)
    val new : unit -> t

    (* Split a generator into two independent generators *)
    val split : t -> t * t

    (* Generate a random integer between given low and high
     * Returns the integer along with a new gernerator
     * raises `Fail` if `lo > hi` *)
    val intInfRange : IntInf.int * IntInf.int -> t -> IntInf.int * t

    (* Generate a random floating point number between given low and high
     * Returns the number along with a new gernerator
     * raises `Fail` if `lo > hi` or if either input is NaN or infinite *)
    val realRange : LargeReal.real * LargeReal.real -> t -> LargeReal.real * t
  end

