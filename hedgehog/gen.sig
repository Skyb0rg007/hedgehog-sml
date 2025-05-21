(* gen.sig
 * © 2023 Skye Soss
 *
 * Interface to the generator monad.
 *)

signature GEN =
sig
  (* Monadic interface.
   * See signature definition in monad.fun for documentation *)
  include MONAD

  (* `delay f`
   * Create a delayed generator from a thunk
   * The provided function is called at generation time, so this can be used
   * to create recursive generators
   *
   * Example:
   *
   *   datatype t1 = T1 of int * t2 list
   *   and t2 = T2 of int * t1 list
   *
   *   fun genT1 () =
   *     Gen.map2 T1 (Gen.int range, Gen.list range (Gen.delay genT2))
   *
   *   and genT2 () =
   *     Gen.map2 T2 (Gen.int range, Gen.list range (Gen.delay genT1))
   *)
  val delay : (unit -> 'a t) -> 'a t

  (***************************************************************************
   *
   * Sampling
   *
   ***************************************************************************)

  (* Generate a sample using a random seed and default size of 30 *)
  val sample : 'a t -> 'a

  (* Debugging - print a randomly generated value, along with its shrinks
   * `print` and `printWith` show first-level shrinks
   * `printTree` and `printTreeWith` show the entire shrink tree
   * Note that this may not terminate for massive shrink trees
   * `print{Tree}With` functions also take the size and seed to use
   *)
  val print : ('a -> string) -> 'a t -> unit
  val printWith : ('a -> string) -> int -> SplitMix.t -> 'a t -> unit
  val printTree : ('a -> string) -> 'a t -> unit
  val printTreeWith : ('a -> string) -> int -> SplitMix.t -> 'a t -> unit

  (***************************************************************************
   *
   * Size
   *
   ***************************************************************************)

  (* Generators have a notion of the current size parameter, which is used
   * as a hint for how large of an output to generate.
   * This is necessary to prevent recursive generators from getting stuck
   * in a loop - such combinators will decrease the size on recursive calls. *)

  (* Make a generator smaller by scaling the size parameter *)
  val small : 'a t -> 'a t

  (* Constructs a generator which depends on the size parameter
   * Much like `delay`, the provided function is called at generation time *)
  val sized : (int -> 'a t) -> 'a t

  (* Adjust the size parameter by applying it to the given function *)
  val scale : (int -> int) -> 'a t -> 'a t

  (* Overrides the size parameter for a given generator *)
  val resize : int -> 'a t -> 'a t

  (***************************************************************************
   *
   * Integral
   *
   ***************************************************************************)

  (* Generate a random integral value in the provided `Range` *)

  val int : Range.t -> int t
  val intInf : Range.t -> IntInf.int t
  val int32 : Range.t -> Int32.int t
  val int64 : Range.t -> Int64.int t
  val position : Range.t -> Position.int t
  val word : Range.t -> Word.word t
  val word8 : Range.t -> Word8.word t
  val word32 : Range.t -> Word32.word t
  val word64 : Range.t -> Word64.word t
  val largeWord : Range.t -> LargeWord.word t
  val sysWord : Range.t -> SysWord.word t

  (***************************************************************************
   *
   * Floating-point
   *
   ***************************************************************************)

  val real : RealRange.t -> real t
  val real64 : RealRange.t -> Real64.real t
  val largeReal : RealRange.t -> LargeReal.real t

  (***************************************************************************
   *
   * Characters
   *
   ***************************************************************************)

  (* #"0" or #"1" *)
  val binDigit : char t

  (* #"0" .. #"7" *)
  val octDigit : char t

  (* #"0" .. #"9" *)
  val digit : char t

  (* [0-9a-fA-F] *)
  val hexDigit : char t

  (* [a-z] *)
  val lower : char t

  (* [A-Z] *)
  val upper : char t

  (* [a-zA-Z] *)
  val alpha : char t

  (* [a-zA-Z0-9] *)
  val alphaNum : char t

  (* #"\0" - #"\127" *)
  val ascii : char t

  (* #"\0" - #"\255" *)
  val latin1 : char t

  (* Generates unicode codepoints.
   * Excludes noncharacters (U+FFFE, U+FFFF), surrogates (U+D800 - U+DFFF). *)
  val unicode : word t

  (* Generates any unicode codepoint (U+0000 - U+10FFFF). *)
  val unicodeAll : word t

  (***************************************************************************
   *
   * Strings
   *
   ***************************************************************************)

  (* Generates a string using a `Range` to determine the length. *)
  val string : Range.t -> char t -> string t

  (* Generates a utf8-encoded string using a `Range` to specify
   * the number of codepoints. *)
  val utf8 : Range.t -> Word32.word t -> string t

  (***************************************************************************
   *
   * Choice
   *
   ***************************************************************************)

  (* Generator that always produces the same element. Alias for `pure`. *)
  val constant : 'a -> 'a t

  (* Randomly selects one of the elements in the list.
   * This shrinks towards the first element in the list.
   * Note: This function raises `Fail` when provided an empty list *)
  val element : 'a list -> 'a t

  (* Randomly selects one of the generators in the list.
   * This shrinks towards the first generator in the list.
   * Note: This function raises `Fail` when provided an empty list *)
  val choice : 'a t list -> 'a t

  (* Use a weighted dist. to randomly select one of the generators in the list
   * This shrinks towards the first generator in the list.
   * Note: This function raises `Fail` when provided an empty list *)
  val frequency : (int * 'a t) list -> 'a t

  (* Modifies combinators like `choice` so they can be used recursively.
   * This function is provided with a `choice`-like combinator,
   * a list of nonrecursive generators, and a list of recursive generators.
   * The resulting generator uses the size parameter to gradually
   * choose from the recursive list less and less often, ensuring termination.
   *
   * Example:
   *   datatype exp =
   *     = Int of int
   *     | Var of string
   *     | Plus of exp * exp
   *     | Times of exp * exp
   *
   *   fun genExp () =
   *     Gen.recursive Gen.choice
   *       (* Non-recursive subterms *)
   *       [Gen.map Int (Gen.int (Range.constant 0 10)),
   *        Gen.map Var (Gen.string (Range.constant 1 10) Gen.alpha)]
   *       (* Recursive subterms *)
   *       [Gen.map2 Plus (Gen.delay genExp, Gen.delay genExp),
   *        Gen.map2 Times (Gen.delay genExp, Gen.delay genExp)]
   *)
  val recursive : ('a t list -> 'a t) -> 'a t list -> 'a t list -> 'a t

  (***************************************************************************
   *
   * Primitive datatypes and Collections
   *
   ***************************************************************************)

  (* Generates a random boolean. Shrinks to `false`. *)
  val bool : bool t

  (* Generates `NONE` some of the time, based on the size parameter. *)
  val option : 'a t -> 'a option t

  (* Generates one of the two values.
   * As the size parameter increases, this generator becomes more and more
   * biased in favor of `Either.INR`. *)
  val either : 'a t * 'b t -> ('a, 'b) Either.either t

  (* Generates a list using a `Range` to determine the length. *)
  val list : Range.t -> 'a t -> 'a list t

  (* Generates a list of unique values using a `Range` to determine the length.
   * The resulting list is returned in ascending order.
   * Note: This may fail if the element generator is unable to produce enough
   * unique values. *)
  val uniqueList : ('a * 'a -> order) -> Range.t -> 'a t -> 'a list t

  (* Generates an association list using a `Range` to determine the length.
   * The resulting association list is returned in ascending order.
   * Note: same caveats as `uniqueList` *)
  val assocList : ('k * 'k -> order) -> Range.t -> ('k * 'v) t -> ('k * 'v) list t

  (***************************************************************************
   *
   * Combinations and Permutations
   *
   ***************************************************************************)

  (* Generates a random subsequence of the list
   * This shrinks towards `[]`, but in a very complicated way. *)
  val subsequence : 'a list -> 'a list t

  (* Generates a random permutation of the list
   * This shrinks towards the identity permutation (no shuffling) *)
  (* TODO *)
  (* val shuffle : 'a list -> 'a list t *)

  (***************************************************************************
   *
   * Subterms
   *
   ***************************************************************************)

  val subterm : 'a t -> ('a -> 'a t) -> 'a t
  val subterm2 : 'a t -> 'a t -> ('a * 'a -> 'a t) -> 'a t
  val subterms : 'a t list -> ('a list -> 'a t) -> 'a t

  (* Freeze the size and seed used by a generator so that the generated
   * value can be inspected.
   * This allows the shrinking the number of subterms before shrinking the
   * subterms themselves, ex. if you were implementing `subterms` yourself.
   * Note: This is a low-level combinator, you likely don't need to use this *)
  val freeze : 'a t -> ('a * 'a t) t

  (***************************************************************************
   *
   * Shrinking
   *
   ***************************************************************************)

  (* While the combinators in this module create shrink trees themselves,
   * sometimes one wants to add additional, custom shrinking attempts. *)

  (* Give a generator additional shrinking options.
   * This keeps the existing shrinks intact. *)
  val shrink : ('a -> 'a list) -> 'a t -> 'a t

  (* Same as `shrink`, but allows one to provide a lazy sequence.
   * This can be useful if shrinking is expensive,
   * though one should ensure that the generated lists are always finite. *)
  val shrink' : ('a -> 'a Seq.t) -> 'a t -> 'a t

  (* Remove a generator's shrink tree. *)
  val prune : 'a t -> 'a t

  (***************************************************************************
   *
   * Discarding
   *
   ***************************************************************************)

  (* While you should try to write generators that always succeed,
   * this is not always possible.
   * The discarding mechanism allows users to control when
   * generators fail to produce any outputs. *)

  (* Discards the generator *)
  val discard : 'a t

  (* Generate a value which satisfies the predicate.
   * Note: This function has a limit on the number of failures the inner
   * generator can have before the resulting generator fails as well.
   * If the predicate is rarely satisfied, try writing a generator
   * that satisfies the predicate by construction,
   * rather than relying on filtering. *)
  val filter : ('a -> bool) -> 'a t -> 'a t

  (* Generate a value for which the given function returns SOME.
   * Note: same caveats as `filter` *)
  val mapPartial : ('a -> 'b option) -> 'a t -> 'b t

  (* Try using the first generator, then the second if the first one fails. *)
  val or : 'a t * 'a t -> 'a t
end
