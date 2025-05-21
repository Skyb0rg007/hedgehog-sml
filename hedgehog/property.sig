(* property.sig
 * © 2023 Skye Soss
 *)

signature PROPERTY =
  sig
    include MONAD

    type property

    val delay : (unit -> 'a t) -> 'a t
    val discard : 'a t
    
    (** Code coverage **)

    (* `cover (name, minimum, covered)` *)
    val cover : string * real * bool -> unit t

    (* `classify (name, covered)` == `cover (name, 0.0, covered)` *)
    val classify : string * bool -> unit t

    (* `label name` == `classify (name, true)` *)
    val label : string -> unit t

    (** Annotations **)
    val annotate : string -> unit t

    (** Generating inputs **)
    (* `forAllWith toString generator` *)
    val forAllWith : ('a -> string) -> 'a Gen.t -> 'a t

    (** Running and configuring properties **)

    (* Convert the testing monad to a property *)
    val property : unit t -> property

    (* Run the property, printing results to stdout *)
    val run : property -> unit

    (* Ensure result is statistically significant *)
    val setConfidence : property * LargeInt.int -> unit

    (* Number of times a property is executed for each shrink before trying another
     * Default 0 *)
    val setRetries : property * int -> unit

    (* Number of times a property is allowed to discard before giving up
     * Default 100 *)
    val setDiscardLimit : property * int -> unit

    (* Number of times a property is allowed to shrink before printing a counterexample
     * Default 1000 *)
    val setShrinkLimit : property * int -> unit

    (* Number of times a property is executed before it's considered successful
     * Default 100 *)
    val setTestLimit : property * int -> unit

    (* Bail out if the property isn't going to hit its requirements *)
    val verifyTermination : property -> unit
  end
