
signature FUNCTION =
  sig
    type ('a, 'b) t
  end

structure Function =
struct

  datatype either = datatype Either.either

  datatype 'a observable = Observable of {
      toString : 'a -> string,
      compare : 'a * 'a -> order
    }

  (* structure Table : *)
  (*   sig *)
  (*     type ('a, 'b) t *)

  (*     val create : *)
  (*       {observable : 'a observable, *)
  (*        toString : 'b -> string, *)
  (*        generate : 'b Gen.t, *)
  (*        size : int} *)
  (*        -> ('a, 'b) t Gen.t *)
  (*   end = *)
  (*   struct *)
  (*     datatype ('a, 'b) t = T of { *)
  (*         get : 'a -> 'b option, *)
  (*         toString : unit -> string, *)
  (*         trees : ('a * 'b Tree.t) list ref *)
  (*       } *)

  (*     fun create {observable, toString, generate, size} (gsize, seed) = *)
  (*       let *)
  (*         val Observable {toString = keyToString, compare} = observable *)

  (*         val seed = ref seed *)
  (*         (1* fun run *1) *) 

  (*         fun make (extend, tbl) = *)
  (*           let *)
  (*             val initial = *)
  (*               List.revMap *)
  (*                 (fn (k, v) => (k, Tree.singleton v)) *)
  (*                 (OrdMap.toList tbl) *)
  (*             val tree = ref initial *)

  (*             fun get k = *)
  (*               case OrdMap.lookup (tbl, k) of *)
  (*                   SOME v => SOME v *)
  (*                 | NONE => *)
  (*                     if not extend *)
  (*                       then NONE *)
  (*                       else *)
  (*                         raise Fail "" *)
  (*                         (1* let *1) *)
  (*                         (1*   val v = generate (gsize, ) *1) *)
  (*                         (1* in *1) *)
  (*                         (1* end *1) *)
  (*           in *)
  (*             () *)
  (*           end *)

  (*         val tbl = T { *)
  (*             get = fn _ => NONE, *)
  (*             toString = fn () => "", *)
  (*             trees = ref [] *)
  (*           } *)
  (*       in *)
  (*         SOME (Tree.singleton tbl) *)
  (*       end *)
  (*   end *)


  (* datatype ('a, 'b) function_repr *)
  (*   = Tbl of { *)
  (*       table : ('a, 'b) table, *)
  (*       gen : 'b Gen.t, *)
  (*       toString : 'b -> string, *)
  (*       default : 'b *)
  (*     } *)
  (*   | Map of (exn, exn) function_repr * ((exn -> exn) -> ('a -> 'b)) *)

  (* datatype ('a, 'b) function = Function of ('a -> 'b) * ('a, 'b) function_repr *)

  (* fun apply (Function (f, _), x) = f x *)

  (* fun fromRepr' (Tbl {table = Table {get, ...}, default, ...}) = *)
  (*   (fn x => Option.getOpt (get x, default)) *)
  (*   | fromRepr' (Map (rep, k)) = k (fromRepr' rep) *)

  (* fun fromRepr (Tbl {table = Table {get, ...}, default, ...}) = *)
  (*   (fn x => Option.getOpt (get x, default)) *)
  (*   | fromRepr (Map (rep, k)) = k (fromRepr' rep) *)

  (* fun make rep = Function (fromRepr rep, rep) *)
end
