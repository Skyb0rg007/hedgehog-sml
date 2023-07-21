
signature FUNCTION =
  sig
    type ('a, 'b) t
  end

structure Function =
struct

  datatype ('a, 'b) iso = Iso of ('a -> 'b) * ('b -> 'a)
  datatype either = datatype Either.either

  datatype 'a observable = Observable of {
      toString : 'a -> string,
      eq : 'a * 'a -> bool,
      hash : 'a -> word
    }

  datatype ('a, 'b) table = Table of {
      get : 'a -> 'b option,
      size : ('b -> int) -> int,
      toString : unit -> string,
      trees : ('a * 'b Tree.t) list ref
    }

  datatype ('a, 'b) function_repr
    = Tbl of {
        table : ('a, 'b) table,
        gen : 'b Gen.t,
        toString : 'b -> string,
        default : 'b
      }
    | Map of (exn, exn) function_repr * ((exn -> exn) -> ('a -> 'b))

  datatype ('a, 'b) function = Function of ('a -> 'b) * ('a, 'b) function_repr

  fun apply (Function (f, _), x) = f x

  fun fromRepr' (Tbl {table = Table {get, ...}, default, ...}) =
    (fn x => Option.getOpt (get x, default))
    | fromRepr' (Map (rep, k)) = k (fromRepr' rep)

  fun fromRepr (Tbl {table = Table {get, ...}, default, ...}) =
    (fn x => Option.getOpt (get x, default))
    | fromRepr (Map (rep, k)) = k (fromRepr' rep)

  fun make rep = Function (fromRepr rep, rep)
end
