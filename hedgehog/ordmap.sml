(* ordmap.sml
 * © 2023 Skye Soss
 *
 * A simple mutable ordered map implementation for internal use
 *)

structure OrdMap :
sig

  type ('k, 'v) t

  val new : unit -> ('k, 'v) t
  val size : ('k, 'v) t -> int
  val insert : ('k * 'k -> order) -> ('k, 'v) t * 'k * 'v -> unit
  val toList : ('k, 'v) t -> ('k * 'v) list
  val elems : ('k, 'v) t -> 'v list

end =
struct

  (* Current implementation is based on AA trees *)
  datatype ('k, 'v) node
    = Nil
    | Bin of int ref * ('k, 'v) node ref * 'k * 'v ref * ('k, 'v) node ref

  datatype ('k, 'v) t = Tree of {root : ('k, 'v) node ref, size : int ref}

  fun skew t =
    case !t of
        Nil => ()
      | Bin (lvl, l, _, _, r) =>
          case !l of
              Nil => ()
            | left as Bin (llvl, _, _, _, lr) =>
                if !lvl = !llvl
                  then (l := !lr; lr := !t; t := left)
                  else ()

  fun split t =
    case !t of
        Nil => ()
      | Bin (lvl, _, _, _, r) =>
          case !r of
              Nil => ()
            | right as Bin (rlvl, rl, _, _, rr) =>
                case !rr of
                    Nil => ()
                  | Bin (rrlvl, _, _, _, _) =>
                      if !lvl = !rrlvl
                        then (r := !rl; rl := !t; rlvl := !rlvl + 1; t := right)
                        else ()

  fun new () = Tree {root = ref Nil, size = ref 0}

  fun size (Tree {size, ...}) = !size

  fun insert cmp (Tree {root, size}, k, v) =
    let
      fun loop t =
        case !t of
            Nil =>
              (t := Bin (ref 1, ref Nil, k, ref v, ref Nil);
               size := !size + 1)
          | Bin (_, l, k', v', r) =>
              case cmp (k, k') of
                  EQUAL => v' := v
                | LESS => (loop l; skew t; split t)
                | GREATER => (loop r; skew t; split t)
    in
      loop root
    end

  fun toList (Tree {root, ...}) =
    let
      fun loop (t, acc) =
        case !t of
            Nil => acc
          | Bin (_, l, k, v, r) =>
              loop (l, (k, !v) :: loop (r, acc))
    in
      loop (root, [])
    end

  fun elems (Tree {root, ...}) =
    let
      fun loop (t, acc) =
        case !t of
            Nil => acc
          | Bin (_, l, _, v, r) =>
              loop (l, !v :: loop (r, acc))
    in
      loop (root, [])
    end

end
