(* map.sml
 * © 2023 Skye Soss
 *
 * This module implements a simple associative map for internal use.
 * The comparison function is passed as input to each operation to avoid
 * storing a closure in a value.
 * Currently, the interface is implemented with an ordered list.
 *)

structure Map : sig

  type ('k, 'v) map

  (* The empty map *)
  val empty : ('k * 'k -> order) -> ('k, 'v) map

  (* Number of entries in the map *)
  val size : ('k, 'v) map -> int

  (* The values in the map (in sorted order) *)
  val elems : ('k, 'v) map -> 'v list

  (* Insert a new value at the given key, replacing any existing value *)
  val insert : ('k, 'v) map * 'k * 'v -> ('k, 'v) map

  (* Insert each of the key-value pairs into the map, stopping once the map
   * has the given size or if the list is exhaused, whichever occurs first.
   * Returns the modified map *)
  val insertUntil : ('k, 'v) map * ('k * 'v) list * int -> ('k, 'v) map

end =
struct

  datatype ('k, 'v) map = Map of ('k * 'k -> order) * int * ('k * 'v) list

  fun size (Map (_, n, _)) = n

  fun elems (Map (_, _, kvs)) = List.map #2 kvs

  fun empty cmp = Map (cmp, 0, [])

  fun insert (Map (cmp, n, kvs), k, v) =
    let
      fun loop [] = ([(k, v)], true)
        | loop ((k', v') :: kvs) =
          case cmp (k, k') of
              LESS => ((k, v) :: (k', v') :: kvs, true)
            | EQUAL => ((k, v) :: kvs, false)
            | GREATER =>
                let
                  val (kvs', b) = loop kvs
                in
                  ((k', v') :: kvs', b)
                end

      val (kvs', added) = loop kvs
      val size' = if added then n + 1 else n
    in
      Map (cmp, size', kvs')
    end

  fun insertUntil (m, kvs, n) =
    if size m >= n
      then m
    else
      case kvs of
          [] => m
        | (k, v) :: kvs =>
            insertUntil (insert (m, k, v), kvs, n)

end
