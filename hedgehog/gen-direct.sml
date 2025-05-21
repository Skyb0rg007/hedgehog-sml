
structure GenDirect =
struct

structure Cont = SMLofNJ.Cont

datatype t = T of unit Property.t Cont.cont list ref * ((unit -> unit Property.t) -> unit)

fun new () : t =
  let
    val abort = ref (fn _ => ())
    val pstack = ref []
    val v =
      Cont.callcc (fn k => (abort := Cont.throw k; Cont.throw k NONE))
  in
    case v of
        NONE => T (pstack, !abort o SOME)
      | SOME f =>
          let
            val r = f ()
          in
            case !pstack of
                [] => raise Fail "Missing prompt"
              | h :: t => (pstack := t; Cont.throw h r)
          end
  end

fun shift f (T (pstack, abort)) =
  Cont.callcc (fn k =>
    (abort
      (fn () =>
        f (fn v =>
          Cont.callcc (fn k' =>
            pstack := k' :: !pstack;
            Cont.throw k v)));
      raise Fail ""))

fun lift gen t =
  shift (fn k => Property.bind gen k) t

fun run thunk =
  let
    val t as T (pstack, abort) = new ()
  in
    Cont.callcc
      (fn k =>
        (pstack := k :: !pstack;
         abort (fn () => Property.pure (thunk t));
         raise Fail ""))
  end

end
