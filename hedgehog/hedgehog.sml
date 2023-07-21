(* hedgehog.sml
 * © 2023 Skye Soss
 *)

structure Hedgehog =
struct
  structure SplitMix = SplitMix
  structure Gen : GEN = Gen
  structure Range = Range
  structure Seq = Seq
  structure Property = Property
  structure Either = Either
  structure State = State
  structure Function = Function

  (* XXX: Internal *)
  structure Tree = Tree
  structure Score = Score
  structure Map = Map
  structure OrdMap = OrdMap

  (* /dev/urandom is the best source for initial randomness *)
  fun urandomSeed () =
    let
      infix add
      val stream = BinIO.openIn "/dev/urandom"
      val vec = BinIO.inputN (stream, 8)
      val () = BinIO.closeIn stream
      fun w add i =
        LargeWord.<< (w, 0w8) + Word8.toLarge (Word8Vector.sub (vec, i))
    in
      Word64.fromLarge (0w0 add 0 add 1 add 2 add 3 add 4 add 5 add 6 add 7)
    end

  (* https://github.com/haskellari/splitmix/blob/d2692c3c87189f663e350b0148060f9e51c66ae7/cbits-unix/init.c#L25 *)
  (* This should result in very good prng initialization *)
  fun timePidSeed () =
    let
      val pid = Posix.Process.pidToWord (Posix.ProcEnv.getpid ())
      val {elapsed, cutime, ...} = Posix.ProcEnv.times ()
      val sec = Time.toSeconds elapsed
      val usec = Time.toMicroseconds elapsed mod 1000000
      val cpu = Time.toMicroseconds cutime
    in
      Word64.xorb (Word64.fromLargeInt sec,
      Word64.xorb (Word64.fromLargeInt usec,
      Word64.xorb (Word64.<< (Word64.fromLargeInt cpu, 0w16),
      Word64.<< (Word64.fromLarge (SysWord.toLarge pid), 0w32))))
    end

  (* https://github.com/schmouk/PyRandLib/blob/1785fe367b76002ee087bedf1ebbb523cbb76d56/PyRandLib/fastrand32.py#L139 *)
  (* This should be completely platform-agnostic and never fail,
   * but doesn't have very good randomness qualities. *)
  fun timeSeed () =
    let
      val t = Time.now ()
      val msec = Word64.fromLargeInt (Time.toMilliseconds t)
    in
        Word64.>> (Word64.andb (msec, 0wxFF000000), 0w24)
      + Word64.>> (Word64.andb (msec, 0wx00FF0000), 0w8)
      + Word64.<< (Word64.andb (msec, 0wx0000FF00), 0w8)
      + Word64.<< (Word64.andb (msec, 0wx000000FF), 0w24)
    end

  val initialSeed =
    (urandomSeed () handle _ => timePidSeed ()) handle _ => timeSeed ()
end
