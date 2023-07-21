(* splitmix-dieharder/test.sml
 * © 2023 Skye Soss
 *
 * Generates random numbers using SplitMix for use in `dieharder` benchmarks
 *
 * This module uses the following workflow:
 *   Split the generator
 *   Use one of the generators to provide 512 bytes of randomness
 *   Use the other to continue this process
 * If this workflow is sufficiently random, so should other workflows
 *)

structure Dieharder =
struct
  structure SplitMix = Hedgehog.SplitMix

  val initialSeed : Word64.word = 0wxdeadbeef

  val arr = Word8Array.array (512, 0w0)

  val maxWord64 = Word64.toLargeInt (Word64.notb 0w0)

  fun loop g =
    let
      val (g1, g2) = SplitMix.split g
      fun loop1 (g, i) =
        if i >= 512
          then ()
        else
          let
            val (n, g') = SplitMix.intInfRange (0, maxWord64) g
            val w64 = Word64.fromLargeInt (IntInf.toLarge n)
            fun update (i, shift) =
              let
                val w8 = Word64.andb (Word64.>> (w64, shift), 0wxFF)
              in
                Word8Array.update (arr, i, Word8.fromLarge (Word64.toLarge w8))
              end
          in
            update (i + 0, 0wx00);
            update (i + 1, 0wx08);
            update (i + 2, 0wx10);
            update (i + 3, 0wx18);
            update (i + 4, 0wx20);
            update (i + 5, 0wx28);
            update (i + 6, 0wx30);
            update (i + 7, 0wx38);
            loop1 (g', i + 8)
          end
    in
      loop1 (g1, 0);
      Posix.IO.writeArr (Posix.FileSys.stdout, Word8ArraySlice.full arr);
      loop g2
    end

  val () = loop (SplitMix.fromWord64 initialSeed)

end
