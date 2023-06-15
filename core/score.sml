(* Confidence scoring *)

signature SCORE =
  sig
    (* `val (lo, hi) = wilsonBounds {count, positives, confidence}`
     *   count - number of trials
     *   positives - number of successful trials
     *   confidence - acceptable rate of false occurrances
     *)
    val wilsonBounds :
      {positives : LargeInt.int, count : LargeInt.int, confidence : LargeInt.int}
      -> real * real
  end

structure Score : SCORE =
  struct

    (* Algorithm AS 241: The Percentage Points of the Normal Distribution *)
    local
      val A0 = 3.3871328727963666080e0
      val A1 = 1.3314166789178437745e2
      val A2 = 1.9715909503065514427e3
      val A3 = 1.3731693765509461125e4
      val A4 = 4.5921953931549871457e4
      val A5 = 6.7265770927008700853e4
      val A6 = 3.3430575583588128105e4
      val A7 = 2.5090809287301226727e3
      val B1 = 4.2313330701600911252e1
      val B2 = 6.8718700749205790830e2
      val B3 = 5.3941960214247511077e3
      val B4 = 2.1213794301586595867e4
      val B5 = 3.9307895800092710610e4
      val B6 = 2.8729085735721942674e4
      val B7 = 5.2264952788528545610e3
      val C0 = 1.42343711074968357734e0
      val C1 = 4.63033784615654529590e0
      val C2 = 5.76949722146069140550e0
      val C3 = 3.64784832476320460504e0
      val C4 = 1.27045825245236838258e0
      val C5 = 2.41780725177450611770e~1
      val C6 = 2.27238449892691845833e~2
      val C7 = 7.74545014278341407640e~4
      val D1 = 2.05319162663775882187e0
      val D2 = 1.67638483018380384940e0
      val D3 = 6.89767334985100004550e~1
      val D4 = 1.48103976427480074590e~1
      val D5 = 1.51986665636164571966e~2
      val D6 = 5.47593808499534494600e~4
      val D7 = 1.05075007164441684324e~9
      val E0 = 6.65790464350110377720e0
      val E1 = 5.46378491116411436990e0
      val E2 = 1.78482653991729133580e0
      val E3 = 2.96560571828504891230e~1
      val E4 = 2.65321895265761230930e~2
      val E5 = 1.24266094738807843860e~3
      val E6 = 2.71155556874348757815e~5
      val E7 = 2.01033439929228813265e~7
      val F1 = 5.99832206555887937690e~1
      val F2 = 1.36929880922735805310e~1
      val F3 = 1.48753612908506148525e~2
      val F4 = 7.86869131145613259100e~4
      val F5 = 1.84631831751005468180e~5
      val F6 = 1.42151175831644588870e~7
      val F7 = 2.04426310338993978564e~15
      val SPLIT1 = 0.425
      val SPLIT2 = 5.0
      val CONST1 = 0.180625
      val CONST2 = 1.6
    in
      fun inv_cdf p =
        let
          val () = if p <= 0.0 orelse p >= 1.0 then raise Domain else ()
          val q = p - 0.5
        in
          if Real.abs q <= SPLIT1
            then
              let
                val r = CONST1 - q * q
                val num = A7
                val num = num * r + A6
                val num = num * r + A5
                val num = num * r + A4
                val num = num * r + A3
                val num = num * r + A2
                val num = num * r + A1
                val num = num * r + A0
                val den = B7
                val den = den * r + B6
                val den = den * r + B5
                val den = den * r + B4
                val den = den * r + B3
                val den = den * r + B2
                val den = den * r + B1
                val den = den * r + 1.0
              in
                q * (num / den)
              end
            else
              let
                val r = if q <= 0.0 then p else 1.0 - p
                val r = Math.sqrt (~(Math.ln r))
              in
                if r <= SPLIT2
                  then
                    let
                      val r = r - CONST2
                      val num = C7
                      val num = num * r + C6
                      val num = num * r + C5
                      val num = num * r + C4
                      val num = num * r + C3
                      val num = num * r + C2
                      val num = num * r + C1
                      val num = num * r + C0
                      val den = D7
                      val den = den * r + D6
                      val den = den * r + D5
                      val den = den * r + D4
                      val den = den * r + D3
                      val den = den * r + D2
                      val den = den * r + D1
                      val den = den * r + 1.0
                    in
                      if q < 0.0 then ~(num / den) else num / den
                    end
                  else
                    let
                      val r = r - SPLIT2
                      val num = E7
                      val num = num * r + E6
                      val num = num * r + E5
                      val num = num * r + E4
                      val num = num * r + E3
                      val num = num * r + E2
                      val num = num * r + E1
                      val num = num * r + E0
                      val den = F7
                      val den = den * r + F6
                      val den = den * r + F5
                      val den = den * r + F4
                      val den = den * r + F3
                      val den = den * r + F2
                      val den = den * r + F1
                      val den = den * r + 1.0
                    in
                      if q < 0.0 then ~(num / den) else num / den
                    end
              end
        end
    end

    (* https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval *)
    fun wilsonBounds {positives, count, confidence} =
      let
        val () = if count <= 0 then raise Domain else ()
        val () = if positives > count then raise Domain else ()

        val p = Real.fromLargeInt positives
        val n = Real.fromLargeInt count
        val acceptance = 1.0 / Real.fromLargeInt confidence

        val p_hat = p / n
        val z = inv_cdf (1.0 - acceptance / 2.0)

        val midpoint = p_hat + z * z / (2.0 * n)
        val denominator = 1.0 + z * z / n
        val offset = z * Math.sqrt (p_hat * (1.0 - p_hat) / n + z * z / (4.0 * n * n))

        val low = (midpoint - offset) / denominator
        val high = (midpoint + offset) / denominator
      in
        (low, high)
      end
  end
