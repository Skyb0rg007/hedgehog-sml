(* Binary confidence scoring *)

signature SCORE =
  sig
    (* `val (lo, hi) = wilsonBounds {count, positives, confidence}`
     *   count - number of trials performed
     *   positives - number of successful trials of those performed
     *   confidence - acceptable rate of false occurrances
     *   0 <= positives <= count
     *   0 < confidence
     *   Note: The inputs are converted to Real64.real, so numbers close to
     *   `Real64.maxFinite` will likely result in a raised `Domain` exception
     *
     *   (lo, hi) - range that the actual percentage could reasonably be in
     *   0.0 <= lo <= hi <= 1.0
     *
     * Ex. for count=100, positives=50, confidence=1,000,000, we get bounds
     *  lo=0.28 and hi=0.72
     * So if the expected percentage is 40% (we expect 40 positives),
     *   the wilson score says the result is within our requirements,
     *   and thus we shouldn't reject the hypothesis outright
     * But if the expected percentage is 25% (we expect 25 positives),
     *   then we can say that our expectations are not met (given confidence)
     *)
    val wilsonBounds :
      {positives : LargeInt.int, count : LargeInt.int, confidence : LargeInt.int}
      -> Real64.real * Real64.real
  end

structure Score : SCORE =
  struct

    (* Algorithm AS 241: The Percentage Points of the Normal Distribution *)
    val A0 : Real64.real = 3.3871328727963666080e0
    val A1 : Real64.real = 1.3314166789178437745e2
    val A2 : Real64.real = 1.9715909503065514427e3
    val A3 : Real64.real = 1.3731693765509461125e4
    val A4 : Real64.real = 4.5921953931549871457e4
    val A5 : Real64.real = 6.7265770927008700853e4
    val A6 : Real64.real = 3.3430575583588128105e4
    val A7 : Real64.real = 2.5090809287301226727e3
    val B1 : Real64.real = 4.2313330701600911252e1
    val B2 : Real64.real = 6.8718700749205790830e2
    val B3 : Real64.real = 5.3941960214247511077e3
    val B4 : Real64.real = 2.1213794301586595867e4
    val B5 : Real64.real = 3.9307895800092710610e4
    val B6 : Real64.real = 2.8729085735721942674e4
    val B7 : Real64.real = 5.2264952788528545610e3
    val C0 : Real64.real = 1.42343711074968357734e0
    val C1 : Real64.real = 4.63033784615654529590e0
    val C2 : Real64.real = 5.76949722146069140550e0
    val C3 : Real64.real = 3.64784832476320460504e0
    val C4 : Real64.real = 1.27045825245236838258e0
    val C5 : Real64.real = 2.41780725177450611770e~1
    val C6 : Real64.real = 2.27238449892691845833e~2
    val C7 : Real64.real = 7.74545014278341407640e~4
    val D1 : Real64.real = 2.05319162663775882187e0
    val D2 : Real64.real = 1.67638483018380384940e0
    val D3 : Real64.real = 6.89767334985100004550e~1
    val D4 : Real64.real = 1.48103976427480074590e~1
    val D5 : Real64.real = 1.51986665636164571966e~2
    val D6 : Real64.real = 5.47593808499534494600e~4
    val D7 : Real64.real = 1.05075007164441684324e~9
    val E0 : Real64.real = 6.65790464350110377720e0
    val E1 : Real64.real = 5.46378491116411436990e0
    val E2 : Real64.real = 1.78482653991729133580e0
    val E3 : Real64.real = 2.96560571828504891230e~1
    val E4 : Real64.real = 2.65321895265761230930e~2
    val E5 : Real64.real = 1.24266094738807843860e~3
    val E6 : Real64.real = 2.71155556874348757815e~5
    val E7 : Real64.real = 2.01033439929228813265e~7
    val F1 : Real64.real = 5.99832206555887937690e~1
    val F2 : Real64.real = 1.36929880922735805310e~1
    val F3 : Real64.real = 1.48753612908506148525e~2
    val F4 : Real64.real = 7.86869131145613259100e~4
    val F5 : Real64.real = 1.84631831751005468180e~5
    val F6 : Real64.real = 1.42151175831644588870e~7
    val F7 : Real64.real = 2.04426310338993978564e~15
    val SPLIT1 : Real64.real = 0.425
    val SPLIT2 : Real64.real = 5.0
    val CONST1 : Real64.real = 0.180625
    val CONST2 : Real64.real = 1.6

    fun inv_cdf (p : Real64.real) : Real64.real =
      let
        val () = if p <= 0.0 orelse p >= 1.0 then raise Domain else ()
        val q = p - 0.5
      in
        if Real64.abs q <= SPLIT1
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
              val r = Real64.Math.sqrt (~ (Real64.Math.ln r))
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
                    val res = num / den
                  in
                    if q < 0.0 then ~ res else res
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
                    val res = num / den
                  in
                    if q < 0.0 then ~ res else res
                  end
            end
      end

    (* https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval *)
    fun wilsonBounds {positives : LargeInt.int, count : LargeInt.int, confidence : LargeInt.int} =
      let
        val () = if count <= 0 then raise Domain else ()
        val () = if positives > count then raise Domain else ()
        val () = if confidence <= 0 then raise Domain else ()

        val p = Real64.fromLargeInt positives
        val n = Real64.fromLargeInt count
        val acceptance = 1.0 / Real64.fromLargeInt confidence

        val () = if Real64.isFinite p then () else raise Domain
        val () = if Real64.isFinite n then () else raise Domain
        val () = if Real64.isFinite acceptance then () else raise Domain

        val p_hat = p / n
        val z = inv_cdf (1.0 - acceptance / 2.0)

        val midpoint = p_hat + z * z / (2.0 * n)
        val denominator = 1.0 + z * z / n
        val radicand = p_hat * (1.0 - p_hat) / n + z * z / (4.0 * n * n)
        val offset = z * Real64.Math.sqrt radicand

        val low = (midpoint - offset) / denominator
        val high = (midpoint + offset) / denominator
      in
        (low, high)
      end
  end
