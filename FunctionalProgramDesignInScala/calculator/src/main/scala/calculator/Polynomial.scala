package calculator

object Polynomial {

  /*
  Δ = b^2 - 4ac
   */
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal{
      math.pow(b(),2) - (4 * a() * c())
    }
  }

  /*
  (-b ± √Δ) / 2a
   */
  def computeSolutions(a: Signal[Double],
                       b: Signal[Double],
                       c: Signal[Double],
                       delta: Signal[Double]
                      ): Signal[Set[Double]] = {
    Signal {
      delta() match {
        case d if d<0 => Set()
        case _ => Set(
          ((-1 * b()) + math.sqrt(delta())) / (2 * a()),
          ((-1 * b()) - math.sqrt(delta())) / (2 * a())
        )
      }
    }
  }
}
