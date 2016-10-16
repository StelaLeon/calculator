package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    //Δ = b² - 4ac
    def getDelta(a: Double,b: Double,c: Double): Double = {
      b*b-4*a*c
    }
    Signal(getDelta(a(),b(),c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    def getValues(a: Double, b :Double, c :Double, delta :Double) :Set[Double] = {
      val v1 = ((-1 * b) + Math.sqrt(Math.abs(delta)))/(2*a)
      val v2 = ((-1 * b) - Math.sqrt(Math.abs(delta)))/(2*a)

      Set(v1,v2)
    }
    Signal(getValues(a(),b(),c(),delta()))
  }
}
