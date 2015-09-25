package a.baltic.scion.bitcoin

import a.baltic.scion.Hash
import scala.annotation.tailrec

/**
 * @author andrew
 */
object BlockLocator {

  def blockLocator(headers: Vector[Hash]) = {
    @tailrec
    def f(i: Int, start: Int, step: Int, result: Vector[Hash]): Vector[Hash] = {
      if (i <= 0) {
        result ++ Vector(headers(0))
      } else {
        val nextStep = if (start < 10) 1 else 2 * step
        f(i - step, start + 1, nextStep, result :+ headers(i))
      }
    }
    val topDepth = headers.length - 1
    f(topDepth, 0, 1, Vector.empty)
  }
}
