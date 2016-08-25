object MeasureEntropy extends App {

  val s = "measure measure here measure measure measure"

  def entropyValue(s: String) = {

    val m = s.split(" ").toList.groupBy((word: String) => word).mapValues(_.length.toDouble)
    var result: Double = 0.0;
    val len = s.split(" ").length;

    m map {
      case (key, value: Double) =>
        {
          var frequency: Double = value / len;
          result -= frequency * (scala.math.log(frequency) / scala.math.log(2));
        }
    }

    result;
  }

  println(entropyValue(s))
}
