package traffic
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

object InputJSON {
  implicit val rw: RW[InputJSON] = macroRW
}
case class InputJSON(
    trafficMeasurements: List[MeasurementCollections]
)

object MeasurementCollections {
  implicit val rw: RW[MeasurementCollections] = macroRW
}
case class MeasurementCollections(
    measurementTime: Double,
    measurements: List[Measurement]
)

object Measurement {
  implicit val rw: RW[Measurement] = macroRW
}
case class Measurement(
    startAvenue: String,
    startStreet: String,
    transitTime: Double,
    endAvenue: String,
    endStreet: String
)

object OutputJSON {
  implicit val rw: RW[OutputJSON] = macroRW
}
case class OutputJSON(
    startAvenue: String,
    startStreet: String,
    endAvenue: String,
    endStreet: String,
    totalTransitTime: Double,
    segments: List[Segment]
)

object Segment {
  implicit val rw: RW[Segment] = macroRW
}

case class Segment(
    startAvenue: String,
    startStreet: String,
    endAvenue: String,
    endStreet: String
)
