package traffic

import java.io.File
import scopt.OParser
import scala.io.Source
import upickle.default._

case class InterviewInput(
    input: File,
    startingIntersection: String,
    endingIntersection: String
)

object Main extends App {
  val builder = OParser.builder[InterviewInput]
  val parser = {
    import builder._
    OParser.sequence(
      programName("interview"),
      head("interview", "1.0 Ultimate Edition"),
      opt[File]('f', "file")
        .required()
        .action((x, c) => c.copy(input = x))
        .text("The JSON input file"),
      opt[String]('s', "start")
        .required()
        .action((x, c) => c.copy(startingIntersection = x))
        .text(
          "The starting intersection in format <Avenue Letter><Street number>, e.g. A1"
        ),
      opt[String]('e', "end")
        .required()
        .action((x, c) => c.copy(endingIntersection = x))
        .text(
          "The ending intersection in format <Avenue Letter><Street number>, e.g. A1"
        )
    )
  }
  OParser.parse(parser, args, InterviewInput(new File("."), "", "")) foreach {
    input =>
      val fileContents = Source.fromFile(input.input).getLines.mkString
      val inputObject = read[InputJSON](fileContents)
      // we combine all measurements into one giant list
      val allTraficMeasuresCombined =
        inputObject.trafficMeasurements.flatMap(_.measurements)
      // we get all possible intersections
      val allIntersections = allTraficMeasuresCombined
        .flatMap(tm =>
          (tm.startAvenue, tm.startStreet) :: (
            tm.endAvenue,
            tm.endStreet
          ) :: Nil
        )
        .toSet
      // we partition the list by segment
      val segmentToMeasurements = allTraficMeasuresCombined.groupBy(tm =>
        ((tm.startAvenue, tm.startStreet), (tm.endAvenue, tm.endStreet))
      )
      // for each segment we compute the mean
      val neighbourMap = segmentToMeasurements.map(e =>
        (e._1, e._2.map(_.transitTime).reduce(_ + _) / e._2.size)
      )
      val startAvenue = input.startingIntersection.head.toString()
      val startStreet = input.startingIntersection.tail.toString
      val endAvenue = input.endingIntersection.head.toString()
      val endStreet = input.endingIntersection.tail.toString
      val tuple = ((startAvenue, startStreet), (endAvenue, endStreet))
      val (segments, time) = FunctionalDijkstra
        .computeShortestDistance(
          (startAvenue, startStreet),
          (endAvenue, endStreet),
          allIntersections,
          neighbourMap
        )
      val segmentsPlusLastLeg = segments.appended((endAvenue, endStreet))
      println(
        write(
          OutputJSON(
            startAvenue,
            startStreet,
            endAvenue,
            endStreet,
            time,
            segmentsPlusLastLeg
              .sliding(2)
              .map(pair =>
                Segment(pair(0)._1, pair(0)._2, pair(1)._1, pair(1)._2)
              )
              .toList
          )
        )
      )
  }
}
