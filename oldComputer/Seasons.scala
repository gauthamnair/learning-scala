import ChecksumAccumulator.calculate

object Seasons extends App {
    for (season <- List("fall", "winter", 
        "spring", "summer"))
    println(season + ":" + calculate(season))
}