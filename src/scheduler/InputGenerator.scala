package scheduler

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object InputGenerator {

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt //number of jobs to output
    val range = args(1).toInt //time range of jobs

    val lines = for (i <- 0 until n) yield {
      val job = randomJob(generateName(i), range)
      job.name + "\t\t" + job.start + "\t\t" + job.end
    }

    val contents = lines.mkString("\n")
    Files.write(Paths.get("input.txt"), contents.getBytes(StandardCharsets.UTF_8))
  }

  def randomJob(name: String, range: Int) = {
    val end = (math.random * range).toInt
    val length = (math.random * end).toInt
    Job(name, end - length, end + 1)
  }

  def generateName(i: Int) = Integer.toString(i, 26) map (c => (if (c <= 57) c + 17 else c - 22).toChar)

}