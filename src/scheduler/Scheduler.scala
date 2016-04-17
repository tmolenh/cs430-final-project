package scheduler

import scala.collection.mutable.ArrayBuffer

object Scheduler {

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: Scheduler num-machines input-file")
      return
    }
    val machines = args(0).toInt // number of machines
    val filepath = args(1) // path to input file

    val jobs = parse(filepath)
    val sortedJobs = jobs sortBy (_.end) // sort jobs by end time

    val result = (0 until machines) map { _ =>
      val jobIndices = schedule(sortedJobs)
      val scheduledJobs = jobIndices map (sortedJobs(_)) // get scheduled jobs
      jobIndices.reverse map sortedJobs.remove // remove scheduled jobs
      scheduledJobs
    }

    // output results to console
    printf("%d of %d jobs scheduled%n%n", result.foldLeft(0)(_ + _.size), jobs.size)

    (0 until machines) foreach { i =>
      printf("Machine %d - %d jobs%n", i, result(i).size)
      result(i) foreach println
      println
    }

    printf("Unscheduled - %d jobs%n", sortedJobs.size)
    sortedJobs foreach println
  }

  /**
    * Parses a text file containing jobs in the following format:
    *
    * JobName 1 15
    * AnotherJob 2 4
    * Job3 9 10
    * ... (etc.)
    *
    * Invalid lines will be skipped.
    *
    * @param filepath the path of the file to parse
    * @return an [[ArrayBuffer]] of jobs from the file
    **/
  private def parse(filepath: String) = {
    val file = io.Source.fromFile(filepath) // open file
    val jobRegex =
      """(\S+)\s+(-?\d+)\s+(-?\d+)""".r

    // convert the lines of the file to Job objects
    val jobs = file.getLines().to[ArrayBuffer] collect {
      case jobRegex(name, start, end) => Job(name, start.toInt, end.toInt)
    }

    jobs filter (job => job.start < job.end) // remove invalid jobs
  }

  /**
    * Schedules jobs for a single machine.
    *
    * @param jobs sorted list of Jobs to schedule
    * @return the indices of the scheduled jobs
    */
  private def schedule[A](jobs: IndexedSeq[Job[A]])(implicit ev: A => Ordered[A]) = {
    // run findPrev on all jobs
    val prev = jobs.indices map (findPrev(_, jobs))

    // find the value of the optimal solution for each index
    val opt = OptArray(jobs.size)
    for (i <- opt.indices) {
      opt(i) = math.max(1 + opt(prev(i)), opt(i - 1))
    }

    findSolution(prev, opt)
  }

  /**
    * Use binary search to find the nearest previous [[Job]] that is disjoint with jobs(i).
    *
    * @param i    the index of the Job you want to find the nearest previous disjoint Job of
    * @param jobs a sorted list of Jobs to search
    * @return the index of the Job or -1 if there is none
    */
  private def findPrev[A](i: Int, jobs: IndexedSeq[Job[A]])(implicit ev: A => Ordered[A]) = {
    def go(min: Int, max: Int): Int = {
      if (max < min)
        return max

      val mid = (min + max) / 2

      jobs(mid).end compare jobs(i).start match {
        case -1 => go(mid + 1, max) // end < start
        case 0 => if (jobs(mid).end == jobs(mid + 1).end) go(mid + 1, max) else mid
        case 1 => go(min, mid - 1)
      }
    }

    go(0, i - 1)
  }

  /**
    * Finds the Jobs that form the optimal solution.
    *
    * @param prev the indices calculated by findPrev
    * @param opt  optimal solution values for each index
    * @return the indices of the jobs that form the optimal solution
    */
  private def findSolution(prev: IndexedSeq[Int], opt: OptArray): List[Int] = {
    def go(n: Int, a: List[Int]): List[Int] = {
      if (n == -1)
        a
      else if (1 + opt(prev(n)) >= opt(n - 1))
        go(prev(n), n :: a) // add n to final result
      else
        go(n - 1, a)
    }

    go(opt.size - 1, List.empty)
  }

}