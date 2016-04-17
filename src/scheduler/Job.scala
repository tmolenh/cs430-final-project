package scheduler

/**
  * A POD class for jobs
  *
  * @tparam A the type of this job's start and end values, must have an order
  * @param name  job identifier or description
  * @param start when this job begins
  * @param end   when this job ends
  */
case class Job[A](name: String, start: A, end: A)(implicit ev: A => Ordered[A])