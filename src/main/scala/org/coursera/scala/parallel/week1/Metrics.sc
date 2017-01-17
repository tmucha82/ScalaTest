import org.scalameter._

for (i <- 0 until 10)
  println(
    // without warming up
    measure {
      (0 until 1000000).toArray
    }
  )
for (i <- 0 until 10)
  println(
    // with default warming up
    withWarmer(new Warmer.Default) measure {
      (0 until 1000000).toArray
    }
  )
for (i <- 0 until 10)
  println(
    // with configuration and warming up
    config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 60,
      Key.verbose -> true
    ) withWarmer new Warmer.Default measure {
      (0 until 1000000).toArray
    }
  )

