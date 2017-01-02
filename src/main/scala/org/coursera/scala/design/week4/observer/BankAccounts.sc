import org.coursera.scala.design.week4.observer.{Consolidator, BankAccount}

val a = new BankAccount
val b = new BankAccount

val consolidator = new Consolidator(List(a, b))
consolidator.totalBalance

a.deposit(20)
consolidator.totalBalance

b.deposit(30)
consolidator.totalBalance

b.withdraw(10)
consolidator.totalBalance