def createAccount(initialBalance: Double): (Double => Unit, Double => Unit, (Double, (Double => Unit)) => Unit, () => Double, () => String) = {
  var currentBalance = initialBalance

  def deposit(amount: Double): Unit = {
    if (amount > 0) {
      currentBalance += amount
    } else {
      println("Error: Deposit amount must be positive")
    }
  }

  def withdraw(amount: Double): Unit = {
    if (amount > 0 && amount <= currentBalance) {
      currentBalance -= amount
    } else {
      if (amount > 0) println("Error: Insufficient funds")
      else println("Error: Withdrawal amount must be positive")
    }
  }

  def transfer(amount: Double, targetDeposit: Double => Unit): Unit = {
    if (amount > 0 && amount <= currentBalance) {
      withdraw(amount)
      targetDeposit(amount)
    } else {
      if (amount > 0) println("Error: Insufficient funds for transfer")
      else println("Error: Transfer amount must be positive")
    }
  }

  def getBalance(): Double = currentBalance

  def accountToString(): String = f"Account(balance: Rs.$$${currentBalance}%.2f)"

  (deposit, withdraw, transfer, getBalance, accountToString)
}

def createBank(accounts: List[(Double => Unit, Double => Unit, (Double, Double => Unit) => Unit, () => Double, () => String)]): 
  (() => List[String], () => Double, () => Unit) = {

  def negativeBalances(): List[String] = {
    accounts.filter(_._4() < 0).map(_._5())
  }

  def totalBalance(): Double = {
    accounts.map(_._4()).sum
  }

  def applyInterest(): Unit = {
    accounts.foreach { acc =>
      val balance = acc._4()
      if (balance > 0) {
        acc._1(balance * 0.05)
      } else {
        acc._1(balance * 0.10)
      }
    }
  }

  (negativeBalances, totalBalance, applyInterest)
}

def main(args: Array[String]): Unit = {
  val acc1 = createAccount(1000.00)
  val acc2 = createAccount(500.00)
  val acc3 = createAccount(-200.00)
  val bank = createBank(List(acc1, acc2, acc3))

  println("Initial Balances:")
  println(s"Account1: ${acc1._5()}")
  println(s"Account2: ${acc2._5()}")
  println(s"Account3: ${acc3._5()}")

  println("\nNegative Balances:")
  bank._1().foreach(println)

  println("\nTotal Balance of all accounts:")
  println(f"Rs.${bank._2()}%.2f")

  println("\nApplying interest...")
  bank._3()

  println("\nBalances after applying interest:")
  println(s"Account1: ${acc1._5()}")
  println(s"Account2: ${acc2._5()}")
  println(s"Account3: ${acc3._5()}")
}
