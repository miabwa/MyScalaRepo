package retcalc

object RetCalc {
  def nbOfMonthsSaving(interestRate: Double, nbOfMonthsInRetirement: Int,
                       netIncome: Int, currentExpenses: Int, initialCapital:
                       Double): Int = {
    def loop(months: Int): Int = {
      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
        interestRate = interestRate,
        nbOfMonthsSaving = months, nbOfMonthsInRetirement =
          nbOfMonthsInRetirement,
        netIncome = netIncome, currentExpenses = currentExpenses,
        initialCapital = initialCapital)
      val returnValue =
        if (capitalAfterDeath > 0.0)
          months
        else
          loop(months + 1)
      returnValue
    }

    loop(0)
  }

  def simulatePlan(interestRate: Double,
                   nbOfMonthsSaving: Int, nbOfMonthsInRetirement: Int,
                   netIncome: Int, currentExpenses: Int, initialCapital:
                   Double): (Double, Double) = {
    val capitalAtRetirement = futureCapital(
      interestRate = interestRate, nbOfMonths = nbOfMonthsSaving,
      netIncome = netIncome, currentExpenses = currentExpenses,
      initialCapital = initialCapital)
    val capitalAfterDeath = futureCapital(
      interestRate = interestRate, nbOfMonths = nbOfMonthsInRetirement,
      netIncome = 0, currentExpenses = currentExpenses,
      initialCapital = capitalAtRetirement)
    (capitalAtRetirement, capitalAfterDeath)
  }

  def futureCapital(interestRate: Double, nbOfMonths: Int, netIncome: Int,
                    currentExpenses: Int, initialCapital: Double): Double = {
    val monthlySavings = netIncome - currentExpenses
    (0 until nbOfMonths).foldLeft(initialCapital)(
      (accumulated, _) => accumulated * (1 + interestRate) + monthlySavings)
  }
}