import java.io.File

val fileName = "day_10_input.txt"
val relevantCycles = listOf(20, 60, 100, 140, 180, 220)

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()

    var currentInd = 0
    var cycleCount = 0
    var registerChanges: List<Int> = emptyList()
    var cyclesSpentOnCurrent = 0

    fun tick() {
        val currentInstr = input.get(currentInd)
        if (currentInstr == "noop") {
            currentInd += 1
        }
        else {
            if (cyclesSpentOnCurrent == 1) {
                val change = currentInstr.split(" ")[1].toInt()
                registerChanges += change
                cyclesSpentOnCurrent = 0
                currentInd += 1
            }
            else {
                cyclesSpentOnCurrent = 1
            }
        }
        cycleCount += 1
    }
    
    var signalStrengths: List<Int> = emptyList()
    
    while (cycleCount < relevantCycles.max()) {
        if (relevantCycles.contains(cycleCount + 1)) {
            val currentSignalStrength = (cycleCount + 1) * (registerChanges.fold(1, { a, b -> a + b }))
            signalStrengths += currentSignalStrength
        }
        tick()
    }
    println(signalStrengths.sum())
}
