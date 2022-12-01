import java.io.File

val fileName = "day_1_input.txt"

fun main() {
    val input = readInput()
    val result = maxCalories(input)
    println(result)
}

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun maxCalories(input: List<String>): Int {
    var maxVal = 0
    var current = 0
    for (line in input) {
        if (line.isNullOrEmpty()) {
            if (current > maxVal) {
                maxVal = current
            }
            current = 0
        }
        else {
            current += line.toInt()
        }
    }
    return maxVal
}
