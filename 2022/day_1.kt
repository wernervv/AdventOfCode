import java.io.File

val fileName = "day_1_input.txt"

fun main() {
    val input = readInput()
    val result = maxCalories_part_two(input)
    println(result)
}

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun maxCalories_part_one(input: List<String>): Int {
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

fun maxCalories_part_two(input: List<String>): Int {
    var maxVal1 = 0
    var maxVal2 = 0
    var maxVal3 = 0
    var current = 0
    for (line in input) {
        if (line.isNullOrEmpty()) {
            if (current > maxVal1) {
                maxVal1 = current.also {current = maxVal1}
            }
            if (current > maxVal2) {
                maxVal2 = current.also {current = maxVal2}
            }
            if (current > maxVal3) {
                maxVal3 = current.also {current = maxVal3}
            }
            current = 0
        }
        else {
            current += line.toInt()
        }
    }
    return maxVal1 + maxVal2 + maxVal3
}
