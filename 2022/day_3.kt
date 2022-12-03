import java.io.File

val fileName = "day_3_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()
    var total = 0
    for (line in input) {
        val currentPriority = getPriority(findCommon(splitInTwo(line)))
        total += currentPriority
    }
    println(total)
}

fun splitInTwo(s: String): List<String> {
    val midIndex = s.length / 2
    val splitStrings = listOf(s.substring(0, midIndex), s.substring(midIndex))
    return splitStrings
}

fun findCommon(parts: List<String>): Char {
    val first = parts[0]
    val second = parts[1]
    for (c in first) {
        for (candidate in second) {
            if (c == candidate) {
                return c
            }
        }
    }
    return '1'
}

fun getPriority(c: Char): Int {
    if (c.isUpperCase()) {
        return c.code - 'A'.code + 27
    }
    else {
        return c.code - 'a'.code + 1
    }
}
