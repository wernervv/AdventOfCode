import java.io.File

val fileName = "day_3_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()
    var total = 0
    var index = 0
    while (index <= input.size - 3) {
        val group = listOf(input[index], input[index+1], input[index+2])
        val currentPriority = getPriority(findCommonTwo(group))
        total += currentPriority
        index += 3
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

fun findCommonTwo(parts: List<String>): Char {
    val first = parts[0]
    val second = parts[1]
    val third = parts[2]
    for (c in first) {
        for (candidate2 in second) {
            if (c == candidate2) {
                for (candidate3 in third) {
                    if (c == candidate3) {
                        return c
                    }
                }
            }
        }
    }
    return '1'
}
