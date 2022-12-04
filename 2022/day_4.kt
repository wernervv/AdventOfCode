import java.io.File

val fileName = "day_4_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()
    var total = 0
    for (line in input) {
        if (fullOverlap(givePoints(line))) {
            total += 1
        }
    }
    println(total)
}

fun givePoints(line: String): List<Int> {
    val (first, second) = line.split(",")
    val (fStart, fEnd) = first.split("-").map({ it.toInt() })
    val (sStart, sEnd) = second.split("-").map({ it.toInt() })
    return listOf(fStart, fEnd, sStart, sEnd)
}

fun fullOverlap(points: List<Int>): Boolean {
    val fStart = points[0]
    val fEnd = points[1]
    val sStart = points[2]
    val sEnd = points[3]

    if (fStart >= sStart && fEnd <= sEnd) {
        return true
    }

    if (sStart >= fStart && sEnd <= fEnd) {
        return true
    }

    return false
}
