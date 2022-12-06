import java.io.File
import java.util.ArrayDeque

val fileName = "day_6_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput().get(0)
    val result = findStart(input, 14)
    println(result)
}

fun findStart(buffer: String, markerSize: Int): Int {
    var window = ArrayDeque<Char>()
    for (i in 0..(markerSize - 1)) {
        window.addLast(buffer.get(i))
    }
    var index = markerSize

    while (containsDuplicates(window)) {
        window.pop()
        window.addLast(buffer.get(index))
        index += 1
    }
    return index
}

fun containsDuplicates(window: ArrayDeque<Char>): Boolean {
    return (window.asSequence().distinct().count() != window.size)
}
