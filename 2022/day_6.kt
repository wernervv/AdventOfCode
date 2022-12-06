import java.io.File
import java.util.ArrayDeque

val fileName = "day_6_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput().get(0)
    val result = findStart(input)
    println(result)
}

fun findStart(buffer: String): Int {
    var window = ArrayDeque<Char>()
    for (i in 0..3) {
        window.addLast(buffer.get(i))
    }
    var index = 4

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
