import java.io.File
import java.util.HashSet
import kotlin.math.abs

typealias Position = Pair<Int, Int>

val fileName = "day_9_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()

    var rope: Array<Position> = Array(10) { Pair(0,0) }

    var tailVisited: HashSet<Position> = HashSet<Position>()
    tailVisited.add(rope[9])

    fun parseOneInstruction(inputLine: String) {
        val (direction, amountS) = inputLine.split(" ")
        val amount = amountS.toInt()

        if (direction == "U") {
            repeat(amount) {
                rope[0] = moveHeadUp(rope[0])
                for (i in 1..9) {
                    rope[i] = moveTail(rope[i-1], rope[i])
                }
                tailVisited.add(rope[9])
            }
        }
        else if (direction == "R") {
            repeat(amount) {
                rope[0] = moveHeadRight(rope[0])
                for (i in 1..9) {
                    rope[i] = moveTail(rope[i-1], rope[i])
                }
                tailVisited.add(rope[9])
            }
        }
        else if (direction == "D") {
            repeat(amount) {
                rope[0] = moveHeadDown(rope[0])
                for (i in 1..9) {
                    rope[i] = moveTail(rope[i-1], rope[i])
                }
                tailVisited.add(rope[9])
            }
        }
        else if (direction == "L") {
            repeat(amount) {
                rope[0] = moveHeadLeft(rope[0])
                for (i in 1..9) {
                    rope[i] = moveTail(rope[i-1], rope[i])
                }
                tailVisited.add(rope[9])
            }
        }
    }

    for (line in input) {
        parseOneInstruction(line)
    }
    println(tailVisited.size)
}

fun moveHeadUp(oldHead: Position): Position {
    val (x, oldY) = oldHead
    return Pair(x, oldY + 1)
}

fun moveHeadRight(oldHead: Position): Position {
    val (oldX, y) = oldHead
    return Pair(oldX + 1, y)
}

fun moveHeadDown(oldHead: Position): Position {
    val (x, oldY) = oldHead
    return Pair(x, oldY - 1)
}

fun moveHeadLeft(oldHead: Position): Position {
    val (oldX, y) = oldHead
    return Pair(oldX - 1, y)
}

fun moveTail(headPos: Position, tailPos: Position): Position {
    val (headX, headY) = headPos
    val (tailX, tailY) = tailPos

    val xDiff = headX - tailX
    val yDiff = headY - tailY

    var moveX = 0
    var moveY = 0

    val xAbs = abs(xDiff)
    val yAbs = abs(yDiff)

    if (xAbs == 2) {
        moveX = xDiff / 2 // Tail moves in hops of one
        if (yAbs == 1) { // Diagonal move needed
            moveY = yDiff
        }
        else if (yAbs == 2) { // Diagonal move also needed
            moveY = yDiff / 2
        }
    }
    else if (xAbs == 1) {
        if (yAbs == 2) { // Diagonal
            moveX = xDiff
            moveY = yDiff / 2
        }
        // Otherwise no moves are needed
    }
    else if (xAbs == 0) {
        if (yAbs == 2) { // Only case where a move is needed
            moveY = yDiff / 2
        }
    }
    return Pair(tailX + moveX, tailY + moveY)
}
