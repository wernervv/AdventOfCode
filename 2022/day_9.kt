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

    var headPos: Position = Pair(0,0)
    var tailPos: Position = Pair(0,0)
    var tailVisited: HashSet<Position> = HashSet<Position>()
    tailVisited.add(tailPos)

    fun parseOneInstruction(inputLine: String) {
        val (direction, amountS) = inputLine.split(" ")
        val amount = amountS.toInt()

        if (direction == "U") {
            repeat(amount) {
                headPos = moveHeadUp(headPos)
                tailPos = moveTail(headPos, tailPos)
                tailVisited.add(tailPos)
            }
        }
        else if (direction == "R") {
            repeat(amount) {
                headPos = moveHeadRight(headPos)
                tailPos = moveTail(headPos, tailPos)
                tailVisited.add(tailPos)
            }
        }
        else if (direction == "D") {
            repeat(amount) {
                headPos = moveHeadDown(headPos)
                tailPos = moveTail(headPos, tailPos)
                tailVisited.add(tailPos)
            }
        }
        else if (direction == "L") {
            repeat(amount) {
                headPos = moveHeadLeft(headPos)
                tailPos = moveTail(headPos, tailPos)
                tailVisited.add(tailPos)
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
