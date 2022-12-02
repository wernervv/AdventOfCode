import java.io.File

val fileName = "day_2_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()
    var totalScore = 0
    for (line in input) {
        val roundScore = calculateScore(line.split(" "))
        totalScore += roundScore
    }
    println(totalScore)
}

fun shapeScore(shape: String): Int {
    val score = when (shape.first()) {
        'X' -> 1
        'Y' -> 2
        'Z' -> 3
        else -> 0
    }
    return score
}

fun giveBeating(shape: Char): Char {
    if (shape == 'A') {
        return 'Y'
    }
    else if (shape == 'B') {
        return 'Z'
    }
    else {
        return 'X'
    } 
}

fun giveEqual(shape: Char): Char {
    if (shape == 'A') {
        return 'X'
    }
    else if (shape == 'B') {
        return 'Y'
    }
    else {
        return 'Z'
    }
}

fun roundOutcome(shapes: List<String>): Int {
    val opponentShape = shapes[0].first()
    val yourShape = shapes[1].first()

    var score = 0
    if (yourShape == giveBeating(opponentShape)) {
        score = 6
    }
    else if (yourShape == giveEqual(opponentShape)) {
        score = 3
    }

    return score
}

fun calculateScore(shapes: List<String>): Int {
    val shapeS = shapeScore(shapes[1])
    val roundS = roundOutcome(shapes)
    return  shapeS + roundS
}
