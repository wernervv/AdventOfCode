import java.io.File
import java.util.HashMap
import java.util.ArrayList

val fileName = "day_14_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun givePositionsBetween(startX: Int, startY: Int, endX: Int, endY: Int): List<Pair<Int,Int>> {
    var positions: List<Pair<Int,Int>> = emptyList()

    if (startX == endX) {
        if (startY > endY) {
            for (y in endY..startY) {
                positions += Pair(startX, y)
            }
        }
        else {
            for (y in startY..endY) {
                positions += Pair(startX, y)
            }
        }
    }
    else {
        if (startX > endX) {
            for (x in endX..startX) {
                positions += Pair(x, startY)
            }
        }
        else {
            for (x in startX..endX) {
                positions += Pair(x, startY)
            }
        }
    }

    return positions
}

fun addNewRockPositions(positions: List<Pair<Int,Int>>, rocks: HashMap<Int,ArrayList<Int>>) {
    fun addToRightPlace(y: Int, positions: ArrayList<Int>): ArrayList<Int> {
        val index = positions.indexOfFirst({ it > y })

        if (index != -1) {
            positions.add(index, y)
        }
        else positions.add(y)

        return positions
    }

    fun addOnePosition(position: Pair<Int,Int>, rocks: HashMap<Int,ArrayList<Int>>) {
        val (x,y) = position
        val previousPositions = rocks.get(x)
        if (previousPositions != null) {
            val newPositions = addToRightPlace(y, previousPositions)
            rocks.put(x, newPositions)
        }
        else {
            rocks.put(x, ArrayList(listOf(y)))
        }
    }

    for (position in positions) {
        addOnePosition(position, rocks)
    }
}

fun readOneRockPath(path: String, rocks: HashMap<Int,ArrayList<Int>>) {
    var turningPoints = path.split(" -> ")

    fun parsePosition(positionString: String): Pair<Int,Int> {
        val (xString, yString) = positionString.split(',')
        return Pair(xString.toInt(), yString.toInt())
    }

    var (previousX, previousY) = parsePosition(turningPoints[0])
    turningPoints = turningPoints.drop(1)

    while (!turningPoints.isEmpty()) {
        val (currentX, currentY) = parsePosition(turningPoints[0])
        addNewRockPositions(givePositionsBetween(previousX, previousY, currentX, currentY), rocks)
        previousX = currentX
        previousY = currentY
        turningPoints = turningPoints.drop(1)
    }
}

fun readRockPositions(input: List<String>): HashMap<Int,ArrayList<Int>> {
    val rocks: HashMap<Int,ArrayList<Int>> = HashMap()

    for (path in input) {
        readOneRockPath(path, rocks)
    }

    return rocks
}

fun giveLowestRock(rocks: HashMap<Int,ArrayList<Int>>): Int {
    var currentBottom = 0
    for (yValList in rocks.values) {
        for (yVal in yValList) {
            if (yVal > currentBottom) {
                currentBottom = yVal
            }
        }
    }
    return currentBottom
}

fun tileIsFree(position: Pair<Int,Int>, cave: HashMap<Int,ArrayList<Int>>): Boolean {
    val (x,y) = position
    val relevantRow = cave.get(x)
    if (relevantRow == null) {
        return true
    }
    else {
        return relevantRow.find({ it == y }) == null
    }
}

fun rightBelow(position: Pair<Int,Int>): Pair<Int,Int> {
    val (x,oldY) = position
    return Pair(x, oldY + 1)
}

fun downLeft(position: Pair<Int,Int>): Pair<Int,Int> {
    val (oldX,oldY) = position
    return Pair(oldX - 1, oldY + 1)
}

fun downRight(position: Pair<Int,Int>): Pair<Int,Int> {
    val (oldX,oldY) = position
    return Pair(oldX + 1, oldY + 1)
}

fun pourOneUnitOfSand(cave: HashMap<Int,ArrayList<Int>>, pouringPosition: Pair<Int,Int>, bottom: Int): Boolean {
    var currentPosition = pouringPosition
    var fallsToVoid = false

    while (true) {
        if (currentPosition.second == bottom) {
            fallsToVoid = true
            break
        }
        val below = rightBelow(currentPosition)
        val left = downLeft(currentPosition)
        val right = downRight(currentPosition)
        if (tileIsFree(below, cave)) {
            currentPosition = below
        }
        else if (tileIsFree(left, cave)) {
            currentPosition = left
        }
        else if (tileIsFree(right, cave)) {
            currentPosition = right
        }
        else {
            val (x,y) = currentPosition
            addNewRockPositions(listOf(currentPosition), cave)
            break
        }
    }

    return fallsToVoid
}

fun pourSand(cave: HashMap<Int,ArrayList<Int>>, pouringPosition: Pair<Int,Int>, bottom: Int): Int {
    var unitsPoured = 0
    var latestFell = false

    while (!latestFell) {
        latestFell = pourOneUnitOfSand(cave, pouringPosition, bottom)
        unitsPoured += 1
    }
    return unitsPoured - 1
}

fun main() {
    val input = readInput()
    val rocks = readRockPositions(input)

    val pouringPosition = Pair(500, 0)
    val bottom = giveLowestRock(rocks)

    val sandPoured = pourSand(rocks, pouringPosition, bottom)
    println(sandPoured)
}
