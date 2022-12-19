import java.io.File
import java.util.ArrayList
import kotlin.math.abs

val fileName = "day_15_input.txt"
val rowToCheck = 2000000
val multiplier = 4000000L
val lowerLimit = 0
val upperLimit = 4000000

typealias Position = Pair<Int,Int>
typealias SensorsBeacons = List<Pair<Position,Position>>

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun readSensorsAndBeacons(input: List<String>): SensorsBeacons {
    val sensorsAndBeacons: ArrayList<Pair<Position,Position>> = ArrayList()

    fun readOneSensorAndBeacon(line: String) {
        val splitInput = line.split(" ")
        val sensorX = splitInput[2].drop(2).dropLast(1).toInt()
        val sensorY = splitInput[3].drop(2).dropLast(1).toInt()
        val beaconX = splitInput[8].drop(2).dropLast(1).toInt()
        val beaconY = splitInput[9].drop(2).toInt()

        sensorsAndBeacons.add(Pair(Pair(sensorX, sensorY), Pair(beaconX, beaconY)))
    }

    for (line in input) {
        readOneSensorAndBeacon(line)
    }

    return sensorsAndBeacons
}

fun manhattanDistance(first: Position, second: Position): Int {
    val (firstX, firstY) = first
    val (secondX, secondY) = second

    return abs(firstX - secondX) + abs(firstY - secondY)
}

fun minAndMaxFromOneSensor(sensor: Position, beacon: Position, relevantRow: Int): Pair<Int,Int> {
    val (sensorX, sensorY) = sensor
    val scanDistance = manhattanDistance(sensor, beacon)

    val difference = scanDistance - abs(sensorY - relevantRow)

    return Pair(sensorX - difference, sensorX + difference)
}

fun minAndMaxReachedByScan(sensorsAndBeacons: SensorsBeacons, relevantRow: Int): Pair<Int,Int> {
    var min = Int.MAX_VALUE
    var max = Int.MIN_VALUE

    for ((sensor, beacon) in sensorsAndBeacons) {
        val (currentMin, currentMax) = minAndMaxFromOneSensor(sensor, beacon, relevantRow)
        if (currentMin < min) {
            min = currentMin
        }
        if (currentMax > max) {
            max = currentMax
        }
    }

    return Pair(min, max)
}

fun scanReachesPosition(sensor: Position, beacon: Position, position: Position): Boolean {
    val scanDistance = manhattanDistance(sensor, beacon)
    val distanceToPosition = manhattanDistance(sensor, position)

    return distanceToPosition <= scanDistance
}

fun positionIsScannedAndEmpty(position: Position, sensorsAndBeacons: SensorsBeacons): Boolean {
    for ((sensor, beacon) in sensorsAndBeacons) {
        if (position == sensor || position == beacon) {
            return false
        }

        if (scanReachesPosition(sensor, beacon, position)) {
            return true
        }
    }

    return false
}

fun giveEmptyCountOnGivenLine(sensorsAndBeacons: SensorsBeacons, line: Int): Int {
    var count = 0

    val (min, max) = minAndMaxReachedByScan(sensorsAndBeacons, line)
    for (x in min..max) {
        val position = Pair(x, line)
        if (positionIsScannedAndEmpty(position, sensorsAndBeacons)) {
            count += 1
        }
    }

    return count
}

// fun giveEdgeInArea(sensor: Position, closestBeacon: Position, minX: Int, maxX: Int, minY: Int, maxY: Int): List<Position> {
//     val edge: ArrayList<Position> = ArrayList()
//     val (sensorX, sensorY) = sensor
//     val scanDistance = manhattanDistance(sensor, closestBeacon)

//     val minXReachedByScan = sensorX - scanDistance
//     val maxXReachedByScan = sensorX + scanDistance

//     if (minXReachedByScan > maxX || maxXReachedByScan < minX) {
//         return edge
//     }

//     val minXInArea = maxOf(minX, minXReachedByScan)
//     val maxXInArea = minOf(maxX, maxXReachedByScan)

//     for (x in minXInArea..maxXInArea) {
//         val difference = scanDistance - abs(sensorX - x)

//         val positiveY = sensorY + difference
//         if (minY <= positiveY && positiveY <= maxY) {
//             edge.add(Pair(x, positiveY))
//         }

//         val negativeY = sensorY - difference
//         if (minY <= negativeY && negativeY <= maxY) {
//             edge.add(Pair(x, negativeY))
//         }
//     }

//     return edge
// }

fun giveCommonEdgePositions(sensorOne: Position, beaconOne: Position, sensorTwo: Position, beaconTwo: Position): List<Position> {
    val edgePositions: ArrayList<Position> = ArrayList()

    val edgeDistanceOne = manhattanDistance(sensorOne, beaconOne) + 1
    val edgeDistanceTwo = manhattanDistance(sensorTwo, beaconTwo) + 1
    val betweenSensors = manhattanDistance(sensorOne, sensorTwo)

    val(firstX, firstY) = sensorOne
    val (secondX, secondY) = sensorTwo

    if (betweenSensors > edgeDistanceOne + edgeDistanceTwo) {
        return edgePositions
    }

    var minX = 0
    var maxX = 0

    if (firstX > secondX) {
        minX = firstX - edgeDistanceOne
        maxX = secondX + edgeDistanceTwo
    }
    else {
        minX = secondX - edgeDistanceTwo
        maxX = firstX + edgeDistanceOne
    }

    for (x in minX..maxX) {
        val heightDifferenceToFirst = edgeDistanceOne - (abs(firstX - x))
        val heightDifferenceToSecond = edgeDistanceTwo - (abs(secondX - x))

        if (heightDifferenceToFirst + heightDifferenceToSecond == abs(firstY - secondY)) {
            if (firstY > secondY) {
                edgePositions.add(Pair(x, firstY - heightDifferenceToFirst))
            }
            else {
                edgePositions.add(Pair(x, firstY + heightDifferenceToFirst))
            }
        }
    }
    return edgePositions
}

fun positionIsNotScanned(position: Position, sensorsAndBeacons: SensorsBeacons): Boolean {
    for ((sensor, beacon) in sensorsAndBeacons) {
        if (scanReachesPosition(sensor, beacon, position)) {
            return false
        }
    }
    return true
}

fun findDistressBeacon(sensorsAndBeacons: SensorsBeacons, minX: Int, maxX: Int, minY: Int, maxY: Int): Position {
    for (i in 0..sensorsAndBeacons.size-2) {
        val (sensorOne, beaconOne) = sensorsAndBeacons[i]
        for (j in i..sensorsAndBeacons.size-1) {
            val (sensorTwo, beaconTwo) = sensorsAndBeacons[j]
            val list = giveCommonEdgePositions(sensorOne, beaconOne, sensorTwo, beaconTwo)
            for (position in list) {
                val (x,y) = position
                if (minX <= x && x <= maxX && minY <= y && y <= maxY) {
                    if (positionIsNotScanned(position, sensorsAndBeacons)) {
                        return position
                    }
                }
            }
        }
    }

    return Pair(-1, -1)
}

fun calculateTuningFrequency(position: Position): Long {
    val (x,y) = position
    val longX = x.toLong()
    val longY = y.toLong()
    return multiplier * x + y
}

fun main() {
    val input = readInput()
    val sensorsAndBeacons = readSensorsAndBeacons(input)

    val distressBeacon = findDistressBeacon(sensorsAndBeacons, lowerLimit, upperLimit, lowerLimit, upperLimit)
    val tuningFrequency = calculateTuningFrequency(distressBeacon)
    println(tuningFrequency)
}
