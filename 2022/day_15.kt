import java.io.File
import kotlin.math.abs

val fileName = "day_15_input.txt"
val rowToCheck = 2000000

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

fun main() {
    val input = readInput()
    val sensorsAndBeacons = readSensorsAndBeacons(input)

    val count = giveEmptyCountOnGivenLine(sensorsAndBeacons, rowToCheck)
    println(count)
}
