import java.io.File
import java.util.HashMap
import kotlin.math.max

val fileName = "day_16_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun parseValves(input: List<String>): HashMap<String, Valve> {
    var valves = HashMap<String, Valve>()

    for (line in input) {
        val splitLine = line.split(" ")
        val valveName = splitLine[1]
        val valveFlowRate = splitLine[4].dropLast(1).drop(5).toInt()

        val valveConnections = ArrayList<String>()
        for (i in 9..splitLine.size-1) {
            valveConnections.add(splitLine[i].dropLastWhile( {it == ','} ))
        }
        val newValve = Valve(valveFlowRate, valveConnections)
        valves.put(valveName, newValve)
    }

    return valves
}

class Valve(flowRate: Int, leadsTo: List<String>) {
    val flowRate = flowRate
    val leadsTo = leadsTo
}

class Path() {
    var currentPath = ArrayList<String>()
    var openingTimes = ArrayList<Int>()
    var releasedPressure = 0

    fun printPath() {
        print("Path: ")
        for (valve in currentPath) {
            print("${valve}, ")
        }
        // println()
        print("Times:   ")
        for (time in openingTimes) {
            print(" ${time}, ")
        }
        println()
    }
}

fun collectDistances(allValves: HashMap<String, Valve>, relevantValves: ArrayList<String>): HashMap<String,HashMap<String,Pair<Int,Valve>>> {
    fun openDistance(current: String, target: String): Int {
        var distance = 0
        val seen = ArrayList<String>()
        seen.add(current)

        while (true) {
            distance += 1
            for (valve in seen.toList()) {
                for (neighbour in allValves.get(valve)!!.leadsTo) {
                    if (neighbour == target) {
                        return distance + 1
                    }
                    else {
                        if (!seen.contains(neighbour)) {
                            seen.add(neighbour)
                        }
                    }
                }
            }
        }
    }

    val allDistances = HashMap<String, HashMap<String, Pair<Int, Valve>>>()

    val allStartingValves = ArrayList(relevantValves)
    allStartingValves.add("AA")
    for (valve in allStartingValves) {
        val distancesFromCurrent = HashMap<String, Pair<Int, Valve>>()
        for (targetValve in relevantValves) {
            if (valve != targetValve) {
                val distance = openDistance(valve, targetValve)
                distancesFromCurrent.put(targetValve, Pair(distance, allValves.get(targetValve)!!))
            }
        }
        allDistances.put(valve, distancesFromCurrent)
    }

    return allDistances
}

fun nextPath(path: Path, isAlreadyModified: Boolean, relevantValves: ArrayList<String>, allValves: HashMap<String, Valve>, distances: HashMap<String,HashMap<String,Pair<Int,Valve>>>): Boolean {
    fun nextUnvisited(collection: List<String> = relevantValves): String {
        for (valve in collection) {
            if (!path.currentPath.contains(valve)) {
                return valve
            }
        }
        return "AA"
    }

    fun currentLast(): String {
        return path.currentPath.get(path.currentPath.lastIndex)
    }

    fun appendNextToPath(): Boolean {
        val current = currentLast()
        val next = nextUnvisited()

        if (next == "AA") {
            return false
        }

        val infoOfNext = distances.get(current)!!.get(next)
        val openDistance = infoOfNext!!.first

        path.currentPath.add(next)
        path.openingTimes.add(openDistance)

        return true
    }

    fun backtrack(): Boolean {
        val current = currentLast()
        if (current == "AA") {
            return false
        }
        val remainingRelevant = relevantValves.dropWhile({it -> it != current}).drop(1)
        val toBeAdded = nextUnvisited(remainingRelevant)

        path.currentPath = ArrayList(path.currentPath.dropLast(1))
        path.openingTimes = ArrayList(path.openingTimes.dropLast(1))
        if (toBeAdded == "AA") {
            return backtrack()
        }
        else {
            val arrivingFrom = currentLast()
            path.currentPath.add(toBeAdded)
            path.openingTimes.add(distances.get(arrivingFrom)!!.get(toBeAdded)!!.first)
            return true
        }
    }

    if (path.openingTimes.sum() <= 27) {
        if (!appendNextToPath()) {
            path.releasedPressure = calculatePressureReleased(path, allValves)
            val modifiedTimes = ArrayList(path.openingTimes.dropLast(1))
            modifiedTimes.add(30)
            path.openingTimes = modifiedTimes
            return true
        }
        return nextPath(path, true, relevantValves, allValves, distances)
    }
    else {
        if (isAlreadyModified) {
            path.releasedPressure = calculatePressureReleased(path, allValves)
            return true
        }
        else {
            if (backtrack()) {
                return nextPath(path, true, relevantValves, allValves, distances)
            }
            else {
                return false
            }
        }
    }
}

fun calculatePressureReleased(path: Path, allValves: HashMap<String, Valve>): Int {
    var time = 30
    var releasedPressure = 0
    for (i in path.openingTimes.indices) {
        val valve = path.currentPath.get(i+1)
        time -= path.openingTimes.get(i)
        val released = max(0, time * allValves.get(valve)!!.flowRate)
        releasedPressure += released
    }
    return releasedPressure
}

fun mostPressureReleased(path: Path, relevantValves: ArrayList<String>, allValves: HashMap<String, Valve>, distances: HashMap<String, HashMap<String, Pair<Int, Valve>>>): Int {
    var mostReleased = 0
    while (nextPath(path, false, relevantValves, allValves, distances)) {
        // path.printPath()
        mostReleased = max(mostReleased, path.releasedPressure)
    }
    return mostReleased
}

fun relevantValves(allValves: HashMap<String, Valve>): ArrayList<String> {
    val relevantValves = ArrayList<String>()
    for (valveEntry in allValves) {
        if (valveEntry.value.flowRate > 0) {
            relevantValves.add(valveEntry.key)
        }
    }
    return relevantValves
}

fun main() {
    val input = readInput()
    val allValves = parseValves(input)

    val relevantValves = relevantValves(allValves)
    val distances = collectDistances(allValves, relevantValves)

    val initialPath = Path()
    initialPath.currentPath.add("AA")
    val result = mostPressureReleased(initialPath, relevantValves, allValves, distances)
    println(result)
}
