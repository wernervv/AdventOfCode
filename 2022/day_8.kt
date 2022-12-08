import java.io.File

val fileName = "day_8_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

class Grid(input: List<String>) {
    val grid: Array<IntArray> = Array(input.size) { i -> (input.get(i).map { it.digitToInt() }).toIntArray() }
    val width = grid[0].size
    val height = grid.size
}

fun main() {
    val input = readInput()
    val grid = Grid(input)
    var visibleCount = 0
    for (y in 0..(grid.height-1)) {
        for (x in 0..(grid.width-1)) {
            if (canBeSeen(x, y, grid)) {
                visibleCount += 1
            }
        }
    }
    println(visibleCount)
}

fun canBeSeenLeft(x: Int, y: Int, g: Grid): Boolean {
    val targetHeight = g.grid[y][x]
    for (i in 0..(x-1)) {
        val currentHeight = g.grid[y][i]
        if (currentHeight >= targetHeight) {
            return false
        }
    }
    return true
}

fun canBeSeenUp(x: Int, y: Int, g: Grid): Boolean {
    val targetHeight = g.grid[y][x]
    for (i in 0..(y-1)) {
        val currentHeight = g.grid[i][x]
        if (currentHeight >= targetHeight) {
            return false
        }
    }
    return true
}

fun canBeSeenRight(x: Int, y: Int, g: Grid): Boolean {
    val targetHeight = g.grid[y][x]
    for (i in (x+1)..(g.width-1)) {
        val currentHeight = g.grid[y][i]
        if (currentHeight >= targetHeight) {
            return false
        }
    }
    return true
}

fun canBeSeenDown(x: Int, y: Int, g: Grid): Boolean {
    val targetHeight = g.grid[y][x]
    for (i in (y+1)..(g.height-1)) {
        val currentHeight = g.grid[i][x]
        if (currentHeight >= targetHeight) {
            return false
        }
    }
    return true
}

fun canBeSeen(x: Int, y: Int, grid: Grid): Boolean {
    if (canBeSeenLeft(x, y, grid)) {
        return true
    }
    if (canBeSeenUp(x, y, grid)) {
        return true
    }
    if (canBeSeenRight(x, y, grid)) {
        return true
    }
    if (canBeSeenDown(x, y, grid)) {
        return true
    }
    return false
}
