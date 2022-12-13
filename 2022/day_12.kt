import java.io.File
import java.util.ArrayList
import kotlin.math.abs

val fileName = "day_12_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

class Square(elevation: Char, x: Int, y: Int) {
    val elevation = elevation
    var distanceSeen = Int.MAX_VALUE
    val x = x
    val y = y
}

class Grid {
    var startX = 0
    var startY = 0
    var endX = 0
    var endY = 0
    var maxX = 0
    var maxY = 0
    var squares: Array<Array<Square>> = emptyArray()
}

fun initializeGrid(input: List<String>): Grid {
    val rows = input.size
    val cols = input[0].length

    val grid: Array<Array<Square>> = Array(rows) { emptyArray() }
    var startX = 0
    var startY = 0
    var endX = 0
    var endY = 0

    val completeGrid = Grid()

    for (row in 0..rows-1) {
        val mapRow: Array<Square> = Array(cols) { Square('a', 0, 0) }
        for (col in 0..cols-1) {
            val elevation = input[row][col]
            if (elevation == 'S') {
                startX = col
                startY = row
                val startSquare = Square('a', col, row)
                startSquare.distanceSeen = 0
                mapRow[col] = startSquare
            }
            else if (elevation == 'E') {
                endX = col
                endY = row
                mapRow[col] = Square('z', col, row)
            }
            else {
                mapRow[col] = Square(elevation, col, row)
            }
        }
        grid[row] = mapRow
    }
    completeGrid.startX = startX
    completeGrid.startY = startY
    completeGrid.endX = endX
    completeGrid.endY = endY
    completeGrid.maxX = cols - 1
    completeGrid.maxY = rows - 1
    completeGrid.squares = grid

    return completeGrid
}

fun giveAccessibleNeighbors(x: Int, y: Int, grid: Grid): List<Square> {
    var neighbors: List<Square> = emptyList()
    val currentSquare = grid.squares[y][x]
    val currentElevation = currentSquare.elevation

    for (neighborXDiff in -1..1) {
        for (neighborYDiff in -1..1) {
            if (neighborXDiff * neighborYDiff == 0 && abs(neighborXDiff + neighborYDiff) == 1) {
                val candidateX = currentSquare.x + neighborXDiff
                val candidateY = currentSquare.y + neighborYDiff
                if (candidateX >= 0 && candidateX <= grid.maxX && candidateY >= 0 && candidateY <= grid.maxY) {
                    val candidate = grid.squares[candidateY][candidateX]
                    if (candidate.elevation.code - currentElevation.code <= 1) {
                        neighbors += candidate
                    }
                }
            }
        }
    }

    return neighbors
}

fun addSeen(seen: Square, allSeen: ArrayDeque<Square>): ArrayDeque<Square> {
    val distanceToAdd = seen.distanceSeen
    val index = allSeen.indexOfFirst( { it -> it.distanceSeen >= distanceToAdd } )
    if (index == -1) {
        allSeen.add(seen)
    }
    else {
        allSeen.add(index, seen)
    }
    return allSeen
}

fun main() {
    val input = readInput()

    val grid = initializeGrid(input)

    var seenSquares: ArrayDeque<Square> = ArrayDeque()
    seenSquares.add(grid.squares[grid.startY][grid.startX])

    while (seenSquares[0].distanceSeen < grid.squares[grid.endY][grid.endX].distanceSeen) {
        val currentSquare = seenSquares.removeFirst()
        val currentDistance = currentSquare.distanceSeen
        for (neighbor in giveAccessibleNeighbors(currentSquare.x, currentSquare.y, grid)) {
            if (currentDistance + 1 < neighbor.distanceSeen) {
                grid.squares[neighbor.y][neighbor.x].distanceSeen = currentDistance + 1
                seenSquares = addSeen(grid.squares[neighbor.y][neighbor.x], seenSquares)
            }
        }
    }

    println(grid.squares[grid.endY][grid.endX].distanceSeen)
}
