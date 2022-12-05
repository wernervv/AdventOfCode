import java.io.File
import java.util.ArrayDeque
import java.util.ArrayList

val fileName = "day_5_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

fun main() {
    val input = readInput()
    val (init, instr) = splitInput(input)
    var stacks = readStartingPositions(init)

    for (instruction in instr) {
        stacks = processInstruction(instruction, stacks)
    }

    for (stack in stacks) {
        if (!stack.isEmpty()) {
            print(stack.peek())
        }
    }
    print("\n")
}

fun splitInput(input: List<String>): List<List<String>> {
    val splitLine = input.indexOf("")
    return listOf(input.subList(0, splitLine), input.subList(splitLine + 1, input.size))
}

fun readStartingPositions(input: List<String>): ArrayList<ArrayDeque<Char>> {
    var numberOfStacks = 9
    var stacks = ArrayList<ArrayDeque<Char>>()

    for (index in 0..(numberOfStacks-1)) {
        var stack: ArrayDeque<Char> = ArrayDeque()
        for (infoLine in input.subList(0, input.size - 1)) {
            val c = infoLine.get(4 * index + 1)
            if (c != ' ') {
                stack.addLast(c)
            }
        }
        stacks.add(stack)
    }

    return stacks
}

fun processInstruction(instruction: String, stacks: ArrayList<ArrayDeque<Char>>): ArrayList<ArrayDeque<Char>> {
    val splitInstruction = instruction.split(" ")
    val amount = splitInstruction[1].toInt()
    val from = splitInstruction[3].toInt() - 1
    val to = splitInstruction[5].toInt() - 1

    var movingChunk = ArrayList<Char>()
    repeat(amount) {
        val moving = stacks.get(from).pop()
        movingChunk.add(moving)
    }

    for (moving in movingChunk.reversed()) {
        stacks.get(to).push(moving)
    }

    return stacks
}
