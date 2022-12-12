import java.io.File
import java.util.ArrayDeque

val fileName = "day_11_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

class Monkey {
    var items: ArrayDeque<Long> = ArrayDeque()
    var inspection: (Long) -> Long = { it }
    // val relief: (Int) -> Int = { it / 3 }
    var test: (Long) -> Boolean = { true }
    var happyCase: Int = 0
    var unHappyCase: Int = 0
    var timeOfInspects = 0
}

fun initializeMonkeys(): Array<Monkey> {
    var monkeys: Array<Monkey> = Array(8) { Monkey() }

    monkeys[0].items = ArrayDeque(listOf(66L, 71L, 94L))
    monkeys[0].inspection = { it * 5L }
    monkeys[0].test = { it % 3L == 0L }
    monkeys[0].happyCase = 7
    monkeys[0].unHappyCase = 4

    monkeys[1].items = ArrayDeque(listOf(70L))
    monkeys[1].inspection = { it + 6L }
    monkeys[1].test = { it % 17L == 0L }
    monkeys[1].happyCase = 3
    monkeys[1].unHappyCase = 0

    monkeys[2].items = ArrayDeque(listOf(62L, 68L, 56L, 65L, 94L, 78L))
    monkeys[2].inspection = { it + 5L }
    monkeys[2].test = { it % 2L == 0L }
    monkeys[2].happyCase = 3
    monkeys[2].unHappyCase = 1

    monkeys[3].items = ArrayDeque(listOf(89L, 94L, 94L, 67L))
    monkeys[3].inspection = { it + 2L }
    monkeys[3].test = { it % 19L == 0L }
    monkeys[3].happyCase = 7
    monkeys[3].unHappyCase = 0

    monkeys[4].items = ArrayDeque(listOf(71L, 61L, 73L, 65L, 98L, 98L, 63L))
    monkeys[4].inspection = { it * 7L }
    monkeys[4].test = { it % 11L == 0L }
    monkeys[4].happyCase = 5
    monkeys[4].unHappyCase = 6

    monkeys[5].items = ArrayDeque(listOf(55L, 62L, 68L, 61L, 60L))
    monkeys[5].inspection = { it + 7L }
    monkeys[5].test = { it % 5L == 0L }
    monkeys[5].happyCase = 2
    monkeys[5].unHappyCase = 1

    monkeys[6].items = ArrayDeque(listOf(93L, 91L, 69L, 64L, 72L, 89L, 50L, 71L))
    monkeys[6].inspection = { it + 1L }
    monkeys[6].test = { it % 13L == 0L }
    monkeys[6].happyCase = 5
    monkeys[6].unHappyCase = 2

    monkeys[7].items = ArrayDeque(listOf(76L, 50L))
    monkeys[7].inspection = { it * it }
    monkeys[7].test = { it % 7L == 0L }
    monkeys[7].happyCase = 4
    monkeys[7].unHappyCase = 6

    return monkeys
}

fun main() {
    val monkeys = initializeMonkeys()

    val modulosCombined = 3L * 17L * 2L * 19L * 11L * 5L * 13L * 7L

    fun turn(monkey: Monkey) {
        while (!monkey.items.isEmpty()) {
            val item = monkey.items.pop()
            val inspectedItem = monkey.inspection(item)
            monkey.timeOfInspects += 1
            // val afterRelief = monkey.relief(inspectedItem)

            val reducedInspected = inspectedItem % modulosCombined

            val testResult = monkey.test(reducedInspected)
            val throwto = if (testResult) monkey.happyCase else monkey.unHappyCase

            monkeys[throwto].items.addLast(reducedInspected)
        }
    }

    fun round() {
        for (monkey in monkeys) {
            turn(monkey)
        }
    }

    fun countMonkeyBusiness(): Long {
        val inspects = monkeys.map { it -> it.timeOfInspects }.toIntArray()
        inspects.sortDescending()

        val biggest = inspects[0].toLong()
        val secondBiggest = inspects[1].toLong()

        return biggest * secondBiggest
    }

    repeat(10000) { round() }

    println(countMonkeyBusiness())
}
