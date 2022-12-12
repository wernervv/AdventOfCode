import java.io.File
import java.util.ArrayDeque

val fileName = "day_11_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

class Monkey {
    var items: ArrayDeque<Int> = ArrayDeque()
    var inspection: (Int) -> Int = { it }
    val relief: (Int) -> Int = { it / 3 }
    var test: (Int) -> Boolean = { true }
    var happyCase: Int = 0
    var unHappyCase: Int = 0
    var timeOfInspects = 0
}

fun initializeMonkeys(): Array<Monkey> {
    var monkeys: Array<Monkey> = Array(8) { Monkey() }

    monkeys[0].items = ArrayDeque(listOf(66, 71, 94))
    monkeys[0].inspection = { it * 5 }
    monkeys[0].test = { it % 3 == 0 }
    monkeys[0].happyCase = 7
    monkeys[0].unHappyCase = 4

    monkeys[1].items = ArrayDeque(listOf(70))
    monkeys[1].inspection = { it + 6 }
    monkeys[1].test = { it % 17 == 0 }
    monkeys[1].happyCase = 3
    monkeys[1].unHappyCase = 0

    monkeys[2].items = ArrayDeque(listOf(62, 68, 56, 65, 94, 78))
    monkeys[2].inspection = { it + 5 }
    monkeys[2].test = { it % 2 == 0 }
    monkeys[2].happyCase = 3
    monkeys[2].unHappyCase = 1

    monkeys[3].items = ArrayDeque(listOf(89, 94, 94, 67))
    monkeys[3].inspection = { it + 2 }
    monkeys[3].test = { it % 19 == 0 }
    monkeys[3].happyCase = 7
    monkeys[3].unHappyCase = 0

    monkeys[4].items = ArrayDeque(listOf(71, 61, 73, 65, 98, 98, 63))
    monkeys[4].inspection = { it * 7 }
    monkeys[4].test = { it % 11 == 0 }
    monkeys[4].happyCase = 5
    monkeys[4].unHappyCase = 6

    monkeys[5].items = ArrayDeque(listOf(55, 62, 68, 61, 60))
    monkeys[5].inspection = { it + 7 }
    monkeys[5].test = { it % 5 == 0 }
    monkeys[5].happyCase = 2
    monkeys[5].unHappyCase = 1

    monkeys[6].items = ArrayDeque(listOf(93, 91, 69, 64, 72, 89, 50, 71))
    monkeys[6].inspection = { it + 1 }
    monkeys[6].test = { it % 13 == 0 }
    monkeys[6].happyCase = 5
    monkeys[6].unHappyCase = 2

    monkeys[7].items = ArrayDeque(listOf(76, 50))
    monkeys[7].inspection = { it * it }
    monkeys[7].test = { it % 7 == 0 }
    monkeys[7].happyCase = 4
    monkeys[7].unHappyCase = 6

    return monkeys
}

fun main() {
    val monkeys = initializeMonkeys()

    fun turn(monkey: Monkey) {
        while (!monkey.items.isEmpty()) {
            val item = monkey.items.pop()
            val inspectedItem = monkey.inspection(item)
            monkey.timeOfInspects += 1
            val afterRelief = monkey.relief(inspectedItem)

            val testResult = monkey.test(afterRelief)
            val throwto = if (testResult) monkey.happyCase else monkey.unHappyCase

            monkeys[throwto].items.addLast(afterRelief)
        }
    }

    fun round() {
        for (monkey in monkeys) {
            turn(monkey)
        }
    }

    fun countMonkeyBusiness(): Int {
        val inspects = monkeys.map { it -> it.timeOfInspects }.toIntArray()
        inspects.sortDescending()

        val biggest = inspects[0]
        val secondBiggest = inspects[1]

        return biggest * secondBiggest
    }

    repeat(20) { round() }

    println(countMonkeyBusiness())
}
