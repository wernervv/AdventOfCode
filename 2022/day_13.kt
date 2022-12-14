import java.io.File

val fileName = "day_13_input.txt"

fun readInput(): List<String> {
    return File(fileName).readLines()
}

class Value(type: String) {
    val type: String = type
    var contents: List<Value> = emptyList()
    var value: Int = -1
}

fun splitAfterList(subPacket: String): Pair<String,String> {
    var currentIndex = 0
    var level = 0

    for (c in subPacket) {
        if (c == '[') {
            level -= 1
        }
        else if (c == ']') {
            level += 1
        }

        currentIndex += 1

        if (level == 0) {
            break
        }
    }

    val listPart = subPacket.substring(0, currentIndex)
    val rest = subPacket.substring(currentIndex)

    return Pair(listPart, rest)
}

fun dropFirstAndLast(input: String): String {
    val length = input.length
    return input.substring(1, length-1)
}

fun parseList(subPacket: String): Pair<Value,String> {
    val (list, rest) = splitAfterList(subPacket)
    var listV = Value("list")

    var remainingListContents = dropFirstAndLast(list)

    while (!remainingListContents.isEmpty()) {

        var remaining = ""
        if (remainingListContents[0] == '[') {
            val (listValue, afterList) = parseList(remainingListContents)
            remaining = afterList

            listV.contents += listValue
        }
        else {
            val integer = remainingListContents.takeWhile({ it -> it.isDigit() }).toInt()
            remaining = remainingListContents.dropWhile({ it -> it.isDigit() })

            val integerValue = Value("int")
            integerValue.value = integer
            listV.contents += integerValue
        }

        if (!remaining.isEmpty() && remaining[0] == ',') {
            remainingListContents = remaining.drop(1)
        }
        else {
            remainingListContents = remaining
        }
    }
    return Pair(listV, rest)
}

fun parsePacket(packet: String): Value {
    val (list, empty) = parseList(packet)
    return list
}

fun convertInteger(integerValue: Value): Value {
    val listValue = Value("list")
    listValue.contents += integerValue

    return listValue
}

fun compareValues(left: Value, right: Value): String {
    if (left.type == "int") {
        if (right.type == "list") {
            return compareValues(convertInteger(left), right)
        }
        else {
            val leftInt = left.value
            val rightInt = right.value
            if (leftInt == rightInt) {
                return "undefined"
            }
            else if (leftInt < rightInt) {
                return "right"
            }
            else {
                return "wrong"
            }
        }
    }
    else {
        if (right.type == "int") {
            return compareValues(left, convertInteger(right))
        }
        else {
            val leftSize = left.contents.size
            val rightSize = right.contents.size

            for (i in 0..minOf(leftSize, rightSize)-1) {
                val resultFromCurrent = compareValues(left.contents[i], right.contents[i])
                if (resultFromCurrent != "undefined") {
                    return resultFromCurrent
                }
            }

            if (leftSize == rightSize) {
                return "undefined"
            }
            else if (leftSize < rightSize) {
                return "right"
            }
            else {
                return "wrong"
            }
        }
    }
}

fun packetOrderIsCorrect(packetOne: String, packetTwo: String): Boolean {
    val valueOne = parsePacket(packetOne)
    val valueTwo = parsePacket(packetTwo)

    if (compareValues(valueOne, valueTwo) == "right") {
        return true
    }
    else {
        return false
    }
}

fun main() {
    val input = ArrayList(readInput())
    input.removeIf({ it.isEmpty() })

    val dividerOne = "[[2]]"
    val dividerTwo = "[[6]]"

    var firstIndex = 1

    val biggerThanFirst: ArrayList<String> = ArrayList()

    for (packet in input) {
        if (packetOrderIsCorrect(packet, dividerOne)) {
            firstIndex += 1
        }
        else {
            biggerThanFirst.add(packet)
        }
    }

    var secondIndex = firstIndex+1

    for (packet in biggerThanFirst) {
        if (packetOrderIsCorrect(packet, dividerTwo)) {
            secondIndex += 1
        }
    }

    println(firstIndex * secondIndex)
}
