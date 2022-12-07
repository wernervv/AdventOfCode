import java.io.File
import java.util.ArrayDeque

val fileName = "day_7_input.txt"
val limit = 100000
val totalSpace = 70000000
val neededSpace = 30000000

fun readInput(): List<String> {
    return File(fileName).readLines()
}

class myFile(size: Int, name: String) {
    val size = size
    val name = name
}


class Directory(name: String) {
    val dirName = name

    var subdirectories: List<Directory> = emptyList()
    var files: List<myFile> = emptyList()

    var totalSize = -1
}

class Command(val command: String, val arg: String) {
    var commandOutput: List<String> = emptyList()
}

fun main() {
    val input = readInput()
    val root = traverseFilesystem(input)
    val usedSpace = countDirectorySize(root)
    val unusedSpace = totalSpace - usedSpace
    val toRemove = neededSpace - unusedSpace

    val allDirs = giveAllDirectories(root)
    val result = allDirs.map { countDirectorySize(it) }
                        .filter { it >= toRemove }
                        .min()
    
    println(result)
}

fun parseOneCommand(input: List<String>): Pair<Command, List<String>> {
    val givenCommandL = input.get(0)
    val splitCommandL = givenCommandL.drop(2).split(" ")
    val command = splitCommandL[0]
    var arg = ""
    if (command == "cd") {
        arg = splitCommandL[1]
    }

    var output: List<String> = emptyList()
    var linesParsed = 1

    for (line in input.subList(1, input.size)) {
        if (line.get(0) == '$') {
            break
        }
        output += line
        linesParsed += 1
    }
    val com = Command(command, arg)
    com.commandOutput = output

    val rest = input.subList(linesParsed, input.size)
    return Pair(com, rest)
}

fun traverseFilesystem(input: List<String>): Directory {
    var root = Directory("/")
    var currentDirectory = root
    var previousDirectories = ArrayDeque<Directory>()

    var commandOutput = input
    while (!commandOutput.isEmpty()) {
        val (command, rest) = parseOneCommand(commandOutput)

        when (command.command) {
            "cd" -> {
                val dirName = command.arg
                if (dirName == "..") {
                    currentDirectory = previousDirectories.pop()
                }
                else {
                    previousDirectories.push(currentDirectory)
                val directoryCandidate: Directory? = currentDirectory.subdirectories.find { it.dirName == dirName }
                    currentDirectory = if (directoryCandidate != null) directoryCandidate else root
                }
            }
            "ls" -> {
                for (line in command.commandOutput) {
                    val splitLine = line.split(" ")
                    if (splitLine[0] == "dir") {
                        val newDirName = splitLine[1]
                        currentDirectory.subdirectories += Directory(newDirName)
                    }
                    else {
                        val fName = splitLine[1]
                        val fSize = splitLine[0].toInt()
                        currentDirectory.files += myFile(fSize, fName)
                    }
                }
            }
        }
        commandOutput = rest
    }
    return root
}

fun giveAllDirectories(root: Directory): List<Directory> {
    var directories: List<Directory> = emptyList()
    for (subDir in root.subdirectories) {
        directories += giveAllDirectories(subDir)
    }
    return directories + root
}

fun sizeInThisDirectory(root: Directory): Int {
    var total = 0
    for (file in root.files) {
        total += file.size
    }
    return total
}

fun countDirectorySize(root: Directory): Int {
    var total: Int
    if (root.totalSize != -1) {
        total = root.totalSize
    }
    else {
        total = sizeInThisDirectory(root)
        for (dir in root.subdirectories) {
            if (dir.totalSize != -1) {
                total += dir.totalSize
            }
            else {
                total += countDirectorySize(dir)
            }
        }
    }
    root.totalSize = total

    return total
}