package nl.deltadak.plep.commands

import java.util.*

/**
 * Provider for undo operations by keeping commands on a stack.
 */
class UndoFacility {

    private val undoStack = ArrayDeque<Command>()

    /**
     * Saves executed command to possibly revert later.
     * @param command to execute
     */
    fun execute(command: Command) {
        command.execute()
        undoStack.push(command)
    }

    /**
     * Undo last done command.
     */
    fun undo() {
        if (!undoStack.isEmpty()) {
            val command = undoStack.pop()
            if (command.isExecuted) {
                command.undo()
            }
        }
    }

}
