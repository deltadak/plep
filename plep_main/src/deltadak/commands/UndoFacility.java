package deltadak.commands;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Provider for undo operations by keeping commands on a stack.
 */
public class UndoFacility {

    private Deque<Command> undoStack = new ArrayDeque<>();

    /**
     * Saves executed command to possibly revert later.
     * @param command to execute
     */
    public void execute(Command command) {
        command.execute();
        undoStack.push(command);
    }

    /**
     * Undo last done command.
     */
    public void undo() {
        if (!undoStack.isEmpty()) {
            Command command = undoStack.pop();
            if (command.isExecuted()) {
                command.undo();
            }
        }
    }

}
