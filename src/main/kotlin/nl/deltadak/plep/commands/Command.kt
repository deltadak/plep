package nl.deltadak.plep.commands

/**
 * Default command, conform the command pattern.
 */
abstract class Command {

    /**
     * make sure not to be executed twice
     */
    var isExecuted: Boolean = false

    /**
     * Execute command
     * @throws IllegalStateException when already executed
     */
    @Throws(IllegalStateException::class)
    fun execute() {
        if (isExecuted) {
            throw IllegalStateException("Cannot execute command twice")
        }
        isExecuted = true
        executionHook()
    }

    /**
     * Requires subclass to implement execute(),
     * but at least does not require them to not forget
     * to call super.execute(), conform Template pattern.
     */
    protected abstract fun executionHook()

    /**
     * Undo
     */
    fun undo() {
        isExecuted = false
        undoHook()
    }

    /**
     * Same reason as with [.executionHook] )}.
     */
    protected abstract fun undoHook()

}